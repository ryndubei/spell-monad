{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
module App.Thread
  ( AppThread
  , withAppThread
  , appThreadVty
  , BrickThread
  , withBrickThread
  , sendBrickEvent
  , isBrickQueueEmpty
  , waitBrickThread
  , mapBrickResult
  , takeBrickThread
  , SFThread
  , sendSFThread
  , takeSFThread
  , withSFThread
  , waitSFThread
  , flushSFThreadEvents
  , mapDiscreteOutput
  , ReplThread
  , InterpretRequest(..)
  , ReplStatus(..)
  , withReplThread
  , submitRepl
  , replStatus
  , getReplResult
  , interruptRepl
  ) where

import Graphics.Vty (defaultConfig, Vty(..))
import Graphics.Vty.CrossPlatform
import Control.Exception
import Brick.BChan
import Control.Concurrent.Async
import Brick
import Control.Monad.IO.Class
import Control.Monad
import Control.Concurrent.STM.TQueue
import Data.Profunctor
import Control.Concurrent.STM
import Data.Bifunctor
import FRP.Yampa
import Data.Time
import Control.Concurrent
import Data.Maybe
import Graphics.Vty.Output
import Spell
import Type.Reflection (Typeable)
import Data.Text (Text)
import Language.Haskell.Interpreter
import Data.Char
import Data.List (stripPrefix)
import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Void
import qualified Data.Text as T

-- | Initialised terminal resources.
data AppThread = AppThread
  { vty :: !(TVar Vty)
  , rebuildVty :: !(IO Vty)
  }

appThreadVty :: AppThread -> IO Vty
appThreadVty = readTVarIO . vty

withAppThread :: (AppThread -> IO a) -> IO a
withAppThread f = do
  bracket
    do
      vty1 <- ourMkVty
      vty <- newTVarIO vty1

      let rebuildVty = do
            v <- ourMkVty
            -- update TVar with whichever is currently in use: useful in case
            -- of a Brick thread crash
            atomically $ writeTVar vty v
            pure v

      pure AppThread {..}
    (\AppThread{..} -> readTVarIO vty >>= shutdown)
    f
  where
    ourMkVty = do
      v <- mkVty defaultConfig

      -- Enable bracketed paste
      let output = outputIface v
      when (supportsMode output BracketedPaste) $
        setMode output BracketedPaste True
      -- Enable mouse click reporting
      when (supportsMode output Mouse) $
        setMode output Mouse True

      pure v

-- | Minimal BChan size since we assume events will be pushed at exactly the
-- rate Brick can handle them or less (e.g. the events are game states to be
-- drawn)
brickBChanSize :: Int
brickBChanSize = 1

-- | AppThread running a particular Brick application
data BrickThread r e o = forall e1 o1. BrickThread
  { appThread :: AppThread
  , brickBChan :: !(BChan e1)
  , brickAsync :: !(Async (r, Vty))
  , eventTQueue :: !(TQueue e1)
  , outputTQueue :: !(TQueue o1)
  , eventMapper :: e -> e1
  , outputMapper :: o1 -> o
  }

deriving instance Functor (BrickThread r e)

instance Profunctor (BrickThread r) where
  lmap f BrickThread{..} =
    let eventMapper' = eventMapper . f
     in BrickThread{eventMapper = eventMapper', ..}
  rmap = fmap

mapBrickResult :: (r -> r') -> BrickThread r e o -> BrickThread r' e o
mapBrickResult f b@BrickThread{brickAsync} = b { brickAsync = fmap (Data.Bifunctor.first f) brickAsync }

-- | Runs the passed Brick application on a separate thread, using resources initialised by
-- AppThread. Does not guarantee that BrickThread will remain active for the entire continuation:
-- use 'waitBrickThread' with 'race' to exit immediately when BrickThread terminates.
withBrickThread :: Ord n => AppThread -> (TQueue o -> App s e n) -> s -> (BrickThread s e o -> IO a) -> IO a
withBrickThread appThread theapp initialState k = do
  brickBChan <- liftIO $ newBChan brickBChanSize
  vty' <- liftIO $ readTVarIO (vty appThread)
  eventTQueue <- liftIO newTQueueIO
  outputTQueue <- newTQueueIO
  withAsync
    (customMainWithVty vty' (rebuildVty appThread) (Just brickBChan) (theapp outputTQueue) initialState)
    \brickAsync -> do
      withAsync
        do
          Control.Monad.forever do
            e <- atomically $ readTQueue eventTQueue
            writeBChan brickBChan e
        \_ -> k BrickThread{..}
  where
    eventMapper = id
    outputMapper = id

-- | This function does not retry, and BrickThread can queue an unbounded number
-- of events 'e'. This may seem to defeat the point of Brick's BChan. To avoid a
-- memory leak, sendBrickEvent should only be called when 'isBrickQueueEmpty' is True.
sendBrickEvent :: BrickThread r e o -> e -> STM ()
sendBrickEvent BrickThread{..} = writeTQueue eventTQueue . eventMapper

isBrickQueueEmpty :: BrickThread r e o -> STM Bool
isBrickQueueEmpty BrickThread{eventTQueue} = isEmptyTQueue eventTQueue

-- | Retries until BrickThread exits.
waitBrickThread :: BrickThread r e o -> STM (Either SomeException r)
waitBrickThread BrickThread{brickAsync} = Data.Bifunctor.second fst <$> waitCatchSTM brickAsync

takeBrickThread :: BrickThread r e o -> STM o
takeBrickThread BrickThread{outputTQueue, outputMapper} = outputMapper <$> readTQueue outputTQueue

maxDelay :: Num a => a
maxDelay = 30 * 10^(3 :: Int) -- 30ms in microseconds

-- | Signal function thread with input 'u', continuous output 's' and discrete
-- output 'e'.
data SFThread e u s = forall s' u' e'. Semigroup u' => SFThread
  { sfAsync :: !(Async ())
  , lastOutput :: !(TMVar s')
  , userInputs :: !(TMVar u')
  , rmapperSFThread :: s' -> s
  , lmapperSFThread :: u -> u'
  , paused :: !(TMVar UTCTime)
  , lastTime :: !(TVar UTCTime)
  , sfEvents :: TQueue e'
  , eventsMapperSFThread :: e' -> e
  }

deriving instance Functor (SFThread e u)

instance Profunctor (SFThread e) where
  rmap = fmap
  lmap f SFThread{..} =
    let lmapperGameThread' = lmapperSFThread . f
     in SFThread{lmapperSFThread = lmapperGameThread', ..}

mapDiscreteOutput :: (e -> e') -> SFThread e u s -> SFThread e' u s
mapDiscreteOutput f SFThread{..} = SFThread{eventsMapperSFThread = f . eventsMapperSFThread, ..}

-- | Non-retrying, sends input 'u' to the SFThread.
sendSFThread :: SFThread e u s -> u -> STM ()
sendSFThread SFThread{userInputs, lmapperSFThread} =
  (\u -> tryTakeTMVar userInputs >>= maybe (writeTMVar userInputs u) (writeTMVar userInputs . (u <>))) . lmapperSFThread

-- | Retries until an output 's' is available from SFThread, then
-- removes and returns 's'.
takeSFThread :: SFThread e u s -> STM s
takeSFThread SFThread{lastOutput, rmapperSFThread} =
  rmapperSFThread <$> takeTMVar lastOutput

-- | Retries until the SFThread exits.
waitSFThread :: SFThread e u s -> STM (Maybe SomeException)
waitSFThread SFThread{..} =
  either Just (const Nothing) <$> waitCatchSTM sfAsync

-- | Get all events in the SFThread's queue. Never retries.
flushSFThreadEvents :: SFThread e u s -> STM [e]
flushSFThreadEvents SFThread{..} = map eventsMapperSFThread <$> flushTQueue sfEvents

-- | Run the passed signal function as an SFThread. Just like BrickThread, does
-- not guarantee that the SFThread will be running for the entire continuation.
withSFThread :: Semigroup u => SF (Event u) (s, Event e) -> (SFThread e u s -> IO a) -> IO a
withSFThread sf k = do
  userInputs <- newEmptyTMVarIO
  lastOutput <- newEmptyTMVarIO
  paused <- newEmptyTMVarIO
  lastTime <- getCurrentTime >>= newTVarIO
  sfEvents <- newTQueueIO

  let
    sfThread = do
      getCurrentTime >>= atomically . writeTVar lastTime
      reactimate
        (pure NoEvent)
        sense
        actuate
        sf

    sense block = do
      -- User input event, if given
      e <- do
        us <- atomically (tryTakeTMVar userInputs)

        if isNothing us && block
          then do
            withAsync (threadDelay maxDelay) $ \a -> do
              atomically do
                result <- newTVar NoEvent
                orElse
                  -- if timed out, exit without modifying the queue,
                  -- otherwise proceed to reading the queue
                  (waitSTM a)
                  -- try reading from the queue, retry if empty
                  do
                    us' <- takeTMVar userInputs
                    writeTVar result (Event us')
                readTVar result
          else pure $ maybeToEvent us

      t' <- getCurrentTime
      dt <- atomically do
        dt' <- realToFrac . diffUTCTime t' <$> readTVar lastTime
        writeTVar lastTime t'
        pure dt'
      pure (dt, Just e)

    actuate _ (s, e) = do
      atomically $ writeTMVar lastOutput s
      event (pure ()) (atomically . writeTQueue sfEvents) e
      pure False

  withAsync
    sfThread
    \sfAsync -> k SFThread{..}
  where
    rmapperSFThread = id
    lmapperSFThread = id
    eventsMapperSFThread = id

-- | A request to carry out some side effects in the game.
data InterpretRequest =
  forall a. Typeable a => InterpretRequest
    { submitResponseHere :: TMVar (a -> STM ())
      -- ^ Exceptions in 'a' are allowed.
      -- Becomes empty if no longer needed (e.g. due to an interrupt):
      -- in that case should stop the Spell computation ASAP.
      -- (TODO: Becomes an AsyncException if a response is no longer needed?)
    , toInterpret :: Spell a
    }

data ReplThread = ReplThread
  { toCompile :: !(TMVar String)
  -- , spellOutputQueue :: !(TQueue Text)
  , replResult :: !(TMVar (Maybe Text))
      -- ^ ReplThread should not be unblocked until this is emptied
      -- or it is interrupted
  , interpretRequest :: !(TMVar InterpretRequest)
  , replThreadAsync :: !(Async InterpreterError)
  , replInterrupt :: !(TVar Bool)
      -- ^ Tells the ReplThread that it must interrupt everything it is currently
      -- blocked on and accept the next input in 'toCompile'.

  , replInitialised :: !(TVar Bool)
      -- ^ Should have a loading screen preventing the user from sending
      -- anything to the REPL until it is initialised here, but this is not
      -- strictly necessary.

  , replBlocked :: !(TVar Bool)
      -- ^ Should not send any input until this is False.
  }

withReplThread :: (ReplThread -> IO a) -> IO a
withReplThread k = do
  toCompile <- newEmptyTMVarIO
  replResult <- newEmptyTMVarIO
  interpretRequest <- newEmptyTMVarIO
  replInterrupt <- newTVarIO False
  replInitialised <- newTVarIO False
  replBlocked <- newTVarIO False

  withAsync
    do
      e <- runInterpreter $ do
        setImports ["Prelude.Spell"]
        liftIO . atomically $ writeTVar replInitialised True
        forever do

          s <- liftIO $ atomically do
            -- An interrupt will always clear the pending input atomically, so
            -- even if there is an interrupt already, then there is nothing to do.
            writeTVar replInterrupt False
            -- no need to write to replBlocked because that is already done by
            -- submitRepl
            readTMVar toCompile
          t <- typeChecksWithDetails s
          case t of
            Left errs -> liftIO $ atomically do
              b <- readTVar replInterrupt
              if b
                then writeTVar replInterrupt False
                else do
                  writeTMVar replResult ((\tx -> if T.null tx then Nothing else Just tx) . T.unlines $ map (T.pack . errMsg) errs) 
                  writeTVar replBlocked False
            Right _ -> undefined
      pure $ either id absurd e
    \replThreadAsync -> k ReplThread{..}

-- | Retries until the ReplThread can accept more input.
submitRepl :: ReplThread -> String -> STM ()
submitRepl ReplThread{replResult, replBlocked, toCompile} s = do
  b <- readTVar replBlocked
  check (not b)
  _ <- takeTMVar replResult
    -- submitting something new for compilation should invalidate whatever was
    -- in the result, if the REPL is no longer blocked
  writeTMVar toCompile s
  writeTVar replBlocked True

data ReplStatus = Initialising | Initialised | Dead (Either SomeException InterpreterError)

replStatus :: ReplThread -> STM ReplStatus
replStatus ReplThread{replThreadAsync, replInitialised} = do
  asyncResult <- pollSTM replThreadAsync
  case asyncResult of
    Nothing -> do
      a <- readTVar replInitialised
      pure $ if a then Initialised else Initialising
    Just (Right e) -> pure (Dead (Right e))
    Just (Left e) -> pure (Dead (Left e))

-- | Acknowledge the ReplThread's result, allowing it to accept more inputs.
-- Retries until there is a result.
--
-- A result is not guaranteed to come in if a separate thread is calling
-- both 'submitRepl' and 'interruptRepl': the repl may be interrupted, but
-- 'getReplResult' may not be woken up until the next 'submitRepl', which
-- empties 'replResult'.
getReplResult :: ReplThread -> STM (Maybe Text)
getReplResult ReplThread{replBlocked, replResult} = do
  writeTVar replBlocked False
  takeTMVar replResult

-- | Immediately interrupt whatever is currently blocking the REPL, allowing it
-- to accept more inputs. Never retries.
interruptRepl :: ReplThread -> STM ()
interruptRepl ReplThread{toCompile, replResult, replInterrupt, replBlocked} = do
  _ <- tryTakeTMVar toCompile
  _ <- tryPutTMVar replResult Nothing -- ^ in case something is blocked on getReplResult
  writeTVar replBlocked False
  writeTVar replInterrupt True
  pure ()
