{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ApplicativeDo #-}
module App.Thread
  ( AppThread
  , withAppThread
  , BrickThread
  , withBrickThread
  , sendBrickEvent
  , isBrickQueueEmpty
  , waitBrickThread
  , SFThread
  , sendSFThread
  , takeSFThread
  , withSFThread
  , waitSFThread
  , flushSFThreadLogs
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
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Foldable
import Data.Text (Text)
import Data.Maybe

-- | Initialised terminal resources.
data AppThread = AppThread
  { vty :: !(TVar Vty)
  , rebuildVty :: !(IO Vty)
  }

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

      -- TODO: possibly make this a settings option
      -- Enable bracketed paste
      -- let output = outputIface v
      -- when (supportsMode output BracketedPaste) $
      --   setMode output BracketedPaste True

      pure v

-- | Minimal BChan size since we assume events will be pushed at exactly the
-- rate Brick can handle them or less (e.g. the events are game states to be
-- drawn)
brickBChanSize :: Int
brickBChanSize = 1

-- | AppThread running a particular Brick application
data BrickThread e s = forall e1. BrickThread
  { appThread :: AppThread
  , brickBChan :: !(BChan e1)
  , brickAsync :: !(Async (s, Vty))
  , eventTQueue :: !(TQueue e1)
  , eventMapper :: e -> e1
  }

deriving instance Functor (BrickThread e)

instance Profunctor BrickThread where
  lmap f BrickThread{..} =
    let eventMapper' = eventMapper . f
     in BrickThread{eventMapper = eventMapper', ..}
  rmap = fmap

-- | Runs the passed Brick application on a separate thread, using resources initialised by
-- AppThread. Does not guarantee that BrickThread will remain active for the entire continuation:
-- use 'waitBrickThread' with 'race' to exit immediately when BrickThread terminates.
withBrickThread :: Ord n => AppThread -> App s e n -> s -> (BrickThread e s -> IO a) -> IO a
withBrickThread appThread theapp initialState k = do
  brickBChan <- liftIO $ newBChan brickBChanSize
  vty' <- liftIO $ readTVarIO (vty appThread)
  eventTQueue <- liftIO newTQueueIO
  withAsync
    (customMainWithVty vty' (rebuildVty appThread) (Just brickBChan) theapp initialState)
    \brickAsync -> do
      withAsync
        do
          Control.Monad.forever do
            e <- atomically $ readTQueue eventTQueue
            writeBChan brickBChan e
        \_ -> k BrickThread{..}
  where
    eventMapper = id

-- | This function does not retry, and BrickThread can queue an unbounded number
-- of events 'e'. This may seem to defeat the point of Brick's BChan. To avoid a
-- memory leak, sendBrickEvent should only be called when 'isBrickQueueEmpty' is True.
sendBrickEvent :: BrickThread e s -> e -> STM ()
sendBrickEvent BrickThread{..} = writeTQueue eventTQueue . eventMapper

isBrickQueueEmpty :: BrickThread e s -> STM Bool
isBrickQueueEmpty BrickThread{eventTQueue} = isEmptyTQueue eventTQueue

-- | Retries until BrickThread exits.
waitBrickThread :: BrickThread e s -> STM (Either SomeException s)
waitBrickThread BrickThread{brickAsync} = Data.Bifunctor.second fst <$> waitCatchSTM brickAsync

maxDelay :: Num a => a
maxDelay = 30 * 10^(3 :: Int) -- 30ms in microseconds

-- | Signal function thread with input 'u' and discardable output 's'.
-- Only the last output is stored.
data SFThread u s = forall s' u'. SFThread
  { sfAsync :: !(Async ())
  , lastOutput :: !(TMVar s')
  , userInputs :: !(TQueue u')
  , rmapperSFThread :: s' -> s
  , lmapperSFThread :: u -> u'
  , paused :: !(TMVar UTCTime)
  , lastTime :: !(TVar UTCTime)
  , logs :: TQueue Text
  }

deriving instance Functor (SFThread u)

instance Profunctor SFThread where
  rmap = fmap
  lmap f SFThread{..} =
    let lmapperGameThread' = lmapperSFThread . f
     in SFThread{lmapperSFThread = lmapperGameThread', ..}

-- | Non-retrying, sends input 'u' to the SFThread's queue.
sendSFThread :: SFThread u s -> u -> STM ()
sendSFThread SFThread{userInputs, lmapperSFThread} =
  writeTQueue userInputs . lmapperSFThread

-- | Retries until an output 's' is available from SFThread, then
-- removes and returns 's'.
takeSFThread :: SFThread u s -> STM s
takeSFThread SFThread{lastOutput, rmapperSFThread} =
  rmapperSFThread <$> takeTMVar lastOutput

-- | Retries until the SFThread exits.
waitSFThread :: SFThread u s -> STM (Maybe SomeException)
waitSFThread SFThread{..} =
  either Just (const Nothing) <$> waitCatchSTM sfAsync 

-- | Get all logs sent by the SFThread. Never retries.
flushSFThreadLogs :: SFThread u s -> STM [Text] 
flushSFThreadLogs SFThread{..} = flushTQueue logs

-- | Run the passed signal function as an SFThread. Just like BrickThread, does
-- not guarantee that the SFThread will be running for the entire continuation.
withSFThread :: Foldable f => TQueue u -> SF (Event (NonEmpty u)) (s, f Text) -> (SFThread u s -> IO a) -> IO a
withSFThread userInputs sf k = do
  lastOutput <- newEmptyTMVarIO
  paused <- newEmptyTMVarIO
  lastTime <- getCurrentTime >>= newTVarIO 
  logs <- newTQueueIO

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
        us <- NE.nonEmpty <$> atomically (flushTQueue userInputs)

        if isNothing us && block
          then do
            timeout <- newTVarIO False
            withAsync (threadDelay maxDelay >> atomically (writeTVar timeout True)) $ \a -> do
              link a
              atomically do
                result <- newTVar NoEvent
                orElse
                  -- if timed out, exit without modifying the queue,
                  -- otherwise proceed to reading the queue
                  (readTVar timeout >>= check)
                  -- try reading from the queue, retry if empty
                  do
                    us' <- (NE.:|) <$> readTQueue userInputs <*> flushTQueue userInputs
                    writeTVar result (Event us')
                readTVar result
          else pure $ maybeToEvent us

      t' <- getCurrentTime
      dt <- atomically do
        dt' <- realToFrac . diffUTCTime t' <$> readTVar lastTime
        writeTVar lastTime t'
        pure dt'
      pure (dt, Just e)

    actuate _ (s, newLogs) = do
      atomically $ writeTMVar lastOutput s
      traverse_ (atomically . writeTQueue logs) newLogs 
      pure False

  withAsync
    sfThread
    \sfAsync -> k SFThread{..}
  where
    rmapperSFThread = id
    lmapperSFThread = id

