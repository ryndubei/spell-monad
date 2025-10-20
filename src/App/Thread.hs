{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ApplicativeDo #-}
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
  , dupBrickThread
  , SFThread
  , sendSFThread
  , takeSFThread
  , withSFThread
  , waitSFThread
  , flushSFThreadEvents
  , mapDiscreteOutput
  , module App.Thread.Repl
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
import App.Thread.Repl
import Data.Foldable

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
  , outputTChan :: !(TChan o1)
  , eventMapper :: e -> e1
  , outputMapper :: o1 -> o
  
  , queueWriting :: !(TVar Bool)
    -- ^ eventTQueue may be flushed, but should not be treated as emptied unless
    -- this is also False, since it may still be writing its contents to brickBChan

  -- For duplicating the input stream. emptiedGlobal is incremented whenever
  -- eventTQueue is emptied into brickBChan, meaning the BrickThread can accept
  -- more output. emptiedLocal is set to the value of emptiedGlobal whenever
  -- an output is pushed to eventTQueue.
  -- emptiedGlobal /= emptiedLocal only if the queue has been emptied at least
  -- once since the input was last sent into _this_ duplicate of BrickThread.
  -- Therefore as long as there are a finite number of duplicates, the size of
  -- eventTQueue will always be bounded.  
  -- This is almost always an if-and-only-if, unless emptiedGlobal has overflown
  -- to the exact value of emptiedLocal (unlikely). In that case, a thread may
  -- miss a chance to write an output to BrickThread when it could have.
  -- In the case that this somehow happens, an extra check on the actual
  -- state of the queue could be used to ensure that not _every_ writer thread
  -- blocks unnecessarily, so we don't deadlock.
  , emptiedGlobal :: !(TVar Int)
  , emptiedLocal :: !(TVar Int)
  -- TODO: While this edge case is very unlikely to ever happen, maybe there is
  -- a way to do this with no edge cases at all.
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
withBrickThread :: Ord n => AppThread -> (TChan o -> App s e n) -> s -> (BrickThread s e o -> IO a) -> IO a
withBrickThread appThread theapp initialState k = do
  brickBChan <- liftIO $ newBChan brickBChanSize
  vty' <- liftIO $ readTVarIO (vty appThread)
  eventTQueue <- liftIO newTQueueIO
  outputTChan <- newTChanIO
  emptiedGlobal <- newTVarIO 1
  emptiedLocal <- newTVarIO 0
  queueWriting <- newTVarIO False
  withAsync
    (customMainWithVty vty' (rebuildVty appThread) (Just brickBChan) (theapp outputTChan) initialState)
    \brickAsync -> do
      withAsync
        do
          Control.Monad.forever do
            es <- atomically do
              es <- flushTQueue eventTQueue
              check (not $ null es)
              writeTVar queueWriting True
              pure es
            traverse_ (writeBChan brickBChan) es
            atomically do
              writeTVar queueWriting False
              modifyTVar' emptiedGlobal (+ 1) 
        \_ -> k BrickThread{..}
  where
    eventMapper = id
    outputMapper = id

-- | This function does not retry, and BrickThread can queue an unbounded number
-- of events 'e'. This may seem to defeat the point of Brick's BChan. To avoid a
-- memory leak, sendBrickEvent should only be called when 'isBrickQueueEmpty' is True.
sendBrickEvent :: BrickThread r e o -> e -> STM ()
sendBrickEvent BrickThread{..} e = do
  writeTQueue eventTQueue (eventMapper e)
  readTVar emptiedGlobal >>= writeTVar emptiedLocal

-- | Does not actually mean the event queue is empty, just "essentially empty"
-- in this thread. As long as `sendBrickEvent` is only called when `isBrickQueueEmpty`
-- is True, the size of the BrickThread's event queue will be bounded in proportion to
-- the number of writer threads.
isBrickQueueEmpty :: BrickThread r e o -> STM Bool
isBrickQueueEmpty BrickThread{eventTQueue, queueWriting, emptiedGlobal, emptiedLocal} = asum
  [ do -- the queue is empty, and its contents are not being currently written
      isEmptyTQueue eventTQueue >>= check
      readTVar queueWriting >>= check . not
      pure True
  , do -- the queue was emptied since we last wrote anything to it
      g <- readTVar emptiedGlobal
      l <- readTVar emptiedLocal
      pure (g /= l)
  ]

-- | Retries until BrickThread exits.
waitBrickThread :: BrickThread r e o -> STM (Either SomeException r)
waitBrickThread BrickThread{brickAsync} = Data.Bifunctor.second fst <$> waitCatchSTM brickAsync

takeBrickThread :: BrickThread r e o -> STM o
takeBrickThread BrickThread{outputTChan, outputMapper} = outputMapper <$> readTChan outputTChan

-- | Duplicate the BrickThread's output and input streams, allowing it to be
-- used independently from a different thread.
--
-- > (dupBrickThread bth >>= takeBrickThread) = retry
--
-- > (dupBrickThread bth >>= isBrickQueueEmpty) = isBrickQueueEmpty bth
dupBrickThread :: BrickThread r e o -> STM (BrickThread r e o)
dupBrickThread BrickThread{..} = do
  o <- dupTChan outputTChan
  l <- readTVar emptiedLocal >>= newTVar
  pure BrickThread{outputTChan = o, emptiedLocal = l, ..}

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
