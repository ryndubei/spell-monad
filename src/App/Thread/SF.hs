{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module App.Thread.SF
  ( SFThread
  , sendSFThread
  , takeSFThread
  , withSFThread
  , waitSFThread
  , flushSFThreadEvents
  , mapDiscreteOutput
  
  -- * Helpers for signal functions
  , morphSF
  , generaliseSF
  ) where

import FRP.BearRiver
import Control.Concurrent.STM
import Control.Concurrent.Async
import Data.Time
import Control.Exception
import Data.Profunctor
import Control.Concurrent
import Data.Maybe
import Control.Monad.IO.Class
import Control.Monad.Trans.MSF
import Data.Functor.Identity

generaliseSF :: Monad m => SF Identity a b -> SF m a b
generaliseSF = morphSF (pure . runIdentity)

-- | Move between monads in an SF, while keeping time information.
morphSF :: (Monad m2, Monad m1) => (forall c. m1 c -> m2 c) -> SF m1 a b -> SF m2 a b
-- I think this would be as safe as morphS, but who knows
morphSF nat = morphS (mapReaderT nat) 

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
  -- Note: this does not force the value stored by 'userInputs'.
  -- This is a deliberate choice: it guarantees that the transaction is fast
  -- (just like a queue), while simplifying withSFThread's interface
  -- (otherwise it has to take 'Event (NonEmpty u)' instead of just 'Event u')
  (\u -> tryTakeTMVar userInputs >>= maybe (writeTMVar userInputs u) (writeTMVar userInputs . (<> u))) . lmapperSFThread

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
--
-- Does not require a MonadUnliftIO, because the natural transformation needn't
-- preserve state (it could be e.g. evaluating a StateT)
withSFThread :: (MonadIO m, Semigroup u) => (forall x. m x -> IO x) -> SF m (Event u) (s, Event e) -> (SFThread e u s -> IO a) -> IO a
withSFThread tf sf k = do
  userInputs <- newEmptyTMVarIO
  lastOutput <- newEmptyTMVarIO
  paused <- newEmptyTMVarIO
  lastTime <- getCurrentTime >>= newTVarIO
  sfEvents <- newTQueueIO

  let
    sfThread = do
      getCurrentTime >>= atomically . writeTVar lastTime
      tf $ reactimate
        (pure NoEvent)
        sense
        actuate
        sf

    sense block = do
      -- User input event, if given
      e <- do
        us <- liftIO $ atomically (tryTakeTMVar userInputs)

        if isNothing us && block
          then do
            liftIO . withAsync (threadDelay maxDelay) $ \a -> do
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

      t' <- liftIO getCurrentTime
      dt <- liftIO $ atomically do
        dt' <- realToFrac . diffUTCTime t' <$> readTVar lastTime
        writeTVar lastTime t'
        pure dt'
      pure (dt, Just e)

    -- Since previous states depend on future states, laziness gives us nothing,
    -- and only offloads the computation to the thread viewing SFThread.
    -- (which probably harms performance if we run with +RTS -N)
    actuate _ (!s, !e) = do
      liftIO . atomically $ writeTMVar lastOutput s
      event (pure ()) (liftIO . atomically . writeTQueue sfEvents) e
      pure False

  withAsync
    sfThread
    \sfAsync -> k SFThread{..}
  where
    rmapperSFThread = id
    lmapperSFThread = id
    eventsMapperSFThread = id
