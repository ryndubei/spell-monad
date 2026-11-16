{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
module App.Thread.UI
  ( UIThread
  , mapUIResult
  , sendUIEvent
  , isUIQueueEmpty
  , withUIThread
  , waitUIThread
  , takeUIThread
  , dupUIThreadIn
  , dupUIThreadOut
  ) where

import Control.Concurrent.STM
import Control.Concurrent.Async
import Data.Profunctor
import Control.Applicative
import Control.Exception
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

data UIThread r e o = forall e1 o1. UIThread
  { eventTQueue :: TQueue e1
  , outputTChan :: TChan o1
  , eventMapper :: e -> e1
  , outputMapper :: o1 -> o
  , queueWaitingForInput :: TVar Bool
  , emptiedGlobal :: TVar Int
  -- ^ emptiedGlobal is incremented whenever eventTQueue is emptied
  , emptiedLocal :: TVar Int
  -- ^ emptiedLocal should be set to the value of emptiedLocal whenever some
  -- 'bounded amount' of input is pushed to the eventTQueue.
  --
  -- no input should be pushed to UIThread when emptiedLocal is the same as
  -- emptiedGlobal (enforced by user via isUIQueueEmpty)
  --
  -- UIThread can be duplicated over input (i.e. both instances have independent
  -- rate-limiting) by creating a new TVar for emptiedLocal
  --
  -- Behaviour in case of overflow is undefined. But in practice it would still
  -- work in almost all cases even if the Ints are tiny.
  , uiThreadAsync :: Async r
  }

deriving instance Functor (UIThread r e)

instance Profunctor (UIThread r) where
  lmap f UIThread{..} =
    let eventMapper' = eventMapper . f
     in UIThread{eventMapper = eventMapper', ..}
  rmap = fmap

mapUIResult :: (r -> r') -> UIThread r e o -> UIThread r' e o
mapUIResult f u@UIThread{uiThreadAsync} = u { uiThreadAsync = fmap f uiThreadAsync }

-- | This function does not retry, and UIThread can queue an unbounded number
-- of events 'e'. To avoid a memory leak, sendUIEvent should only be called
-- when 'isBrickQueueEmpty' is True.
sendUIEvent :: UIThread r e o -> e -> STM ()
sendUIEvent = undefined

-- | Build a UIThread by handling a blocking operation to accept input events to
-- the UI (i.e: game output) and a non-blocking operation to send output events
-- from the UI (i.e: user input).
withUIThread :: (IO (NonEmpty e) -> (o -> IO ()) -> IO r) -> (UIThread r e o -> IO a) -> IO a
withUIThread th k = do
  eventTQueue <- newTQueueIO
  outputTChan <- newTChanIO
  emptiedGlobal <- newTVarIO 0
  emptiedLocal <- newTVarIO 0
  queueWaitingForInput <- newTVarIO False
  let eventMapper = id
      outputMapper = id
  withAsync
    do
      th
        do
          atomically $ writeTVar queueWaitingForInput True
          atomically do
            es <- flushTQueue eventTQueue
            case NE.nonEmpty es of
              Just es' -> do
                writeTVar queueWaitingForInput False
                modifyTVar' emptiedGlobal (+ 1)
                pure es'
              Nothing -> retry
        \o -> atomically $ writeTChan outputTChan o
    \uiThreadAsync -> k UIThread{..}

-- | Does not actually mean the event queue is empty, just "essentially empty"
-- in this thread.
--
-- As long as `sendUIEvent` is only called when `isUIQueueEmpty` is True, the
-- size of the UIThread's event queue will be bounded in proportion to the
-- number of writer threads.
isUIQueueEmpty :: UIThread r e o -> STM Bool
isUIQueueEmpty UIThread{eventTQueue, emptiedGlobal, emptiedLocal, queueWaitingForInput} = asum
  [ do -- the queue is actually empty, and UIThread is waiting for more input
      isEmptyTQueue eventTQueue >>= check
      readTVar queueWaitingForInput >>= check
      pure True
  , do -- the queue was emptied since we last wrote anything to it
      g <- readTVar emptiedGlobal
      l <- readTVar emptiedLocal
      pure (g /= l)
  ]

-- | Retries until the UIThread exits.
waitUIThread :: UIThread r e o -> STM (Either SomeException r)
waitUIThread = waitCatchSTM . uiThreadAsync

-- | Retries until the UIThread has an output.
takeUIThread :: UIThread r e o -> STM o
takeUIThread UIThread{outputMapper, outputTChan} = outputMapper <$> readTChan outputTChan

-- | Duplicate the UIThread's input stream, allowing it to be pushed to
-- independently from a different thread.
--
-- > (atomically (dupUIThreadIn uith) >>= atomically . isUIQueueEmpty) = atomically (isUIQueueEmpty uith)
dupUIThreadIn :: UIThread r e o -> STM (UIThread r e o)
dupUIThreadIn UIThread{..} = do
  l <- readTVar emptiedLocal >>= newTVar
  pure UIThread{emptiedLocal = l, ..}

-- | Duplicate the UIThread's output stream, allowing it to be consumed
-- independently from a different thread.
dupUIThreadOut :: UIThread r e o -> STM (UIThread r e o)
dupUIThreadOut UIThread{..} = do
  tc <- dupTChan outputTChan
  pure UIThread{outputTChan = tc, ..}
