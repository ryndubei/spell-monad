{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ApplicativeDo #-}
module App.Thread (AppThread(vty), withAppThread, BrickThread, newBrickThread, BrickExitClock(..), DisplayClock(..), sendBrickEvent) where

import Graphics.Vty
import Graphics.Vty.CrossPlatform
import Control.Exception
import Data.IORef
import Brick.BChan
import Control.Concurrent.Async
import Control.Monad.Trans.Resource
import Brick
import Control.Monad.IO.Class
import FRP.Rhine
import Data.Void
import Data.Time
import Control.Monad
import Control.Concurrent.STM (check, STM)
import qualified Data.Automaton.Trans.Except as A
import UnliftIO.STM (atomically)
import Control.Concurrent.STM.TQueue
import Data.Profunctor

type role AppThread nominal
-- | ST trick to force each app thread to be a distinct type
data AppThread s = AppThread
  { vty :: !(IORef Vty)
  , rebuildVty :: !(IO Vty)
  }

withAppThread :: (forall s. AppThread s -> IO a) -> IO a
withAppThread f = do
  bracket
    do
      vty1 <- ourMkVty
      vty <- newIORef vty1

      let rebuildVty = do
            v <- ourMkVty
            writeIORef vty v
            pure v

      pure AppThread {..}
    (\AppThread{..} -> readIORef vty >>= shutdown)
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

data BrickThread s e st = forall e1. BrickThread
  { appThread :: !(AppThread s)
  , brickBChan :: !(BChan e1)
  , brickAsync :: !(Async (st, Vty))
  , rk :: !ReleaseKey
  {- | This is a TQueue, which allows for an unbounded number of elements, seemingly
  defeating the point of Brick's BChan. This may be worrying, but the TQueue will be
  emptied before every subsequent tick of DisplayClock. As long as the
  number of writes per tick is bounded, then so is the number of elements in the
  queue.
  -}
  , eventTQueue :: !(TQueue e1)
  , eventMapper :: e -> e1
  }

deriving instance Functor (BrickThread s e)

instance Profunctor (BrickThread s) where
  lmap f BrickThread{..} =
    let eventMapper' = eventMapper . f 
     in BrickThread{eventMapper = eventMapper', ..}
  rmap = fmap

newBrickThread :: (MonadResource m, Ord n) => AppThread s -> App st e n -> st -> m (ReleaseKey, BrickThread s e st)
newBrickThread appThread theapp initialState = do
  brickBChan <- liftIO $ newBChan brickBChanSize
  vty' <- liftIO $ readIORef (vty appThread)
  eventTQueue <- liftIO newTQueueIO 
  (rk, (brickAsync, _)) <- allocate
    do
      a1 <- async $ customMainWithVty vty' (rebuildVty appThread) (Just brickBChan) theapp initialState
      link a1
      -- queue-to-BChan writer thread
      a2 <- async $ Control.Monad.forever do
        e <- atomically $ readTQueue eventTQueue
        writeBChan brickBChan e
      link a2
      pure (a1, a2)
    \(a1, a2) -> do
      uninterruptibleCancel a2
      uninterruptibleCancel a1
  let eventMapper = id
  pure (rk, BrickThread{..})

-- | Clock ticks whenever Brick's event queue is empty.
-- TODO next commit: DisplayClock s 
data DisplayClock e s = forall st. DisplayClock !(BrickThread s e st)

sendBrickEvent :: BrickThread st e s -> e -> STM ()
sendBrickEvent BrickThread{..} = writeTQueue eventTQueue . eventMapper

instance MonadIO m => Clock m (DisplayClock e s) where
  type Time (DisplayClock e s) = UTCTime
  type Tag (DisplayClock e s) = ()
  initClock (DisplayClock BrickThread{..}) = do
    t0 <- liftIO getCurrentTime
    let tq = eventTQueue
        rc = A.forever do
          -- complain about needing more display data once the TQueue is
          -- emptied by the writer thread
          A.step $ const do
            atomically do
              b <- isEmptyTQueue tq
              check b
            t <- liftIO getCurrentTime
            pure ((t, ()), ())
          -- block until the queue is written to
          A.once_ . liftIO $ atomically do
            b <- isEmptyTQueue tq
            check (not b)
    pure (rc, t0)

instance GetClockProxy (DisplayClock e s)

data BrickExitClock st s = forall e. BrickExitClock (BrickThread s e st)

instance MonadIO m => Clock (ExceptT (Either SomeException st) m) (BrickExitClock st s) where
  type Time (BrickExitClock st s) = UTCTime
  type Tag (BrickExitClock st s) = Void
  initClock (BrickExitClock bth) = do
    t0 <- liftIO getCurrentTime
    let rcl = constM do
          esv <- liftIO . Control.Exception.try $ wait (brickAsync bth)
          case esv of
            Right (s,v) -> do
              liftIO $ writeIORef (vty $ appThread bth) v
              release (rk bth)
              throwE (Right s)
            Left e -> do
              v <- liftIO $ rebuildVty (appThread bth)
              liftIO $ writeIORef (vty $ appThread bth) v 
              release (rk bth)
              throwE (Left e)
    pure (rcl, t0)

instance GetClockProxy (BrickExitClock st s)