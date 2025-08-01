{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
module App.Thread (AppThread, withAppThread, BrickThread(brickBChan), newBrickThread, BrickExitClock(..)) where

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
      let output = outputIface v
      when (supportsMode output BracketedPaste) $
        setMode output BracketedPaste True

      pure v

-- | Minimal BChan size since we assume events will be pushed at exactly the
-- rate Brick can handle them or less (e.g. the events are game states to be
-- drawn)
brickBChanSize :: Int
brickBChanSize = 1

data BrickThread st e s = BrickThread
  { appThread :: !(AppThread s)
  , brickBChan :: !(BChan e)
  , brickAsync :: !(Async (st, Vty))
  , rk :: !ReleaseKey
  }

newBrickThread :: (MonadResource m, Ord n) => AppThread s -> App st e n -> st -> m (ReleaseKey, BrickThread st e s)
newBrickThread appThread theapp initialState = do
  brickBChan <- liftIO $ newBChan brickBChanSize
  vty' <- liftIO $ readIORef (vty appThread)
  (rk, brickAsync) <- allocate
    do
      a <- async $ customMainWithVty vty' (rebuildVty appThread) (Just brickBChan) theapp initialState
      link a
      pure a
    uninterruptibleCancel
  pure (rk, BrickThread{..})

data BrickExitClock st s = forall e. BrickExitClock (BrickThread st e s)

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