{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeAbstractions #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module App.UI.GameScreen (newGameUI, GameExit(..), SymmContT(..)) where

import Control.Monad.IO.Class
import App.Thread
import FRP.Rhine
import Simulation
import Input
import Data.Time (getCurrentTime)
import qualified Data.Automaton.Trans.Except as A
import Brick.BChan
import Data.Void
import UnliftIO
import Brick
import Control.Monad.Schedule.Class (MonadSchedule)
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Cont
import Control.Concurrent.STM (flushTQueue, check)
import Data.Foldable (traverse_)

data GameExit = ExitDesktop | ExitMainMenu

type role DisplayClock nominal
data DisplayClock s = forall st. DisplayClock (BrickThread st SimState s)

instance MonadIO m => Clock m (DisplayClock s) where
  type Time (DisplayClock s) = UTCTime
  type Tag (DisplayClock s) = TQueue SimState
  initClock (DisplayClock th) = do
    t0 <- liftIO getCurrentTime
    tq <- newTQueueIO
    let rcl = A.forever do
          -- complain about needing more display data once
          A.step . const $ do
            events <- atomically $ flushTQueue tq
            liftIO $ traverse_ (writeBChan (brickBChan th)) events
            t <- liftIO getCurrentTime
            pure ((t, tq), ())
          -- then block until more data is given
          A.once_ . liftIO $ atomically do
            b <- isEmptyTQueue tq
            check (not b)
          -- repeat
    return (rcl, t0)

instance GetClockProxy (DisplayClock s)

-- type role UserInputClock nominal
-- newtype UserInputClock s = UserInputClock (BrickThread s)
-- 
-- instance MonadIO m => Clock m (UserInputClock s) where
--   type Time (UserInputClock s) = UTCTime
--   type Tag (UserInputClock s) = UserInput
--   initClock = undefined
-- 
-- instance GetClockProxy (UserInputClock s)

newtype AppState = AppState
  { exitCategory :: GameExit -- ^ Where to go if the app exits now
  }

initialAppState :: AppState
initialAppState = AppState
  { exitCategory = ExitMainMenu }

inClSF :: MonadIO m => ClSF (ExceptT GameExit m) (DisplayClock s) SimState ()
inClSF = proc s -> do
  tq <- tagS -< ()
  {-
  Note that `atomically . writeTQueue tq` is not going to block because
  we will only ever have one thread writing to the TQueue
  (unless we do something stupid, like
  
  rh <- newGameUI >>= feedbackRhine ...
  _ <- forkIO $ reactimate rh
  reactimate rh
  
  )
  -}
  arrMCl (\(tc, s) -> atomically $ writeTQueue tc s) -< (tq, s)

newtype SymmContT m a = SymmContT { unSymmCont :: ContT a m a }

newGameUI :: forall s m. (MonadResource m, MonadSchedule m) => AppThread s -> m (Rhine (ExceptT GameExit m) _ SimState UserInput)
newGameUI th = do
  bth <- newBrickThread th theapp initialAppState
  let rh = inClSF @@ DisplayClock bth >-- trivialResamplingBuffer --> (tagS >>> arr absurd) @@ Never
      -- clock makes the Rhine throw GameExit when appropriate
      cl :: HoistClock (ExceptT AppState m) (ExceptT GameExit m) (BrickExitClock AppState s)
      cl = HoistClock (BrickExitClock bth) (withExceptT exitCategory)
      rh' = rh |@| (tagS @@ cl) @>>^ absurd
  pure rh'

theapp :: App AppState SimState ()
theapp = undefined
