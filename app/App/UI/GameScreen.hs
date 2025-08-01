{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeAbstractions #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE RecordWildCards #-}

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
import Data.Foldable (traverse_)
import Data.Maybe
import Control.Lens (makeLenses)
import Data.Text
import Data.Sequence (Seq)
import Brick.Widgets.Dialog

data Name = ExitDialogButtonYes | ExitDialogButtonNo | GameViewport
  deriving (Eq, Ord, Show)

data AppState = AppState
  { _gameExit :: Maybe GameExit
  , _consoleVisible :: Bool
  , _consoleHistory :: Seq Text
  , _consoleCurrentInput :: Text -- terrible asymptotics, but should only be a problem in case of falling asleep at the keyboard
  , _consoleCursorPosition :: Maybe Int -- Nothing - no line input currently allowed (e.g. something is running) 
  , _consoleHasFocus :: Bool
  , _exitDialog :: Maybe (Dialog () Name)
  }

data GameExit = ExitDesktop | ExitMainMenu | Crash String

makeLenses ''AppState

type role DisplayClock nominal
data DisplayClock s = forall st. DisplayClock (BrickThread st SimState s)

instance MonadIO m => Clock m (DisplayClock s) where
  type Time (DisplayClock s) = UTCTime
  {-
  This is a TQueue, which allows for an unbounded number of elements, seemingly
  defeating the point of Brick's BChan. This may be worrying, until you notice
  that the TQueue will only be populated once per tick, per running Rhine from
  newGameUI, and flushed before the next tick.

  The number of running Rhines is in turn bounded per running thread.
  (Unless we do something stupid, this bound is 1)
  (If the number of _threads_ is unbounded, then this is the least of our worries)
  -}
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

newGameUI :: forall s m. (MonadResource m, MonadSchedule m) => AppThread s -> m (ReleaseKey, Rhine (ExceptT GameExit m) _ SimState UserInput)
newGameUI th = do
  (rk, bth) <- newBrickThread th theapp s0
  let rh = inClSF @@ DisplayClock bth >-- trivialResamplingBuffer --> (tagS >>> arr absurd) @@ Never
      -- clock makes the Rhine throw GameExit when appropriate
      cl :: HoistClock (ExceptT (Either SomeException AppState) m) (ExceptT GameExit m) (BrickExitClock AppState s)
      cl = HoistClock (BrickExitClock bth) $ withExceptT \case
        Right st -> fromMaybe (Crash "No reason given for game exit") $ _gameExit st
        Left e -> Crash (displayException e)
      rh' = rh |@| (tagS @@ cl) @>>^ absurd
  pure (rk, rh')
  where
    s0 = undefined

theapp :: App AppState SimState Name
theapp = App {..}
  where
    appDraw s = [ gameWindow s
                , if _consoleVisible s
                    then consoleWindow (_consoleHistory s)
                    else emptyWidget
                , maybe emptyWidget (`renderDialog` emptyWidget) (_exitDialog s)
                ]

    gameWindow _ = viewport GameViewport Both emptyWidget
    consoleWindow = undefined

    appAttrMap = undefined

    appHandleEvent = undefined

    appChooseCursor = undefined

    appStartEvent = undefined
