{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeAbstractions #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
module App.UI.GameScreen (newGameUI, GameExit(..)) where

import Control.Monad.IO.Class
import App.Thread
import FRP.Rhine
import Simulation
import Input
import Data.Void
import UnliftIO
import Brick
import Control.Monad.Schedule.Class (MonadSchedule)
import Control.Monad.Trans.Resource
import Data.Maybe
import Control.Lens (makeLenses)
import Brick.Widgets.Dialog
import Data.Foldable (traverse_)
import Control.Lens.Operators
import Graphics.Vty (defAttr, Event (..), Key (..), Modifier (..))

data Name
  = ExitDialogButtonYes
  | ExitDialogButtonNo
  | GameViewport
  | TerminalViewport
  deriving (Eq, Ord, Show)

data AppState = AppState
  { _gameExit :: !(Maybe GameExit)
  , _exitDialog :: !(Maybe (Dialog () Name))
  }

data GameExit = ExitDesktop | ExitMainMenu | Crash String

makeLenses ''AppState

neverIn
  :: (MonadIO m, Time (In cl) ~ UTCTime, Time cl ~ UTCTime, Clock m cl, Clock m (Out cl), Clock m (In cl), GetClockProxy cl)
  => Rhine m cl () b
  -> Rhine m (Never `SeqClock` cl) a b
neverIn rh = (tagS >>> arr absurd) @@ Never >-- trivialResamplingBuffer --> rh

neverOut
  :: (MonadIO m, Time (Out cl) ~ UTCTime, Time cl ~ UTCTime, Clock m cl, Clock m (Out cl), Clock m (In cl), GetClockProxy cl)
  => Rhine m cl a ()
  -> Rhine m (cl `SeqClock` Never) a b
neverOut rh = rh >-- trivialResamplingBuffer --> (tagS >>> arr absurd) @@ Never

inClSF :: MonadIO m => BrickThread s SimState st -> ClSF m cl SimState ()
-- Despite the `atomically`, presume this to be non-blocking
-- If this takes enough time to not be non-blocking, we have more pressing
-- things to worry about than clock accuracy
inClSF bth = arrMCl (atomically . sendBrickEvent bth)

displayRhine :: forall m s. MonadIO m => BrickThread s SimState AppState -> Rhine m _ SimState UserInput
displayRhine bth = neverOut $ inClSF bth @@ DisplayClock bth

newGameUI
  :: forall s m. (MonadSchedule m, MonadIO m)
  => AppThread s
  -> ResourceT m (ReleaseKey, Rhine (ExceptT GameExit m) _ SimState UserInput)
newGameUI th = do
  -- Output event channel to pass to the app
  -- Unbounded memory usage, but it's expected: we don't want to
  -- silently lose any user input (as opposed to explicitly via ResamplingBuffers)
  (appC :: Chan UserInput) <- UnliftIO.newChan

  (rk, bth) <- newBrickThread th (theapp appC) s0

  let -- ignore the constant type annotations for the monad we are in, the compiler complains without them
      uClock = eventClockOn @(ExceptT GameExit m) appC

  let -- cl1 makes the Rhine throw GameExit when appropriate
      cl1 = HoistClock (BrickExitClock bth) $ withExceptT @m
        (either
          (Crash . displayException @SomeException)
          (fromMaybe (Crash "No reason given for game exit") . _gameExit))
      exitRhine = neverIn $ (tagS @@ cl1) @>>^ absurd
      userInputRhine = neverIn (tagS @@ uClock)
      rh = displayRhine bth |@| (exitRhine |@| userInputRhine)
  pure (rk, rh)
  where
    s0 = AppState
      { _gameExit = Nothing
      , _exitDialog = Nothing
      }


theapp :: Chan UserInput -> App AppState SimState Name
theapp c = App {..}
  where
    appDraw s = [ maybe (gameWindow s) (`renderDialog` gameWindow s) (s ^. exitDialog) ]

    gameWindow _ = viewport GameViewport Both (vLimit 100 $ hLimit 100 $ fill 'A')

    appAttrMap _ = attrMap defAttr []

    appHandleEvent (VtyEvent (EvKey k m)) = traverse_ (liftIO . writeChan c) (directInput k m)

    appHandleEvent _ = pure ()

    appChooseCursor = neverShowCursor

    appStartEvent = pure ()

directInput :: Key -> [Modifier] -> Maybe UserInput
directInput KUp _ = Just $ mempty { moveY = 1 }
directInput KUpRight _ = Just mempty { moveX = 1 / sqrt 2, moveY = 1 / sqrt 2}
directInput KRight _ = Just mempty { moveX = 1 }
directInput KDownRight _ = Just mempty { moveX = 1 / sqrt 2, moveY = -(1 / sqrt 2)}
directInput KDown _ = Just mempty { moveY = -1 }
directInput KDownLeft _ = Just mempty { moveX = -(1 / sqrt 2), moveY = -(1 / sqrt 2)}
directInput KLeft _ = Just mempty { moveX = -1 }
directInput KUpLeft _ = Just mempty { moveX = -(1 / sqrt 2), moveY = 1 / sqrt 2}
directInput (KChar ' ') _ = Just mempty { jump = True }
directInput KEsc _ = Just mempty { togglePause = True }
directInput _ _ = Nothing
