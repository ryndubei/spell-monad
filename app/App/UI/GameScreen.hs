{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
module App.UI.GameScreen (withGameUI, GameExit(..)) where

import Control.Monad.IO.Class
import App.Thread
import Simulation
import Input
import UnliftIO
import Brick
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

data GameExit = ExitDesktop | ExitMainMenu

makeLenses ''AppState

withGameUI :: AppThread -> (BrickThread SimState (Maybe GameExit) -> TQueue UserInput -> IO a) -> IO a
withGameUI th k = do
  -- Output event queue to pass to the app
  -- Unbounded memory usage, but this is necessary:
  -- we don't want to silently lose any user input
  q <- newTQueueIO

  withBrickThread th (theapp q) s0 \bth -> k ((^. gameExit) <$> bth) q
    
  where
    s0 = AppState
      { _gameExit = Nothing
      , _exitDialog = Nothing
      }


theapp :: TQueue UserInput -> App AppState SimState Name
theapp q = App {..}
  where
    appDraw s = [ maybe (gameWindow s) (`renderDialog` gameWindow s) (s ^. exitDialog) ]

    gameWindow _ = viewport GameViewport Both (vLimit 100 $ hLimit 100 $ fill 'A')

    appAttrMap _ = attrMap defAttr []

    appHandleEvent (VtyEvent (EvKey k m)) = traverse_ (liftIO . atomically . writeTQueue q) (directInput k m)

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
