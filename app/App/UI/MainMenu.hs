{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE RecordWildCards #-}
module App.UI.MainMenu (MenuExit(..), newMainMenu) where

import App.Thread
import FRP.Rhine
import GameData
import Control.Monad.Trans.Resource
import Data.Void
import Brick
import Control.Monad.Schedule.Class
import Brick.Widgets.Dialog
import qualified Data.Text as T
import Control.Lens (makeLenses)
import Control.Lens.Operators
import Data.Foldable
import Graphics.Vty

data MenuExit = Quit | Load Level

newtype MainMenuState = MainMenuState
  { _mainMenuDialog :: Dialog Selectability Name }

data Name = ButtonContinue | ButtonNewGame | ButtonLoad | ButtonQuit
  deriving (Eq, Ord, Show)

data Selectability = Selectable | Unselectable deriving (Eq, Show)

makeLenses ''MainMenuState

mainMenuMaxWidth :: Int
mainMenuMaxWidth = 60

initialMainMenuState :: MainMenuState
initialMainMenuState = MainMenuState { _mainMenuDialog }
  where
    _mainMenuDialog = dialog (Just title) (Just buttons) mainMenuMaxWidth

    title = txt $ T.unlines
      [ "HaskellQuest (working title)"
      , "Version 0.1.0.0"
      ]

    buttons = (ButtonNewGame,
      [ ("Continue", ButtonContinue, Unselectable)
      , ("Load Game", ButtonLoad, Unselectable)
      , ("New Game", ButtonNewGame, Selectable)
      , ("Quit", ButtonQuit, Selectable)
      ])

newMainMenu :: forall s m. (MonadResource m, MonadSchedule m) => AppThread s -> m (Rhine (ExceptT MenuExit m) _ () ())
newMainMenu th = do
  bth <- newBrickThread th theapp initialMainMenuState
  let cl :: HoistClock (ExceptT MainMenuState m) (ExceptT MenuExit m) (BrickExitClock MainMenuState s)
      cl = HoistClock (BrickExitClock bth) (withExceptT getExit)
  pure $ (arr id @@ Never) |@| (tagS @@ cl) @>>^ absurd
  where
    getExit s = case getDialogFocus (_mainMenuDialog s) of
      Nothing -> Quit
      Just ButtonQuit -> Quit
      Just ButtonNewGame -> Load (error "new game")
      Just ButtonLoad -> Load (error "load")
      Just ButtonContinue -> Load (error "continue")

selectableButtonsOnly :: Dialog Selectability Name -> Dialog Selectability Name
selectableButtonsOnly = dialogButtonsL %~ filter (\(_, _, sel) -> sel == Selectable)

theapp :: App MainMenuState Void Name
theapp = App {..}
  where
    appDraw s = [ renderDialog (_mainMenuDialog s) emptyWidget ]

    appHandleEvent (VtyEvent e) = do

      -- Handle dialog movement events, skipping unselectable buttons
      s <- Brick.get

      dl' <- nestEventM' (selectableButtonsOnly $ _mainMenuDialog s) (handleDialogEvent e)
      let mn = getDialogFocus dl'
      zoom mainMenuDialog $ traverse_ (Brick.modify . setDialogFocus) mn

      -- Handle Enter key press
      case e of
        EvKey KEnter _ -> do
          sel <- dialogSelection . _mainMenuDialog <$> Brick.get
          case sel of
            Nothing -> pure ()
            Just (_, Unselectable) -> pure ()
            -- exit command to be interpreted by BrickExitClock
            Just (ButtonContinue, _) -> Brick.halt
            Just (ButtonNewGame, _) -> Brick.halt
            Just (ButtonLoad, _) -> undefined -- TODO: load game screen
            Just (ButtonQuit, _) -> Brick.halt
        _ -> pure ()

    appHandleEvent (AppEvent v) = absurd v
    appHandleEvent (MouseDown {}) = pure ()
    appHandleEvent (MouseUp {}) = pure ()

    appAttrMap _ = attrMap defAttr [ (buttonSelectedAttr, defAttr `withStyle` standout)]

    appChooseCursor = neverShowCursor

    appStartEvent = pure ()
