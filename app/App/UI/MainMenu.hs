{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module App.UI.MainMenu (MenuExit(..), withMainMenu) where

import App.Thread
import Data.Void
import Brick
import Control.Lens (makeLenses)
import Graphics.Vty
import Brick.Focus
import qualified Data.CircularList as CList
import Control.Lens.Operators
import Brick.Widgets.Center
import Brick.Widgets.Border
import Control.Exception
import Data.Bifunctor
import Control.Monad.IO.Class

data MenuExit = Quit | NewGame

data MainMenuState = MainMenuState
  { _mainMenuFocusRing :: FocusRing Name
  -- alternatively: could throw an exception with the exit reason and catch it in the main thread
  , _mainMenuExit :: Maybe MenuExit
  -- , savedGames :: [GameSave]
  -- , lastPlayedLevel :: Maybe Level
  }

data Name = ButtonContinue | ButtonLoad | ButtonNewGame | ButtonOptions | ButtonQuit
  deriving (Eq, Ord, Show, Enum)

makeLenses ''MainMenuState

-- | Temporary: ButtonOptions will be selectable when options are implemented,
-- Load will be selectable when there are saved games, and Continue will be
-- selectable when there is a last-played saved game.
unselectableButtons :: [Name]
unselectableButtons = [ButtonContinue, ButtonLoad, ButtonOptions]

buttonAttr :: AttrName
buttonAttr = attrName "button"

selectedAttr :: AttrName
selectedAttr = attrName "selected"

unselectableAttr :: AttrName
unselectableAttr = attrName "unselectable"

mainMenuMaxWidth :: Int
mainMenuMaxWidth = 60

withMainMenu :: AppThread -> (BrickThread Void (Maybe MenuExit) -> IO a) -> IO a
withMainMenu th k = withBrickThread th theapp s0 $ k . fmap (^. mainMenuExit)
  where
    s0 = MainMenuState
      { -- TODO: initial focus should be set to Continue if available
        _mainMenuFocusRing = focusSetCurrent ButtonNewGame $ focusRing [ButtonContinue, ButtonLoad, ButtonNewGame, ButtonOptions, ButtonQuit]
      , _mainMenuExit = Nothing
      }

data KeyMoveDirection = KeyPrev | KeyNext

theapp :: App MainMenuState Void Name
theapp = App {..}
  where
    appDraw s =
      pure . center . hLimit mainMenuMaxWidth $ (titleWidget <=> vBox (buttons s))

    titleWidget = hCenter (str "HaskellQuest (working title)") <=> hCenter (str "Version 0.1.0.0")
    buttons s =
      let sel = focusGetCurrent $ _mainMenuFocusRing s
          btn l = border $ hCenter (str l)
          bts = [(ButtonContinue, "Continue"), (ButtonLoad, "Load Game"), (ButtonNewGame, "New Game"), (ButtonOptions, "Options"), (ButtonQuit, "Quit")]
          -- Create widgets for each button
          bts' = map (second btn) bts
          -- Apply attrs
          bts'' = flip map bts' $ \(n, w) ->
            let nSelected = Just n == sel
                nUnselectable = n `elem` unselectableButtons
                att = buttonAttr <> (if nSelected then selectedAttr else mempty) <> (if nUnselectable then unselectableAttr else mempty)
             in withAttr att w
       in bts''

    appAttrMap _ = attrMap defAttr
      [ (buttonAttr <> selectedAttr, style standout)
      , (buttonAttr <> unselectableAttr, style dim)
      , (buttonAttr <> selectedAttr <> unselectableAttr, style dim `withStyle` standout)
      ]

    appHandleEvent (VtyEvent e)
      | isForceExitEvent e = liftIO $ throwIO UserInterrupt -- Honour Ctrl-C
      | Just dir <- moveDir e = do
          fr <- gets _mainMenuFocusRing
          -- Filtered focus ring that removes buttons which cannot be selected
          let fr' = focusRingModify (CList.filterR (`notElem` unselectableButtons)) fr
              sel = focusGetCurrent $ case dir of
                KeyPrev -> focusPrev fr'
                KeyNext -> focusNext fr'
          case sel of
            Nothing -> pure ()
            Just sel' -> modify (mainMenuFocusRing %~ focusSetCurrent sel')
      | isSelectEvent e = do
          sel <- gets (focusGetCurrent . _mainMenuFocusRing)
          case sel of
            Nothing -> pure ()
            Just ButtonContinue -> pure () -- TODO
            Just ButtonNewGame -> do
              modify (mainMenuExit ?~ NewGame)
              Brick.halt
            Just ButtonLoad -> pure () -- TODO
            Just ButtonQuit -> do
              modify (mainMenuExit ?~ Quit)
              Brick.halt
            Just ButtonOptions -> pure () -- TODO
      | otherwise = pure ()

    appHandleEvent (AppEvent v) = absurd v
    appHandleEvent (MouseDown {}) = pure ()
    appHandleEvent (MouseUp {}) = pure ()

    isSelectEvent (EvKey KEnter _) = True
    isSelectEvent _ = False

    moveDir (EvKey KUp _) = Just KeyPrev
    moveDir (EvKey KDown _) = Just KeyNext
    moveDir (EvKey KBackTab _) = Just KeyPrev
    moveDir (EvKey (KChar '\t') _) = Just KeyNext
    moveDir _ = Nothing

    isForceExitEvent (EvKey (KChar 'c') [MCtrl]) = True
    isForceExitEvent _ = False

    appChooseCursor = neverShowCursor

    appStartEvent = pure ()
