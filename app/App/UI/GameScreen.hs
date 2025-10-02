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
import Data.Foldable (traverse_, toList)
import Control.Lens.Operators
import Graphics.Vty (defAttr, Event (..), Key (..), Modifier (..))
import qualified Control.Lens as L
import FRP.Yampa (VectorSpace(..))
import Control.Monad
import qualified Data.List as List
import Data.Maybe
import Data.Text (Text)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Brick.Widgets.Border
import Brick.Widgets.Center
import System.IO

data Name
  = ExitDialogButtonYes
  | ExitDialogButtonNo
  | GameViewport
  | TerminalViewport
  | LogViewport
  | ObjId ObjectIdentifier
  deriving (Eq, Ord, Show)

data AppState = AppState
  { _gameExit :: !(Maybe GameExit)
  , _exitDialog :: !(Maybe (Dialog () Name))
  , _simState :: !SimState
  , _logLines :: Seq Text
  , _logIndex :: !Int
  }

data GameExit = ExitDesktop | ExitMainMenu

makeLenses ''AppState

withGameUI :: AppThread -> SFThread UserInput o -> (BrickThread (Either SimState [Text]) (Maybe GameExit) -> IO a) -> IO a
withGameUI th sfth k = do
  withBrickThread th (theapp sfth) s0 \bth -> k ((^. gameExit) <$> bth)

  where
    s0 = AppState
      { _gameExit = Nothing
      , _exitDialog = Nothing
      , _simState = ss0
      , _logLines = mempty
      , _logIndex = 0
      }
    -- TODO: don't define a default state
    ss0 = SimState
      { camera = (0,0)
      , objects = []
      }


theapp :: SFThread UserInput o -> App AppState (Either SimState [Text]) Name
theapp sfth = App {..}
  where
    appDraw s =
      [
        vLimitPercent 20 . hCenterLayer . border $ drawLogs (s ^. logIndex) (s ^. logLines)
      , maybe (gameWindow s) (`renderDialog` gameWindow s) (s ^. exitDialog)
      ]

    gameWindow s = viewport GameViewport Both (drawSimState (s ^. simState))

    appAttrMap _ = attrMap defAttr []

    -- TODO: replace with dialogue-based exit
    appHandleEvent (VtyEvent (EvKey (KChar 'q') _)) = do
      L.assign gameExit (Just ExitMainMenu)
      halt
    appHandleEvent (VtyEvent (EvKey k m)) =
      traverse_ (liftIO . atomically . sendSFThread sfth) (directInput k m)
    appHandleEvent (AppEvent (Left ss)) = L.assign simState ss
    appHandleEvent (AppEvent (Right newLogLines)) = do
      -- Prune existing logs
      logLines %= (\ll -> Seq.drop (max 0 $ length ll - 20) ll)
      logIndex %= (+ length newLogLines)
      logLines %= (<> Seq.fromList newLogLines)

    appHandleEvent _ = pure ()

    appChooseCursor = neverShowCursor

    appStartEvent = pure ()

drawLogs :: Int -> Seq Text -> Widget Name
drawLogs logIdx sq = viewport LogViewport Vertical
  . vBox
  . zipWith (\i w -> str (show i ++ " ") <+> w) [logIdx - length sq ..]
  . toList
  . (L._last %~ visible)
  . fmap txt
  $ sq

drawSimState :: SimState -> Widget Name
-- the border is temporary for easier viewing, should be removed when we cover the whole screen
drawSimState SimState{..} = border $ vBox (map hBox cells)
  where
    cells :: [[Widget Name]]
    -- TODO optimise
    cells = do
      y <- reverse [(-30) .. 30]
      pure do
        x <- [(-30) .. 30]
        let hasCamera = (x,y) == simToCell camera
            f = if hasCamera then visible else id
            (x', y') = cellToSim (x,y)
            dists = flip concatMap objects \VisibleObject{..} -> do
              guard $ norm ((x', y') ^-^ position) <= radius
              pure (sdf ((x', y') ^-^ position))
            occupying = List.find (< 0.5) dists
        -- TODO consider object identifier while drawing
        pure . f $ if isJust occupying then occCell else emptyCell

    occCell = str "█"
    emptyCell = str " "

    simToCell :: (Double, Double) -> (Int, Int)
    simToCell (x,y) = (round x, round y)

    cellToSim :: (Int, Int) -> (Double, Double)
    cellToSim (x,y) = (fromIntegral x + 0.5, fromIntegral y + 0.5)

directInput :: Key -> [Modifier] -> Maybe UserInput
directInput KUp _ = Just $ mempty { moveY = 1 }
-- normalising to magnitude 1 is the responsibility of the game
directInput KUpRight _ = Just mempty { moveX = 1, moveY = 1}
directInput KRight _ = Just mempty { moveX = 1 }
directInput KDownRight _ = Just mempty { moveX = 1, moveY = -1}
directInput KDown _ = Just mempty { moveY = -1 }
directInput KDownLeft _ = Just mempty { moveX = -1, moveY = -1}
directInput KLeft _ = Just mempty { moveX = -1 }
directInput KUpLeft _ = Just mempty { moveX = -1, moveY = 1}
directInput _ _ = Nothing
