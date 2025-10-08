{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
module App.UI.GameScreen (withGameUI, GameExit(..)) where

import Control.Monad.IO.Class
import App.Thread
import Simulation
import Input
import Brick
import Control.Lens
import Brick.Widgets.Dialog
import Data.Foldable (traverse_, toList)
import Graphics.Vty (defAttr, Event (..), Key (..), Modifier (..), Vty (outputIface), displayBounds, DisplayRegion)
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
import Control.Concurrent.STM

data Name
  = ExitDialogButtonYes
  | ExitDialogButtonNo
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
  , _windowSize :: DisplayRegion
  }

data GameExit = ExitDesktop | ExitMainMenu

makeLenses ''AppState

withGameUI :: AppThread -> SimState -> (BrickThread (Maybe GameExit) (Either SimState [SimEvent]) UserInput -> IO a) -> IO a
withGameUI th ss0 k = do
  v <- appThreadVty th
  _windowSize <- displayBounds (outputIface v)
  let s0 = AppState
        { _gameExit = Nothing
        , _exitDialog = Nothing
        , _simState = ss0
        , _logLines = mempty
        , _logIndex = 0
        , _windowSize
        }
  withBrickThread th theapp s0 $ k . mapBrickResult (^. gameExit)

theapp :: TQueue UserInput -> App AppState (Either SimState [SimEvent]) Name
theapp q = App {..}
  where
    appDraw s =
      [
        vLimitPercent 20 . hCenterLayer . border $ drawLogs (s ^. logIndex) (s ^. logLines)
      , maybe (gameWindow s) (`renderDialog` gameWindow s) (s ^. exitDialog)
      ]

    gameWindow s = drawSimState (s ^. windowSize) (s ^. simState)

    appAttrMap _ = attrMap defAttr []

    -- TODO: replace with dialogue-based exit
    appHandleEvent (VtyEvent (EvKey (KChar 'q') _)) = do
      L.assign gameExit (Just ExitMainMenu)
      halt
    appHandleEvent (VtyEvent (EvKey k m)) = do
      traverse_ (liftIO . atomically . writeTQueue q) (directInput k m)
      continueWithoutRedraw -- input forwarded directly to simulation thread, not immediately visible
    appHandleEvent (AppEvent (Left ss)) = L.assign simState ss
    appHandleEvent (AppEvent (Right se)) = do
      let se' = mconcat se
      -- Prune existing logs
      logLines %= (\ll -> Seq.drop (max 0 $ length ll - 20) ll)
      logIndex %= (+ length (simLogs se'))
      logLines %= (<> simLogs se')
    appHandleEvent (VtyEvent (EvResize w h)) =
      windowSize .= (w,h)

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

drawSimState :: DisplayRegion -> SimState -> Widget Name
drawSimState (width, height) SimState{..} = vBox (map hBox cells)
  where
    (cameraCellX, cameraCellY) = simToCell camera

    cells :: [[Widget Name]]
    cells = do
      y <- map ((+ cameraCellY) . negate . subtract (height `div` 2)) [0 .. height]
      pure do
        x <- map ((+ cameraCellX) . subtract (width `div` 2)) [0 .. width]
        let (x', y') = cellToSim (x,y)
            dists = flip concatMap objects \VisibleObject{..} -> do
              guard $ norm ((x', y') ^-^ position) <= radius
              pure (sdf ((x', y') ^-^ position))
            occupying = List.find (< 0.5) dists
        -- TODO consider object identifier while drawing
        pure $ if isJust occupying then occCell else emptyCell

    occCell = str "█"
    emptyCell = str " "

    simToCell :: (Double, Double) -> (Int, Int)
    simToCell (x,y) = (round (x*2), round y)

    cellToSim :: (Int, Int) -> (Double, Double)
    cellToSim (x,y) = ((fromIntegral x / 2) + 0.25, fromIntegral y + 0.5)

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
directInput (KChar ' ') _ = Just mempty { jump = True }
directInput _ _ = Nothing
