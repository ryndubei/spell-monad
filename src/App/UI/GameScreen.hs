{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
module App.UI.GameScreen (withGameUI, GameExit(..)) where

import Control.Monad.IO.Class
import App.Thread
import Simulation
import Input
import Brick
import Control.Lens
import Brick.Widgets.Dialog
import Graphics.Vty (defAttr, Event (..), Key (..), Modifier (..), Vty (outputIface), displayBounds)
import qualified Control.Lens as L
import Data.Text (Text)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Concurrent.STM
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Graphics.Vty.Image
import Data.Foldable
import Control.Parallel.Strategies (parMap, rdeepseq)

data Name
  = TerminalViewport
  | LogViewport
  deriving (Eq, Ord, Show)

data AppState = AppState
  { _gameExit :: !(Maybe GameExit)
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
      , gameWindow s
      ]

    gameWindow s = drawSimState (appAttrMap s) (s ^. windowSize) (s ^. simState)

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

drawSimState :: AttrMap -> DisplayRegion -> SimState -> Widget Name
drawSimState attrmap (width, height) SimState{..} = raw displayedSimState
  where
    ys :: Vector Double
    ys = V.map ((+ cameraY) . negate . fromIntegral) $ V.enumFromN (- (height `div` 2)) height

    xs :: V.Vector Double
    xs = V.map ((+ cameraX) . (/ 2) . fromIntegral) $ V.enumFromN (- (width `div` 2)) width

    dists :: V.Vector (V.Vector (ObjectIdentifier, Double))
    dists = flip V.map ys \y -> flip V.map xs \x -> sdf (x,y)

    -- ObjectIdentifiers together with the string lengths to draw
    lineSliceLengths :: [V.Vector (Maybe ObjectIdentifier, Int)]
    lineSliceLengths = flip (parMap rdeepseq) (V.toList dists) $ V.unfoldrN width \row ->
      let u = V.uncons row
      in case u of
        Just (hrow, trow) ->
          let toDraw (oid, d) = if d < 0.5 then Just oid else Nothing
              (lrow, rrow) = V.span (\t -> toDraw t == toDraw hrow) trow
           in Just ((toDraw hrow, 1 + length lrow), rrow)
        Nothing -> Nothing

    lineSlice :: Maybe ObjectIdentifier -> Int -> Image
    -- cannot simply do "translateX len mempty". I don't know why.
    lineSlice Nothing len = translateX len $ text' defAttr mempty
    lineSlice (Just oid) len = text' (attrMapLookup (objectIdToAttr oid) attrmap) $ T.replicate len "█"

    displayedSimState :: Image
    displayedSimState = foldr' vertJoin mempty $ map (V.foldr' horizJoin mempty . V.map (uncurry lineSlice)) lineSliceLengths

objectIdToAttr :: ObjectIdentifier -> AttrName
objectIdToAttr Player = attrName "Player"

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
