{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
module App.UI.LoadingScreen (withLoadingScreen) where

import Data.Void
import App.Thread
import Brick
import Brick.Widgets.Center
import Graphics.Vty
import Control.Concurrent.STM
import Control.Exception

withLoadingScreen :: AppThread -> IO a -> IO a
withLoadingScreen th k = withLoadingScreen' th \lth -> do
  a <- k
  atomically $ sendBrickEvent lth ()
  result <- atomically $ waitBrickThread lth
  case result of
    Right () -> pure a
    Left e -> throwIO e

-- | Halts on first () event received.
withLoadingScreen' :: AppThread -> (BrickThread () () Void -> IO a) -> IO a
withLoadingScreen' th = withBrickThread th theapp ()

theapp :: v -> App () () ()
theapp _ = App {..}
  where
    appDraw _ = [ center (str "Loading...") ]
    appHandleEvent (AppEvent ()) = halt
    appHandleEvent _ = continueWithoutRedraw
    appStartEvent = pure ()
    appAttrMap _ = attrMap defAttr []
    appChooseCursor = neverShowCursor
