{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import App.Thread
import Control.Concurrent.STM
import Control.Exception
import App.UI.GameScreen.Terminal
import Control.Lens.Operators
import Brick
import Data.Void
import Graphics.Vty.Attributes
import Control.Monad
import Graphics.Vty.Input

main :: IO ()
main = withAppThread \th -> do
  withBrickThread th theapp term \bth -> do
    atomically (waitBrickThread bth) >>= either throwIO (const $ pure ())
  where
    term = (prompt .~ "foo> ") terminal

theapp :: TQueue Void -> App Terminal Void ()
theapp _ = App {..}
  where
    appDraw s = [drawTerminal () s]

    appAttrMap _ = attrMap defAttr []

    appChooseCursor = showFirstCursor

    appHandleEvent (VtyEvent (EvKey KEsc _)) = halt
    appHandleEvent e = void $ handleTerminalEvent e

    appStartEvent = pure ()
