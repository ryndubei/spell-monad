{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ApplicativeDo #-}
module Main (main) where

import App.UI.MainMenu
import App.UI.GameScreen
import App.Thread
import GameData
import FRP.Rhine
import Control.Monad.Trans
import System.Exit
import Input
import Simulation
import Orphans ()
import Control.Monad.Trans.Resource
import GHC.Stack
import Control.Monad
import Data.Void

main :: HasCallStack => IO ()
main = withAppThread $ \th -> Control.Monad.forever do
  me <- runMainMenu th
  -- todo: proper crash screen
  l <- case me of
    Quit -> liftIO exitSuccess
    App.UI.MainMenu.Crash str -> liftIO $ fail str
    NewGame -> pure (Level SimState)
  ge <- runGame th l
  case ge of
    ExitDesktop -> liftIO exitSuccess
    App.UI.GameScreen.Crash str -> liftIO $ fail str
    ExitMainMenu -> pure ()

runMainMenu :: HasCallStack => AppThread s -> IO MenuExit
runMainMenu th = runResourceT do
  (_, rh) <- newMainMenu th
  fmap (either id absurd) . lift . runExceptT $ flow rh

runGame :: HasCallStack => AppThread s -> Level -> IO GameExit
runGame th level = runResourceT do
  (_, gurh) <- newGameUI th
  let rh = feedbackRhine rbSimToUI (arr snd ^>>@ gurh >-- rbUIToSim --> srh @>>^ arr ((),))
  fmap (either id absurd) . lift . runExceptT $ flow rh
  where
    srh = simRhine (initialSimState level)
    rbUIToSim :: MonadIO m => ResamplingBuffer m clUI clS UserInput UserInput
    rbUIToSim = foldBuffer (<>) mempty
    rbSimToUI :: MonadIO m => ResamplingBuffer m clS clUI SimState SimState
    rbSimToUI = keepLast (initialSimState level)
