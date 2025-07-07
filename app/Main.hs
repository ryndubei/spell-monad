{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ApplicativeDo #-}
module Main (main) where

import App.UI.MainMenu
import App.UI.GameScreen
import App.Thread
import GameData
import FRP.Rhine
import qualified Data.Automaton as A
import qualified Data.Automaton.Trans.Except as A
import Control.Monad.Trans
import Data.Profunctor
import System.Exit
import Input
import Simulation
import UnliftIO
import Control.Monad.Morph
import Control.Monad.Schedule.Class
import Orphans ()
import Control.Monad.Trans.Resource

userInputBufferSize :: Num a => a
userInputBufferSize = 64

main :: IO ()
main = withAppThread (A.reactimate . mainAutomaton)

mainAutomaton :: (MonadIO m, MonadUnliftIO m, MonadSchedule m) => AppThread s -> A.Automaton m () ()
mainAutomaton th = A.forever do
  me <- mainMenuAutomaton th
  l <- case me of
    Quit -> liftIO exitSuccess
    Load l' -> pure l'
  ge <- gameAutomaton th l
  case ge of
    ExitDesktop -> liftIO exitSuccess
    ExitMainMenu -> pure ()

mainMenuAutomaton :: (MonadUnliftIO m, MonadSchedule m) => AppThread s -> A.AutomatonExcept () () m MenuExit
mainMenuAutomaton th = hoist runResourceT do
  rh <- lift $ newMainMenu th
  aut <- lift . runExceptT . fmap (rmap (const ())) $ eraseClock rh
  either pure A.try aut

gameAutomaton :: (MonadUnliftIO m, MonadSchedule m) => AppThread s -> Level -> A.AutomatonExcept () () m GameExit
gameAutomaton th level = hoist runResourceT do
  gurh <- lift $ newGameUI th
  let rh = feedbackRhine rbSimToUI (arr snd ^>>@ gurh >-- rbUIToSim --> srh @>>^ arr ((),))
  aut <- lift . runExceptT . fmap (rmap (const ())) $ eraseClock rh
  either pure A.try aut
  where
    srh = simRhine (initialSimState level)
    rbUIToSim :: MonadIO m => ResamplingBuffer m clUI clS UserInput (Maybe UserInput)
    rbUIToSim = fifoBounded userInputBufferSize
    rbSimToUI :: MonadIO m => ResamplingBuffer m clS clUI SimState SimState
    rbSimToUI = keepLast (initialSimState level)
