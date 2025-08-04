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
import Control.Monad.Schedule.Class
import Orphans ()
import Control.Monad.Trans.Resource
import App.UI.GameScreen.Haskeline

main :: IO ()
main = withAppThread (runResourceT . A.reactimate . mainAutomaton)

mainAutomaton :: (MonadUnliftIO m, MonadSchedule m, MonadResource m) => AppThread s -> A.Automaton m () ()
mainAutomaton th = A.forever do
  me <- mainMenuAutomaton th
  -- todo: proper crash screen
  l <- case me of
    Quit -> liftIO exitSuccess
    App.UI.MainMenu.Crash str -> liftIO $ fail str
    NewGame -> pure (Level SimState)
  ge <- gameAutomaton th l
  case ge of
    ExitDesktop -> liftIO exitSuccess
    App.UI.GameScreen.Crash str -> liftIO $ fail str
    ExitMainMenu -> pure ()

mainMenuAutomaton :: (MonadUnliftIO m, MonadSchedule m, MonadResource m) => AppThread s -> A.AutomatonExcept () () m MenuExit
mainMenuAutomaton th = do
  (rk, rh) <- lift $ newMainMenu th
  aut <- lift . runExceptT . fmap (rmap (const ())) $ eraseClock rh
  me <- either pure A.try aut
  lift $ release rk
  pure me

gameAutomaton :: (MonadUnliftIO m, MonadSchedule m, MonadResource m) => AppThread s -> Level -> A.AutomatonExcept () () m GameExit
gameAutomaton th level = do
  (rkHth, shth) <- lift newHaskelineThread
  ge <- withSome shth $ \hth -> do
    (rk2, gurh) <- lift $ newGameUI th hth
    let rh = feedbackRhine rbSimToUI (arr (\(_, s) -> (s, Nothing)) ^>>@ gurh >-- rbUIToSim --> srh @>>^ arr ((),))
    aut <- lift . runExceptT . fmap (rmap (const ())) $ eraseClock rh
    ge' <- either pure A.try aut
    lift $ release rk2
    pure ge'
  release rkHth
  pure ge
  where
    srh = simRhine (initialSimState level)
    rbUIToSim :: MonadIO m => ResamplingBuffer m clUI clS UserInput (Maybe UserInput)
    rbUIToSim = fifoUnbounded -- unbounded because it would be bad to lose any REPL input lines
    rbSimToUI :: MonadIO m => ResamplingBuffer m clS clUI SimState SimState
    rbSimToUI = keepLast (initialSimState level)
