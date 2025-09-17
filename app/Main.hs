{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import App.UI.MainMenu
import App.UI.GameScreen
import App.Thread
import Simulation
import GHC.Stack
import Control.Monad
import FRP.BearRiver
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.IO.Class
import Data.Functor.Identity
import Data.Semigroup
import System.Exit
import Input
import Data.List.NonEmpty (NonEmpty)
import Control.Concurrent.Async

-- TODO: exception hierarchy

data UIException = forall e. Exception e => UIException e

deriving instance Show UIException

instance Exception UIException where
  displayException (UIException e) = "Exception in UI thread: " ++ displayException e

data SimException = forall e. Exception e => SimException e

deriving instance Show SimException

instance Exception SimException where
  displayException (SimException e) = "Exception in simulation thread: " ++ displayException e


main :: HasCallStack => IO ()
main = withAppThread $ \th -> Control.Monad.forever do
  me <- runMainMenu th
  -- todo: proper crash screen
  case me of
    Quit -> liftIO exitSuccess
    NewGame -> pure ()
  ge <- runGame th
  case ge of
    ExitDesktop -> liftIO exitSuccess
    ExitMainMenu -> pure ()

runMainMenu :: AppThread -> IO MenuExit
runMainMenu th =
  withMainMenu th (atomically . waitBrickThread) >>= \case
    Left e -> throwIO $ UIException e
    Right mme ->
      maybe (throwIO . UIException $ userError "No reason given for menu exit") pure mme

runGame :: AppThread -> IO GameExit
runGame th = withGameUI th \bth uq ->
  withSFThread uq sf \sfth -> do
    firstExit <- race (atomically $ waitBrickThread bth) (atomically $ waitSFThread sfth)
    case firstExit of
      Left (Right (Just ge)) -> pure ge
      Left (Right Nothing) -> throwIO . UIException $ userError "No reason given for game exit"
      Left (Left e) -> throwIO (UIException e)
      Right (Just e) -> throwIO (SimException e)
      Right Nothing -> throwIO . SimException $ userError "Simulation SF terminated early"
  where
    sf :: SF Identity (Event (NonEmpty UserInput)) SimState
    sf = arr (fmap sconcat) >>> simSF
