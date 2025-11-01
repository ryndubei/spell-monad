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
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.IO.Class
import System.Exit
import Control.Concurrent.Async
import Control.Monad.Cont
import Control.Monad.Trans
import App.UI.LoadingScreen
import Input
import Control.Arrow
import Data.These
import Data.Semigroup
import Data.Bifunctor (bimap)
import FRP.Yampa

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
runGame th = evalContT do
  rth <- ContT withReplThread

  lift $ withLoadingScreen th $ atomically $ replStatus rth >>= check . not . sameStatus Initialising

  sfth <- ContT $ withSFThread id $ generaliseSF simSF'
  -- Assumption: sfth does not block indefinitely until input is given
  -- (ideally does not block at all on first output)
  s0 <- lift . atomically $ takeSFThread sfth

  bth <- ContT $ withGameUI th rth s0

  sfToBrickTh <- ContT . withAsync . forever $ atomically do
    b <- isBrickQueueEmpty bth
    check b
    takeSFThread sfth >>= sendBrickEvent bth . Left
    flushSFThreadEvents sfth >>= sendBrickEvent bth . Right
  lift $ link sfToBrickTh

  brickToSfTh <- ContT . withAsync . forever $ atomically do
    aui <- takeBrickThread bth
    case aui of
      GameInput ui -> sendSFThread sfth (This ui)
      TermStdin _ -> pure () -- TODO
  lift $ link brickToSfTh

  replToSfTh <- ContT . withAsync . forever $ atomically do
    req <- takeInterpretRequest rth
    sendSFThread sfth (That (Last req))
  lift $ link replToSfTh

  firstExit <- lift $ race (atomically $ waitBrickThread bth) (atomically $ waitSFThread sfth)
  lift $ case firstExit of
    Left (Right (Just ge)) -> pure ge
    Left (Right Nothing) -> throwIO . UIException $ userError "No reason given for game exit"
    Left (Left e) -> throwIO (UIException e)
    Right (Just e) -> throwIO (SimException e)
    Right Nothing -> throwIO . SimException $ userError "Simulation SF terminated early"
  where
    simSF' :: SF (Event SFIn) (SimState, Event SimEvent)
    simSF' = arr (event (NoEvent, NoEvent) (fromThese NoEvent NoEvent . bimap Event (Event . getLast))) >>> simSF

type SFIn = These UserInput (Last InterpretRequest)
