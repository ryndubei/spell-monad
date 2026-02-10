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
import FRP.Yampa
import Control.Monad.Fix
import Data.Function
import Spell.Eval
import qualified Data.Sequence as Seq
import Data.Time

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

  sfth <- ContT $ withSFThread id simSF
  -- Assumption: sfth does not block indefinitely until input is given
  -- (ideally does not block at all on first output)
  s0 <- lift . atomically $ takeSFThread sfth
  
  t0 <- liftIO $ getCurrentTime

  bth <- ContT $ withGameUI th rth t0 s0

  sfToBrickTh <- ContT . withAsync $ forever do
    responses <- atomically do
      b <- isBrickQueueEmpty bth
      check b
      takeSFThread sfth >>= sendBrickEvent bth . Left
      flushSFThreadEvents sfth >>= \es -> do
        let responses = mapM_ interpretResponse es
        -- TODO: split into a type not exposing interpretResponse to the brick thread
        sendBrickEvent bth $ Right es
        pure responses
    atomically responses
  lift $ link sfToBrickTh

  brickToSfTh <- ContT . withAsync . forever $ atomically do
    aui <- takeBrickThread bth
    case aui of
      GameInput ui -> sendSFThread sfth (mempty { gameInput = Event ui } )
      TermStdin c -> sendSFThread sfth (mempty { termStdin = Event $ Seq.singleton c })
  lift $ link brickToSfTh

  -- tail-recursive so fix should be ok
  replToSfTh <- ContT $ withAsync $ Nothing & fix \k mreq -> do
    case mreq of
      -- we are already monitoring a request
      Just req -> do
        mreq2 <- atomically do
          orElse
            do
              isInterpretRequestValid req >>= check . not
              sendSFThread sfth (mempty { interpretRequest = Event Nothing })
              pure Nothing
            do
              req2 <- takeInterpretRequest rth
              sendSFThread sfth (mempty { interpretRequest = Event (Just req2)})
              pure (Just req2)
        k mreq2
      -- we have no request to monitor
      Nothing -> do
        req <- atomically do
          req <- takeInterpretRequest rth
          sendSFThread sfth (mempty { interpretRequest = Event (Just req)} )
          pure req
        k (Just req)

  lift $ link replToSfTh

  firstExit <- lift $ race (atomically $ waitBrickThread bth) (atomically $ waitSFThread sfth)
  lift $ case firstExit of
    Left (Right (Just ge)) -> pure ge
    Left (Right Nothing) -> throwIO . UIException $ userError "No reason given for game exit"
    Left (Left e) -> throwIO (UIException e)
    Right (Just e) -> throwIO (SimException e)
    Right Nothing -> throwIO . SimException $ userError "Simulation SF terminated early"
