{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App (UserClock(..), UIThreadException(..), AppThread, withAppThread, DisplayClock(..), AppResult(..), appRh) where

import Graphics.Vty
import FRP.Rhine.Clock
import FRP.Rhine
import Control.Concurrent
import Data.Time.Clock
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Concurrent.Async
import Control.Exception
import Input
import Simulation
import Brick.BChan
import Control.Monad
import Control.Monad.Log
import Data.Text (Text)
import qualified Data.Automaton.Trans.Except as A
import Control.Monad.Reader
import Data.Foldable (traverse_)

stateChanSize :: Int
stateChanSize = 16

data UserClock = UserClock

data AppState

data AppThread = AppThread
  { resultAsync :: !(Async AppResult)
  , gameState :: !(MVar SimState)
  , gameStateChan :: !(BChan SimState)
  , userInput :: !(MVar UserInput)
  }

withAppThread :: (AppThread -> IO a) -> IO a
withAppThread f = do
  userInput <- newEmptyMVar
  gameState <- newEmptyMVar
  gameStateChan <- newBChan stateChanSize
  withAsync
    do undefined -- app
    \resultAsync -> f AppThread{..}

newtype AppResult = AppResult (AppState, Vty)

newtype UIThreadException = UIThreadException SomeException deriving Show

instance Exception UIThreadException

instance MonadIO m => Clock (ReaderT AppThread (ExceptT AppResult m)) UserClock where
  type Time UserClock = UTCTime
  type Tag UserClock = UserInput
  initClock UserClock = do
    t0 <- liftIO getCurrentTime
    pure . (,t0) $ constM do
      AppThread{resultAsync, userInput} <- ask
      mer <- liftIO $ poll resultAsync
      case mer of
        Nothing -> do
          u <- liftIO $ takeMVar userInput
          t <- liftIO getCurrentTime
          pure (t, u)
        Just (Left e) -> liftIO $ throwIO (UIThreadException e)
        Just (Right r) -> throwError r

instance GetClockProxy UserClock

-- | Ticks whenever a display input is needed
data DisplayClock = DisplayClock

instance MonadIO m => Clock (ReaderT AppThread m) DisplayClock where
  type Time DisplayClock = UTCTime
  type Tag DisplayClock = ()
  initClock DisplayClock = do
    t0 <- liftIO getCurrentTime
    pure . (, t0) $ safely do
      AppThread{gameState, gameStateChan} <- A.once_ ask
      -- flush MVar in case it is non-empty
      A.once_ $ do
        a <- liftIO $ tryTakeMVar gameState
        traverse_ (liftIO . writeBChan gameStateChan) a 
      A.try $ A.forever do
        -- announce input is needed
        A.step $ const do
          t <- liftIO getCurrentTime
          pure ((t, ()), ())
        -- block until input is given
        A.once_ $ do
          a <- liftIO $ takeMVar gameState
          liftIO $ writeBChan gameStateChan a 

instance GetClockProxy DisplayClock

appOut :: Monad m => ClSF m UserClock () UserInput
appOut = tagS

appIn :: (MonadIO m, MonadLog (WithSeverity Text) m) => ClSF (ReaderT AppThread m) DisplayClock SimState ()
appIn = arrMCl \s -> do
  AppThread{gameState} <- ask
  written <- liftIO $ tryPutMVar gameState s
  unless written $ logWarning "appIn failed to write to MVar (race condition?)"

appRh :: (MonadIO m, MonadLog (WithSeverity Text) m) => Rhine (ReaderT AppThread (ExceptT AppResult m)) (DisplayClock `SeqClock` UserClock) SimState UserInput
appRh = appIn @@ DisplayClock >-- trivialResamplingBuffer --> appOut @@ UserClock

-- or possibly
-- runApp :: forall m cl. (MonadIO m, (forall u. u ~ In cl), (forall d. d ~ Out cl)) => Rhine m cl UserInput SimState -> m ()