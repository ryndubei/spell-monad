{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module App (UserClock(..), UIThreadException(..), AppThread, withAppThread, DisplayClock, displayClock, AppResult(..), appRh) where

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
import FRP.Rhine.Clock.Except
import Control.Monad
import Control.Monad.Log
import Data.Text (Text)

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

instance (MonadIO m, MonadError AppResult m, MonadReader AppThread m) => Clock m UserClock where
  type Time UserClock = UTCTime
  type Tag UserClock = UserInput
  initClock UserClock = do
    t0 <- liftIO getCurrentTime
    pure . (,t0) $ constM do
      AppThread{resultAsync, userInput} <- ask
      mer <- liftIO $ poll resultAsync
      case mer of
        Nothing -> do
          u <- liftIO $ readMVar userInput
          t <- liftIO getCurrentTime
          pure (t, u)
        Just (Left e) -> liftIO $ throwIO (UIThreadException e)
        Just (Right r) -> throwError r

instance GetClockProxy UserClock

-- | Ticks whenever a display input is consumed
data DisplayedClock = DisplayedClock

instance (MonadIO m, MonadReader AppThread m) => Clock m DisplayedClock where
  type Time DisplayedClock = UTCTime
  type Tag DisplayedClock = ()
  initClock DisplayedClock = do
    t0 <- liftIO getCurrentTime
    pure . (,t0) $ constM do
      AppThread{gameState, gameStateChan} <- ask
      s <- liftIO $ takeMVar gameState
      liftIO $ writeBChan gameStateChan s
      t <- liftIO getCurrentTime
      pure (t, ())

instance GetClockProxy DisplayedClock

-- | Ticks whenever a new display input is needed
newtype DisplayClock = DisplayClock (forall m. MonadIO m => DisplayClock' m)
type DisplayClock' m = CatchClock (Single (ExceptT () m) UTCTime () ()) () DisplayedClock

displayClock :: DisplayClock
displayClock = DisplayClock $ CatchClock
  (Single () (liftIO getCurrentTime) ())
  (const DisplayedClock)

whichever :: Either a a -> a
whichever (Right a) = a
whichever (Left a) = a

instance forall m. (MonadIO m, MonadReader AppThread m) => Clock m DisplayClock where
  type Time DisplayClock = UTCTime
  type Tag DisplayClock = ()
  initClock (DisplayClock cl) = do
    (k, t0) <- initClock (cl :: DisplayClock' m)
    pure (k >>> arr (second whichever), t0)

instance GetClockProxy DisplayClock

appOut :: MonadReader AppThread m => ClSF m UserClock () UserInput
appOut = tagS

appIn :: (MonadIO m, MonadReader AppThread m, MonadLog (WithSeverity Text) m) => ClSF m DisplayClock SimState ()
appIn = arrMCl \s -> do
  AppThread{gameState} <- ask
  written <- liftIO $ tryPutMVar gameState s
  unless written $ logWarning "appIn failed to write to MVar (race condition?)"

appRh :: (MonadIO m, MonadReader AppThread m, MonadLog (WithSeverity Text) m, MonadError AppResult m) => Rhine m (DisplayClock `SeqClock` UserClock) SimState UserInput
appRh = appIn @@ displayClock >-- trivialResamplingBuffer --> appOut @@ UserClock
