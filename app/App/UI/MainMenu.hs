{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module App.UI.MainMenu (MenuExit(..), newMainMenu) where

import App.Thread
import FRP.Rhine
import GameData
import Control.Monad.Trans.Resource
import Data.Void
import Brick
import Control.Monad.Schedule.Class

data MenuExit = Quit | Load Level

newtype MainMenuState = MainMenuState
  { exitCategory :: MenuExit
  }

initialMainMenuState :: MainMenuState
initialMainMenuState = MainMenuState
  { exitCategory = Quit }

newMainMenu :: forall s m. (MonadResource m, MonadSchedule m) => AppThread s -> m (Rhine (ExceptT MenuExit m) _ () ())
newMainMenu th = do
  bth <- newBrickThread th theapp initialMainMenuState
  let cl :: HoistClock (ExceptT MainMenuState m) (ExceptT MenuExit m) (BrickExitClock MainMenuState s)
      cl = HoistClock (BrickExitClock bth) (withExceptT exitCategory)
  pure $ (arr id @@ Never) |@| (tagS @@ cl) @>>^ absurd

theapp :: App MainMenuState Void ()
theapp = undefined
