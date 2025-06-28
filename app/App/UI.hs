module App.UI (AppState, initialAppState, theapp) where

import Input
import Brick
import Control.Concurrent
import Simulation
import Data.Text (Text)

data AppState

initialAppState :: AppState
initialAppState = undefined

theapp :: MVar UserInput -> App AppState SimState Text
theapp = undefined
