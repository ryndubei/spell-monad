{-# LANGUAGE RecordWildCards #-}
module App.UI (AppState, initialAppState, theapp) where

import Input
import Brick
import Control.Concurrent
import Simulation
import Control.Monad.State.Strict

data AppState = AppState
  { consoleShown :: Bool
  , simState :: SimState
  }

initialAppState :: AppState
initialAppState = undefined

-- identity sim state should be empty, interpreted by app as a loading screen

theapp :: MVar UserInput -> App AppState SimState a
theapp userInput = App { .. }
  where
    appDraw AppState{ .. }= [ console | consoleShown ] ++ [ game simState ]
    appChooseCursor = undefined
    appHandleEvent (AppEvent s) = modify' (\a -> a{simState = s})
    appHandleEvent e = undefined
    appStartEvent = undefined
    appAttrMap = undefined

console :: Widget a
console = undefined

game :: SimState -> Widget a
game _ = undefined