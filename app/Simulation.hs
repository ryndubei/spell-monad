module Simulation (SimState(..), simSF) where

import FRP.BearRiver
import Control.Monad.State.Strict
import Input

data SimState = SimState

simSF :: SF (State SimState) (Event UserInput) ()
simSF = arr (const ()) -- TODO
