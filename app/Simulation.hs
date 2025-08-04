module Simulation (SimState(..), simRhine) where

import FRP.Rhine
import Input (UserInput)

data SimState = SimState

simRhine :: MonadIO m => SimState -> Rhine m Busy (Maybe UserInput) SimState
simRhine s0 = constMCl (pure s0) @@ Busy
