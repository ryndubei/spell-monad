{-# LANGUAGE PartialTypeSignatures #-}
module Simulation (SimState(..), simRhine) where

import FRP.Rhine
import Input (UserInput)

data SimState = SimState

simRhine :: forall m. MonadIO m => SimState -> Rhine m _ UserInput SimState
simRhine s0 = constMCl (pure SimState) @@ ioClock @m (waitClock :: Millisecond 50)
