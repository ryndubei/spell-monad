{-# LANGUAGE Arrows #-}
module Simulation (SimState(..), simSF) where

import FRP.Yampa
import Input

data SimState = SimState

simSF :: SF (Event UserInput) SimState
simSF = proc u -> do
  returnA -< SimState
