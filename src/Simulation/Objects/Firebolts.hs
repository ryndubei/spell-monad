{-# OPTIONS_GHC -Wno-orphans #-}
module Simulation.Objects.Firebolts (FireboltState(..), ObjInput(..), ObjOutput(..)) where

import Simulation.Objects
import Data.IntMap.Strict (IntMap)

data instance ObjInput FireboltsObject = FireboltUnits
data FireboltState = FireboltState { fireboltX :: !Double, fireboltY :: !Double, fireboltRadius :: !Double }
newtype instance ObjOutput FireboltsObject = FireboltOutputs (IntMap FireboltState)
