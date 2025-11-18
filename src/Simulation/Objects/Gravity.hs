{-# OPTIONS_GHC -Wno-orphans #-}
module Simulation.Objects.Gravity (ObjInput(..), ObjOutput(..)) where

import Simulation.Objects

data instance ObjInput Gravity = GravityUnit
data instance ObjOutput Gravity = GravityOutput { gravityX :: !Double, gravityY :: !Double }