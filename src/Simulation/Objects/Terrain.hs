{-# OPTIONS_GHC -Wno-orphans #-}
module Simulation.Objects.Terrain (ObjInput(..), ObjOutput(..)) where

import Simulation.Objects

data instance ObjInput Terrain = TerrainUnit
newtype instance ObjOutput Terrain = TerrainOutput { cellHasTerrain :: (Int, Int) -> Bool }
