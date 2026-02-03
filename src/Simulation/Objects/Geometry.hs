{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
module Simulation.Objects.Geometry (Geometry, adHocGeometry) where

import Linear
import Data.QuadTree

newtype Geometry = Geometry (QuadTree Bool)

adHocGeometry :: Geometry
adHocGeometry = Geometry $
  fill (Rect (V2 (-1) 0) (V2 (10) (-10))) True $ pure False

pollGeometry :: V2 Double -> Geometry -> Bool
pollGeometry v (Geometry qd) = Data.QuadTree.lookup (fmap toRational v) qd