{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
module Simulation.Objects.Geometry (Geometry, adHocGeometry, pollGeometry) where

import Linear
import Data.QuadTree

newtype Geometry = Geometry (QuadTree Bool)

adHocGeometry :: Geometry
adHocGeometry = Geometry . fuse $
  fill (Rect (V2 (-1) 0) (V2 (10) (-10))) True $ pure False

-- TODO this is very slow
pollGeometry :: V2 Double -> Geometry -> Bool
pollGeometry v (Geometry q) = Data.QuadTree.lookup (fmap toRational v) q
