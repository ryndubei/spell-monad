{-# OPTIONS_GHC -Wno-orphans #-}
module Simulation.Coordinates
  ( V
  , module Data.Complex
  , module Linear.Metric
  , module Linear.Quaternion
  , module Linear.Epsilon
  , module Linear.Vector
  ) where

import Data.Complex
import Linear.Metric
import Linear.Quaternion (Complicated(..))
import Linear.Epsilon
import Linear.Vector

-- | Continuous 2D coordinate type
type V = Complex Double

-- TODO: why isn't this defined in 'linear' (yet the instance is defined for quaternions)?
instance Metric (Complex) where
  {-# INLINE dot #-}
  dot (r1 :+ i1) (r2 :+ i2) = (r1 * r2) + (i1 * i2)
