{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE PatternSynonyms #-}
module Simulation.Coordinates
  ( V
  , Phase
  , phaseToV
  , vToPhase
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
import Numeric.IEEE
import Control.Exception
import Data.Word

-- | Continuous 2D coordinate type
type V = Complex Double

-- | An angle from the x-axis.
type Phase = Double

-- | Convert a phase to a unit vector.
phaseToV :: Phase -> V
phaseToV theta = cis theta

pattern AmbiguousPhase :: Word64
pattern AmbiguousPhase = 0xDEADBEEF

vToPhase :: V -> Phase
vToPhase = assert (maxNaNPayload (nan :: Double) >= AmbiguousPhase) $ \v ->
  if nearZero v
    -- 'phase' defaults to 0, which would likely lead to subtler bugs than a
    -- marked NaN value.
    then nanWithPayload AmbiguousPhase
    else phase v

-- TODO: why isn't this defined in 'linear' (yet the instance is defined for quaternions)?
instance Metric (Complex) where
  {-# INLINE dot #-}
  dot (r1 :+ i1) (r2 :+ i2) = (r1 * r2) + (i1 * i2)
