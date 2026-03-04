{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MultiWayIf #-}
module Simulation.Coordinates
  ( V
  , Phase
  , phaseToV
  , vToPhase
  , LineSegment(..)
  , plankIntegral
  , nearestPoint
  , hang
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
import Data.Word
import FRP.BearRiver

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
vToPhase v =
  if magnitude v == 0
    -- 'phase' defaults to 0, which would likely lead to subtler bugs than a
    -- marked NaN value.
    then nanWithPayload AmbiguousPhase
    else phase v

-- TODO: why isn't this defined in 'linear' (yet the instance is defined for quaternions)?
instance Metric (Complex) where
  {-# INLINE dot #-}
  dot (r1 :+ i1) (r2 :+ i2) = (r1 * r2) + (i1 * i2)

data LineSegment = LineSegment { segmentEdge :: !V, segmentLine :: !V }

-- | (\v -> prel v v0 v1) . (\t -> lerp t v0 v1) = id
-- if not (nearZero (v1 - v0))
--
-- Measure of 'how far along' our projection is on a line segment. 
prel :: (Fractional a, Metric f) => f a -> f a -> f a -> a
prel v v0 v1 = ((v ^-^ v0) `dot` (v1 ^-^ v0)) / quadrance (v1 ^-^ v0)  

nearestPoint :: LineSegment -> V -> V
nearestPoint LineSegment{segmentEdge, segmentLine} v = p
  where
    s = prel v segmentEdge (segmentEdge + segmentLine)
    s' = min 1 $ max 0 $ s
    p = lerp s' segmentEdge (segmentEdge + segmentLine)

-- | Attaches V to the "overhead" point on the line segment:
-- fixes the x-coordinate whenever possible, and takes on
-- the y-coordinate of the point on the line segment with the
-- closest x.
--
-- Drawing of the shape of the domain:
-- 
--             ______
--           /
--          /
--         /
-- _______/
--
-- >>> hang (LineSegment 0 (1 :+ 1)) (0.5 :+ 0)
-- 0.5 :+ 0.5
--
-- >>> hang (LineSegment 0 (1 :+ 1)) (0.5 :+ 0.1)
-- 0.5 :+ 0.5
--
-- >>> hang (LineSegment 0 (1 :+ 1)) ((-0.5) :+ (-1))
-- (-0.5) :+ 0.0
--
-- >>> hang (LineSegment 0 ((-1) :+ 1)) ((-1.5) :+ (-1))
-- (-1.5) :+ 1.0
hang :: LineSegment -> V -> V
hang LineSegment{segmentEdge, segmentLine = x1 :+ y1} v = v'
  where
    (x :+ y) = v - segmentEdge
    y' = if | 0 <= x1 && x  <= 0  -> 0
            | 0 <= x1 && x1 <= x  -> y1
            | x1 < 0  && x  <= x1 -> y1
            | x1 < 0  && 0  <= x  -> 0
            | nearZero x1 -> y
            | otherwise -> (y1/x1) * x
    v' = segmentEdge + (x :+ y')

-- | Clamps a 'V' to either the "overhead" or the nearest point on the line segment
-- then integrates the position by the velocity along the segment until
-- it exits the segment.
-- The final value is the new position that lies outside the segment.
--
-- ('walking the plank')
--
-- Undefined on a 0-length line segment, and the closer we get to 0, the more undefined it is.
--
-- >>> embed (runTask (plankIntegral (LineSegment 0 1) 0)) (0, [(1, Just 0.1), (1, Just 0.1), (1, Just (-0.1)), (1, Just (-0.2))])
-- [Left (0.0 :+ 0.0),Left (0.1 :+ 0.0),Left (0.2 :+ 0.0),Left (0.1 :+ 0.0),Right ((-0.1) :+ 0.0)]
plankIntegral :: Monad m => LineSegment -> V -> Task Double V m V
plankIntegral ls@LineSegment{segmentEdge, segmentLine} v = mkTask' $ proc vt -> do
    t <- arr (t0 +) <<< integral -< vt / magnitude segmentLine
    let v' = lerp t segmentEdge (segmentEdge + segmentLine)
    returnA -< if 0 <= t && t <= 1 then Left v' else Right v'
    where
      v0 = hang ls v
      t0 = min 1 $ max 0 $ prel v0 segmentEdge (segmentEdge + segmentLine)
