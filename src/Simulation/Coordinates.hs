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
  , lineSegmentIntersection
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

lineSegmentIntersection :: LineSegment -> LineSegment -> Maybe (Double, Double, V)
{-
z_1 + r_1*ell_1 = z2 + r_2*ell_2

If ell_2 /= 0 (safe assumption):

r_2 = (z_1 + r_1*ell_1 - z_2) / ell_2

looking for a real r_1 such that r_2 is real:

Im((z_1 + r_1*ell_1 - z_2) / ell_2) = 0

Im((z_1 + r_1*ell_1 - z_2)*conj_ell_2) = 0

Im((z_1 - z_2)*conj_ell_2) + r_1*Im(ell_1*conj_ell_2) = 0

If Im(ell_1*conj_ell_2) /= 0:

r_1 = Im((z_1 - z_2)*conj_ell_2) / Im(ell_1*conj_ell_2)
-}
lineSegmentIntersection
  ls1@LineSegment{segmentEdge = z1, segmentLine = l1}
  ls2@LineSegment{segmentEdge = z2, segmentLine = l2}
  | 0 <= r1 && r1 <= 1 && 0 <= r2 && r2 <= 1 = Just (r1, r2, (z1 + r1 *^ l1))

  -- Corner cases of corner cases. Not strictly necessary.
  | z1 == z2 = Just (0, 0, z1)
  | colinear && magnitude l2 == 0 && magnitude l1 /= 0 = (\(a,b,c) -> (b,a,c)) <$> lineSegmentIntersection ls2 ls1
  {-
  r_2 = (z_1 + r_1*ell_1 - z_2) / ell_2

  r_2 = s*r_1 + ((z_1 - z_2)/ell_2)

  Im((z_1 - z_2)/ell_2)) = 0
  -}

  | colinear =
      let
          s = magnitude l1 / magnitude l2
          z = (z1 - z2) / l2
          -- equality check should be fine
          haveSolutions = imagPart z == 0
          diff = realPart z
          -- r_2 = s*r_1 + diff
          -- minimum valid r1
          selectedR1 = max 0 (-diff / s)
       in if haveSolutions && (abs diff <= 1) && not (isInfinite selectedR1) && not (isNaN selectedR1)
        then Just (selectedR1, s*selectedR1 + diff, z1 + selectedR1 *^ l1)
        else Nothing
  | otherwise = Nothing
  where
    l2' = conjugate l2
    colinear = isInfinite r1 || isNaN r1 -- division by zero
    r1 = imagPart ((z2 - z1) * l2') / imagPart (l1 * l2')
    (r2 :+ _) = (z1 + r1 *^ l1 - z2) / l2
