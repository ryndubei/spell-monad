{-# LANGUAGE NoArrows #-}
{-# LANGUAGE BlockArguments #-}
module Simulation.Objects.Geometry.Hitbox
  ( Hitbox(..)
  , areHitboxesColliding
  , penetrationVector
  , hitboxCollision
  , rasterise
  , rasteriseParallelogram
  ) where

import Simulation.Coordinates
import Control.Monad.Zip
import Control.Monad
import GHC.Float
import Data.Maybe
import Control.Lens
import qualified Data.List.NonEmpty as NE
import Data.Function
import Data.Foldable
import Data.List (sortOn, nub)
import Control.Monad.Logic ((>>-))

--------------------------------------------------------------------------------
-- Minkowski collision nonsense
--
-- References:
--   - https://www.spaderthomas.com/minkowski/
--     (http://archive.today/2026.03.06-185755/https://www.spaderthomas.com/minkowski/)
--   - https://blog.hamaluik.ca/posts/swept-aabb-collision-using-minkowski-difference/
--     (http://archive.today/2022.04.26-234250/https://blog.hamaluik.ca/posts/swept-aabb-collision-using-minkowski-difference/)
--
-- Retrieved: 2026-03-06
--------------------------------------------------------------------------------

newtype Hitbox = Hitbox { hitboxDiagonal :: V }

-- | Compute the Minkowski difference of two located hitboxes.
--
-- The hitbox edge will always be the lower corner, and it will extend to the
-- upper corner.
-- (both the real part and the imaginary part of the hitbox diagonal will be
-- positive)
minkowskiDifference :: (V, Hitbox) -> (V, Hitbox) -> (V, Hitbox)
minkowskiDifference
  ((x1 :+ y1), Hitbox{hitboxDiagonal = dx1 :+ dy1})
  ((x2 :+ y2), Hitbox{hitboxDiagonal = dx2 :+ dy2})
  = (pos, hb)
  where
    -- maximum difference in each direction
    hb = Hitbox ((abs dx1 + abs dx2) :+ (abs dy1 + abs dy2))
    -- bottom-left corner
    pos = (min x1 (x1 + dx1) - max x2 (x2 + dx2)) :+ (min y1 (y1 + dy1) - max y2 (y2 + dy2))

areHitboxesColliding :: (V, Hitbox) -> (V, Hitbox) -> Bool
areHitboxesColliding hb1 hb2 = signum x0 == -(signum (x0 + dx)) && signum y0 == -(signum (y0 + dy))
  where
    ((x0 :+ y0), Hitbox{hitboxDiagonal = dx :+ dy}) = minkowskiDifference hb1 hb2

-- | Return the minimal penetration vector of two located hitboxes.
-- If there is no collision, this is 0.
penetrationVector :: (V, Hitbox) -> (V, Hitbox) -> V
penetrationVector hb1 hb2
  | not (areHitboxesColliding hb1 hb2) = 0
  | otherwise = vec
  where
    ((x0 :+ y0), Hitbox{hitboxDiagonal = dx :+ dy}) = minkowskiDifference hb1 hb2
    vec = minimumBy (compare `on` magnitude) $
      [ x0 :+ 0
      , (x0 + dx) :+ 0
      , 0 :+ y0
      , 0 :+ (y0 + dy)
      ]

-- | Given hitboxes and line segments representing the paths of their corners,
-- computes their Minkowski difference and its path.
movingMinkowskiDifference
  :: (LineSegment, Hitbox)
  -> (LineSegment, Hitbox)
  -> (LineSegment, Hitbox)
movingMinkowskiDifference
  (LineSegment{segmentEdge = se1, segmentLine = sl1}, hb1)
  (LineSegment{segmentEdge = se2, segmentLine = sl2}, hb2)
  = let (v, hb) = minkowskiDifference (se1, hb1) (se2, hb)
     in (LineSegment{segmentEdge = v, segmentLine = sl1 - sl2}, hb)

hitboxLineSegments :: (V, Hitbox) -> (LineSegment, LineSegment, LineSegment, LineSegment)
hitboxLineSegments (corner, Hitbox{hitboxDiagonal = dx :+ dy}) =
  let vert1 = LineSegment{segmentEdge = corner, segmentLine = 0 :+ dy}
      vert2 = LineSegment{segmentEdge = corner + (dx :+ 0), segmentLine = 0 :+ dy}
      horiz1 = LineSegment{segmentEdge = corner, segmentLine = dx :+ 0}
      horiz2 = LineSegment{segmentEdge = corner + (0 :+ dy), segmentLine = dx :+ 0}
   in (horiz1, vert1, horiz2, vert2)

-- | Find the fraction of the nearest intersection of a line segment with the side of a hitbox.
hitboxIntersection :: (V, Hitbox) -> LineSegment -> Maybe Double
hitboxIntersection hb ls = closest
  where
    (i1,i2,i3,i4) = over each (lineSegmentIntersection ls) (hitboxLineSegments hb)
    closest = minimum . NE.map (^. _1) <$> NE.nonEmpty (catMaybes [i1,i2,i3,i4])

-- | Given two moving hitboxes, return the respective vectors that we can
-- move each hitbox by before the collision. The vectors will be shorter
-- than or equal to the line segments.
hitboxCollision :: (LineSegment, Hitbox) -> (LineSegment, Hitbox) -> (V, V)
hitboxCollision hb1@(LineSegment{segmentLine = sl1}, _) hb2@(LineSegment{segmentLine = sl2}, _) = (v1', v2')
  where
    (LineSegment{segmentEdge, segmentLine}, hb) = movingMinkowskiDifference hb1 hb2
    intersectionFraction =
      fromMaybe 1 $ hitboxIntersection (segmentEdge, hb) (LineSegment 0 (negate segmentLine))
    v1' = intersectionFraction *^ sl1
    v2' = intersectionFraction *^ sl2

-- | Rasterise a line segment as a list of cells.
-- The cells will appear in order of distance from the origin.
rasterise :: LineSegment -> [Complex Int]
rasterise LineSegment{segmentEdge = z, segmentLine = l} =
  possiblyReverse2 $ nubMergeSortedOn (\(x :+ y) -> (x,y))
    do
      guard (not . nearZero $ realPart l)
      x <- nub [floor x0, ceiling x0] ++ [ceiling x0 + 1 .. floor x1]
      -- y such that z + r*l = x + i*y where r is real
      let y = (int2Double x * imagPart l + imagPart (z * conjugate l)) / realPart l
      pure (x :+ floor y)
    $ possiblyReverse $ do
      guard (not . nearZero $ imagPart l)
      y <- nub [floor y0, ceiling y0] ++ [ceiling y0 + 1 .. floor y1]
      let x = (int2Double y * realPart l - imagPart (z * conjugate l)) / imagPart l
      pure (floor x :+ y)
  where
    x0 :+ y0 = mzipWith min z (z + l) 
    x1 :+ y1 = mzipWith max z (z + l)
    -- if the line points down, then the second list will be in reversed order
    possiblyReverse = if signum (imagPart l) == -signum (realPart l) then reverse else id
    -- if the line points backwards, then it should be reversed again to show up
    -- in the order of distance from the line segment origin
    possiblyReverse2 = if realPart l < 0 then reverse else id

nubMergeSortedOn :: Ord b => (a -> b) -> [a] -> [a] -> [a]
nubMergeSortedOn f xs ys = nubSortedOn f $ mergeSortedOn f xs ys

mergeSortedOn :: Ord b => (a -> b) -> [a] -> [a] -> [a]
mergeSortedOn _ xs [] = xs
mergeSortedOn _ [] ys = ys
mergeSortedOn f (x:xs) (y:ys) = case compare (f x) (f y) of
  EQ -> x : y : mergeSortedOn f xs ys
  LT -> x : mergeSortedOn f xs (y:ys)
  GT -> y : mergeSortedOn f (x:xs) ys

nubSortedOn :: Eq b => (a -> b) -> [a] -> [a]
nubSortedOn f [] = []
nubSortedOn f [x] = [x]
nubSortedOn f (x:y:ys) = if (f x) == (f y) then nubSortedOn f (y:ys) else x : nubSortedOn f (y:ys)

-- | Given one side of a parallelogram and the displacement to the parallel side,
-- find all cells covered by the parallelogram.
-- Cells will be in sorted order.
rasteriseParallelogram :: LineSegment -> V -> [Complex Int]
rasteriseParallelogram LineSegment{segmentEdge = z, segmentLine = l1} l2 =
  nubMergeSortedOn (\(x :+ y) -> (x,y)) cells1 cells2
  where
    x0 :+ y0 = mzipWith min z $ mzipWith min (z + l1) $ mzipWith min (z + l2) (z + l1 + l2)
    x1 :+ y1 = mzipWith max z $ mzipWith max (z + l1) $ mzipWith max (z + l2) (z + l1 + l2)

    cells1 = do
      -- We can choose which of l1,l2 is used for division.
      let (l1', l2') = if nearZero l1 then (l2, l1) else (l1, l2)
      guard $ not $ nearZero l1'
      x <- nub [floor x0, ceiling x0] ++ [ceiling x0 + 1 .. floor x1]
      -- The allowed ys form a closed interval. These are the boundary points.
      let x' = int2Double x
          y0Bnd = x' * imagPart l1' + imagPart (z * conjugate l1') / realPart l1'
          y1Bnd = x' * imagPart l1' + imagPart (z * conjugate l1') + imagPart (conjugate l1' * l2') / realPart l1'
      y <- [floor (min y0Bnd y1Bnd) .. floor (max y0Bnd y1Bnd)]
      pure (x :+ y)

    cells2 =
      let (l1', l2') = if nearZero l1 then (l2, l1) else (l1, l2)
          -- to ensure sort order
          possiblyReverse = if signum (imagPart l1') == -signum (realPart l1') then reverse else id
       in possiblyReverse $ do
          y <- nub [floor y0, ceiling y0] ++ [ceiling y0 + 1 .. floor y1]
          pure y
          -- >>- is >>= but in breadth-first order for lists
          >>- \y -> do
            let y' = int2Double y
                x0Bnd = y' * realPart l1' - imagPart (z * conjugate l1') / imagPart l1'
                x1Bnd = y' * realPart l1' - imagPart (z * conjugate l1') - imagPart(conjugate l1' * l2') / imagPart l1'
            x <- [floor (min x0Bnd x1Bnd) .. floor (max x0Bnd x1Bnd)]
            pure $ (x :+ y)
