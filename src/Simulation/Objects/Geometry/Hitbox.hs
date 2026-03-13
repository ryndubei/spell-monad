{-# LANGUAGE NoArrows #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
module Simulation.Objects.Geometry.Hitbox
  ( Hitbox(..)
  , areHitboxesColliding
  , penetrationVector
  , hitboxCollision
  , rasterise
  , hitboxSweep
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
import Control.Monad.Logic ((>>-))
import Control.Arrow ((&&&))
import Data.Ord
import qualified Data.Set as Set

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
-- The cells will appear in sorted order.
rasterise :: LineSegment -> [Complex Int]
rasterise LineSegment{segmentEdge = z, segmentLine = l} =
  nubMergeSortedOn (\(x :+ y) -> (x,y))
    do
      guard (not . nearZero $ realPart l)
      x <- [floor x0 + 1 .. floor x1]
      -- y such that z + r*l = x + i*y where r is real
      let y = (int2Double x * imagPart l + imagPart (z * conjugate l)) / realPart l
      pure (x :+ floor y)
    $ (fmap floor leftEdge :) $ do
      guard (not . nearZero $ imagPart l)
      -- if the line points down, then the second list will be in reversed order
      if signum (imagPart l) == -signum (realPart l)
        then do
          Down y <- [Down (floor y0 + 1) .. Down (floor y1)]
          let x = (int2Double y * realPart l - imagPart (z * conjugate l)) / imagPart l
          pure (floor x :+ y)
        else do
          y <- [(floor y0 + 1) .. (floor y1)]
          let x = (int2Double y * realPart l - imagPart (z * conjugate l)) / imagPart l
          pure (floor x :+ y)
  where
    leftEdge = if realPart z < realPart (z + l) then z else (z + l)
    x0 :+ y0 = mzipWith min z (z + l) 
    x1 :+ y1 = mzipWith max z (z + l)

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
nubSortedOn _ [] = []
nubSortedOn _ [x] = [x]
nubSortedOn f (x:y:ys) = if (f x) == (f y) then nubSortedOn f (y:ys) else x : nubSortedOn f (y:ys)

yGivenX :: LineSegment -> Double -> Maybe Double
yGivenX LineSegment{segmentEdge = z, segmentLine = l} x = do
  let y = (x * imagPart l + imagPart (z * conjugate l)) / realPart l
      r = realPart (((x :+ y) - z) / l)
  guard $ isReal y
  guard $ isReal r && r >= 0 && r <= 1
  pure y

xGivenY :: LineSegment -> Double -> Maybe Double
xGivenY LineSegment{segmentEdge = z, segmentLine = l} y = do
  let x = (y * realPart l - imagPart (z * conjugate l)) / imagPart l
      r = realPart (((x :+ y) - z) / l)
  guard $ isReal x
  guard $ isReal r && r >= 0 && r <= 1
  pure x

-- | not infinite, not NaN
isReal :: Double -> Bool
isReal x = not (isInfinite x) && not (isNaN x)

visualise :: [Complex Int] -> String
visualise [] = mempty
visualise cells0@(Set.fromList . map (\(x :+ y) -> (x,y)) -> cells) = unlines . (++ ["", replicate (yidxlen) ' ' ++ show minx]) $ do
  Down y <- [Down maxy .. Down miny]
  pure $ paddedTo yidxlen (show y) ++ ": " ++ do
    x <- [minx .. maxx]
    case Set.member (x,y) cells of
      True -> pure '#'
      False -> pure '.'
  where
    paddedTo l str = if length str < l then str ++ replicate (l - length str) ' ' else str
    yidxlen = maximum $ map (length . show) [miny .. maxy]
    minx = minimum $ map (^. _e) cells0
    maxx = maximum $ map (^. _e) cells0
    miny = minimum $ map (^. _i) cells0
    maxy = maximum $ map (^. _i) cells0

-- | Given one side of a parallelogram and the displacement to the parallel side,
-- find all cells covered by the parallelogram.
-- Cells will be in sorted order.
rasteriseParallelogram :: LineSegment -> V -> [Complex Int]
rasteriseParallelogram ls0@LineSegment{segmentEdge = z, segmentLine = l1} l2 =
  nubMergeSortedOn (\(x :+ y) -> (x,y)) ((fmap floor leftCorner) : cells1) cells2
  where
    leftCorner = minimumBy (compare `on` realPart) [z, z + l1, z + l2, z + l1 + l2]
    -- TODO edge case: bottom corner not present in general,
    -- ignore until this becomes a problem

    x0 :+ y0 = mzipWith min z $ mzipWith min (z + l1) $ mzipWith min (z + l2) (z + l1 + l2)
    x1 :+ y1 = mzipWith max z $ mzipWith max (z + l1) $ mzipWith max (z + l2) (z + l1 + l2)

    ls1 = ls0{segmentLine = l2}
    ls2 = ls0{segmentEdge = z + l2}
    ls3 = LineSegment{segmentEdge = z + l1, segmentLine = l2}

    cells1 = do
      x <- [floor x0 + 1 .. floor x1]
      -- The allowed ys form a closed interval. These are the boundary points.
      let x' = int2Double x
          -- Extrema candidates
          yb0 = yGivenX ls0 x'
          yb1 = yGivenX ls1 x'
          yb2 = yGivenX ls2 x'
          yb3 = yGivenX ls3 x'
          yInterval = fmap (minimum &&& maximum) . NE.nonEmpty $ catMaybes [yb0, yb1, yb2, yb3]
      case yInterval of
        Nothing -> mzero
        Just (ymin, ymax) -> do
          y <- [floor ymin .. floor ymax]
          pure (x :+ y)

    cells2 = do
        y <- [floor y0 + 1 .. floor y1]
        pure y
        -- >>- is >>= but in breadth-first order for lists
        -- (ensures it's sorted by x and not by y)
      >>- \y -> do
        let y' = int2Double y
            xb0 = xGivenY ls0 y'
            xb1 = xGivenY ls1 y'
            xb2 = xGivenY ls2 y'
            xb3 = xGivenY ls3 y'
            xInterval = fmap (minimum &&& maximum) . NE.nonEmpty $ catMaybes [xb0, xb1, xb2, xb3]
        case xInterval of
          Nothing -> mzero
          Just (xmin, xmax) -> do
            x <- [floor xmin .. floor xmax]
            pure (x :+ y)

-- | The cells covered by the hitbox, given the path the edge of its diagonal.
-- The list is sorted.
hitboxSweep :: (LineSegment, Hitbox) -> [Complex Int]
hitboxSweep (LineSegment{segmentEdge = z, segmentLine = l}, Hitbox{hitboxDiagonal = dx :+ dy}) =
  nubMergeSortedOn tup
    (nubMergeSortedOn tup (rasteriseParallelogram face1 l) (rasteriseParallelogram face2 l))
    (nubMergeSortedOn tup (rasteriseParallelogram face3 l) (rasteriseParallelogram face4 l))
  where
    tup (x :+ y) = (x,y)
    face1 = LineSegment{segmentEdge = z, segmentLine = dx :+ 0}
    face2 = LineSegment{segmentEdge = z, segmentLine = 0 :+ dy}
    face3 = LineSegment{segmentEdge = z + (dx :+ dy), segmentLine = 0 :+ (-dy)}
    face4 = LineSegment{segmentEdge = z + (dx :+ dy), segmentLine = (-dx) :+ 0}
