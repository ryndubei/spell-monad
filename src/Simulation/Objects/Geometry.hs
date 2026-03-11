{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
module Simulation.Objects.Geometry
  ( Geometry
  , adHocGeometry
  , pollGeometry
  , printGeometry
  , fallingMovement
  , groundedMovement
  , geomMovement
  , geomMovementWithJump
  ) where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Maybe
import Data.Char
import Simulation.Coordinates
import FRP.BearRiver
import Data.Fixed
import Control.Lens.Operators
import GHC.Float
import Simulation.Util
import Control.Monad.Fix
import Data.Either
import Simulation.Objects.Geometry.Hitbox

-- | Each point is a 0.5x1-unit 'block' of static terrain.
data Geometry = Geometry { blocks :: !(U.Vector Bool), offsetX :: !Int, offsetY :: !Int, rowLength :: !Int }

geometryFromRows
  :: Int -- ^ horizontal translation
  -> Int -- ^ vertical translation
  -> V.Vector (V.Vector Bool)
  -> Geometry
geometryFromRows offsetX offsetY rows = Geometry
  { blocks = U.generate sz \i ->
    let (col, row) = quotRem i maxRowLen
     in fromMaybe False $ (rows V.!? col) >>= (V.!? row)
  , offsetX
  , offsetY
  , rowLength = maxRowLen
  }
  where
    maxRowLen = V.maximum (fmap V.length rows)
    sz = if V.null rows
      then 0
      else V.length rows * maxRowLen

-- | Convert a multiline string into a Geometry, taking
-- any whitespace chars as empty space. 
--
geometryFromString
  :: Int -- ^ horizontal translation
  -> Int -- ^ vertical translation
  -> String
  -> Geometry
geometryFromString offsetX offsetY str = geometryFromRows offsetX offsetY vs
  where
    -- reversing since we want the bottom-left corner to be 0, not top-left
    vs = V.reverse . V.fromList $ map (fmap (not . isSpace) . V.fromList) $ lines str

adHocGeometry :: Geometry
adHocGeometry = geometryFromString (-5) (-5) $ unlines
  [ "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  , "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                                                                                                                                   xx"
  , "xx                                xxxxxxxxxx                                                                                                         xx"
  , "xx                               xxxxxxxxxxx                       xxxxxxxxxxxxxxxxxxx                                                               xx"
  , "xx                              xxxxxxxxxxxx       xxxxxxx         xxxxxxxxxxxxxxxxxxx                                                               xx"
  , "xx                             xxxxxxxxxxxxx       xxxxxxx                                                                                           xx"
  , "xx                            xxxxxxxxxxxxxx                                                                                                         xx"
  , "xx                           xxxxxxxxxxxxxxx                                                                                                         xx"
  , "xx                          xxxxxxxxxxxxxxxx                                                                                                         xx"
  , "xx                         xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  , "xxxxxxxxxxxx              xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  , "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  , "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  ]

-- | Print the blocks of a Geometry.
--
-- geometryFromString (offsetX g) (offsetY g) $ printGeometry g = g
printGeometry :: Geometry -> String
printGeometry geo@Geometry{ blocks, rowLength, offsetX, offsetY } =
  unlines . V.toList . V.reverse . V.map V.toList $ V.generate nLines \col ->
    V.generate rowLength \row -> if testGeometry ((fromIntegral $ row + offsetX) / 2 :+ (fromIntegral $ col + offsetY)) geo then '█' else ' '
  where
    nLines = U.length blocks `div` rowLength

type Cell = Complex Int

vToCell :: V -> Cell
vToCell (x :+ y) = ix :+ iy
  where
    ix = floor (x * 2)
    iy = floor y

-- | Given a Cell, return the hitbox encompassing the cell.
cellToHitbox :: Cell -> (V, Hitbox)
cellToHitbox (x :+ y) = (v, hb)
  where
    v = ((int2Double x / 2)) :+ (int2Double y)
    hb = Hitbox (0.5 :+ 1)

-- | Important! defaults to True when out of bounds
--
-- (conceptually, the levels take place in a cave)
testGeometryCell :: Cell -> Geometry -> Bool
testGeometryCell (ix1 :+ iy1) Geometry{rowLength, blocks, offsetX, offsetY} =
  if xOob || yOob
    then True
    else case blocks U.!? i of
      Just b -> b
      Nothing -> True
  where
    yOob = iy < 0
    xOob = ix < 0 || ix >= rowLength
    ix = ix1 - offsetX
    iy = iy1 - offsetY
    i = iy*rowLength + ix

testGeometry :: V -> Geometry -> Bool
testGeometry = testGeometryCell . vToCell

-- | Is the given point non-empty?
pollGeometry :: V -> Geometry -> Bool
pollGeometry v geom = case geometryLineSegment v geom of
  Nothing -> testGeometry v geom
  Just ls -> v ^. _i <= hang ls v ^. _i

{-# COMPLETE T, F #-}
pattern F :: Bool
pattern F = False
pattern T :: Bool
pattern T = True

lowerCellEdge :: V -> V
lowerCellEdge (x :+ y) = int2Double (floor (x * 2)) / 2 :+ int2Double (floor y)

-- | Vertical jumps of near-1 unit are interpreted as a slope.
geometryLineSegment :: V -> Geometry -> Maybe LineSegment
geometryLineSegment v geom =
  let x0 :+ y0 = lowerCellEdge v
      (r1, r2, r3) = mapt3 (mapt3 (`testGeometry` geom)) (adjacentCells v)
  in
    case (r1,r2,r3) of
      -- cell is occupied
      (   (_, _, _)
        , (_, T, _)
        , (_, _, _)
        ) -> Nothing
      -- not grounded
      (   (_, _, _)
        , (_, F, _)
        , (_, F, _)
        ) -> Nothing
      -- trough
      (   (F, F, F)
        , (T, F, T)
        , (_, T, _)
        ) -> if mod' (realPart v) 0.5 < 0.25
              -- left slope
              then Just $ LineSegment (x0 :+ (y0 + 1)) (0.25 :+ (-1))
              -- right slope
              else Just $ LineSegment ((x0 + 0.25) :+ y0) (0.25 :+ 1)
      -- only right slope
      (   (_, F, F)
        , (_, F, T)
        , (_, T, _)
        ) -> Just $ LineSegment (x0 :+ y0) (0.5 :+ 1)
      -- only left slope
      (   (F, F, _)
        , (T, F, _)
        , (_, T, _)
        ) -> Just $ LineSegment (x0 :+ (y0 + 1)) (0.5 :+ (-1))
      -- no slope
      (   (_, _, _)
        , (_, F, _)
        , (_, T, _)
        ) -> Just $ LineSegment (x0 :+ y0) 0.5
    
adjacentCells
  :: V
  -> ( (V,V,V)
     , (V,V,V)
     , (V,V,V)
     )
adjacentCells v = (,,) r1 r2 r3
  where
    r1 = (v + ((-0.5) :+ 1)   , v + (0 :+ 1)   , v + (0.5 :+ 1))
    r2 = (v + (-0.5)          , v              , v + 0.5   )
    r3 = (v + ((-0.5) :+ (-1)), v + (0 :+ (-1)), v + (0.5 :+ (-1)))

mapt3 :: (a -> b) -> (a, a, a) -> (b, b, b)
mapt3 f (x,y,z) = (f x, f y, f z)

-- | A small number that is not 'nearZero'.
-- Should still be usable as a step size at 10,000 units.
eps :: Double
eps = 1e-5

plankIntegralWithWalls :: Monad m => Geometry -> LineSegment -> V -> Task Double V m V
plankIntegralWithWalls geom ls pos0 = do
  pos1 <- plankIntegral ls pos0
  -- pos1 will be close to the edge of at least two cells.
  -- We therefore test with a slight offset into the direction
  -- that we have moved in.
  let perturbation = eps *^ signum (pos1 - pos0)
      inWall = testGeometry (pos1 + perturbation) geom
  -- if we have tried to enter (collided with) a wall,
  -- clamp back to the line segment
  if inWall
    then do
      let pos' = nearestPoint ls pos1
      shriek pos' -- to avoid an infinite loop
      plankIntegralWithWalls geom ls pos'
    else pure pos1

-- | If there is no surface where we are standing, wait until there is one for up to
-- the given amount of time while remaining grounded on a flat 'phantom' surface.
coyoteTime :: Monad m => DTime -> Geometry -> V -> Task Double V m (V, Maybe LineSegment)
coyoteTime grace geom pos0 = mkTask $ generaliseSF $ proc vsurf -> do
  pos <- runTask_ $ pos0 & fix
    (\k pos -> do
      pos' <- plankIntegralWithWalls geom (phantomLineSegment pos) pos
      k pos'
    ) -< vsurf
  let mls = geometryLineSegment pos geom
  u <- predicateTimeout grace -< isJust mls
  returnA -< (pos,) $ if | Just ls <- mls -> Event (Just ls)
                         | Event () <- u -> Event Nothing
                         | otherwise -> NoEvent 
  where
    -- A line segment attached to the lower edge of the cell we are in.
    phantomLineSegment pos = LineSegment{segmentEdge = lowerCellEdge pos, segmentLine = 0.5 }

-- | Grounded movement of a point particle.
groundedMovement
  :: Monad m
  => V -- ^ initial position
  -> DTime -- ^ grace period/'coyote time'
  -> Geometry
  -> Task
      Double -- ^ surface velocity
      V -- ^ position
      m
      V -- ^ final position, no longer grounded
groundedMovement pos0 grace geom
  | Nothing <- geometryLineSegment pos0 geom = pure pos0
  | Just ls0 <- geometryLineSegment pos0 geom = do
      (pos0, ls0) & fix \k1 (pos, ls) -> do

        pos1 <- plankIntegralWithWalls geom ls pos

        -- coyote time
        (pos2, mls) <- coyoteTime grace geom pos1       

        shriek pos2
        case mls of
          -- new line segment
          Just ls' -> k1 (pos2, ls')
          -- falling now
          Nothing -> pure pos2

fallingMovement
  :: Monad m
  => V -- ^ initial position
  -> V -- ^ initial velocity
  -> V -- ^ gravity vector
  -> Geometry
  -> Task
      V -- ^ extra velocity
      V -- ^ position
      m
      V -- ^ final position
fallingMovement s0 v0 grav geom = do
  (s2, s1) <- mkTask $ proc vplus -> do
    dv <- integral -< grav
    ds <- integral -< v0 + dv + vplus
    let s = s0 + ds
        grounded = pollGeometry s geom 
    groundedEvt <- edge <<< initially False -< grounded
    s1 <- iPre s0 -< s
    returnA -< (s, groundedEvt `tag` s1)
  let ds = s2 - s1
      s = findCollision geom LineSegment{segmentEdge = s1, segmentLine = ds}
  shriek s
  pure s

-- | Clamp movement of a point particle to the geometry.
geomMovement
  :: Monad m
  => V -- ^ initial position
  -> Double -- ^ magnitude of gravity vector
  -> DTime -- ^ coyote time
  -> Geometry
  -> SF m Double V 
geomMovement pos0 gravMag grace geom = runTask_ $
  pos0 & fix \k pos -> do
    if isJust (geometryLineSegment pos geom)
      then do
        pos1 <- groundedMovement pos grace geom
        pos2 <- mkTask' . (arr (\d -> d :+ 0) >>>) . runTask $ fallingMovement pos1 0 (-(0 :+ gravMag)) geom
        k pos2
      else do
        pos' <- mkTask' . (arr (\d -> d :+ 0) >>>) . runTask $ fallingMovement pos  0 (-(0 :+ gravMag)) geom
        k pos'

-- | Like 'geomMovement', but allows jumping in the opposite
-- direction of gravity.
geomMovementWithJump
  :: Monad m
  => V -- ^ initial position
  -> Double -- ^ magnitude of gravity vector
  -> DTime -- ^ coyote time
  -> Double -- ^ jump impulse
  -> Geometry
  -> SF m (Double, Event ()) V 
geomMovementWithJump pos0 gravMag grace jumpMag geom =
  switch (proc (vsurf, jump) -> do
    pos <- geomMovement pos0 gravMag grace geom -< vsurf
    let hasGround = pollGeometry (pos - (0 :+ eps)) geom
    returnA -< (pos, jump `gate` hasGround)
  ) \pos1 () -> switch
    (proc (vsurf, _) -> do
      epos <- runTask (fallingMovement pos1 (0 :+ jumpMag) (-(0 :+ gravMag)) geom) -< (vsurf :+ 0)
      let pos = either id id epos
          done = if isRight epos then Event () else NoEvent
      returnA -< (pos, done)
    ) \pos2 () -> geomMovementWithJump pos2 gravMag grace jumpMag geom

-- | Trim the line segment up to the nearest collision with the geometry,
-- given that there is up to 1 collision between the start and the end.
findCollision :: Geometry -> LineSegment -> V
findCollision geom ls0@LineSegment{segmentEdge = se0, segmentLine = sl0}
  -- Binary search
  | pollGeometry se0 geom && pollGeometry (se0 + sl0) geom = se0
  | not (pollGeometry se0 geom) && not (pollGeometry (se0 + sl0) geom) = se0 + sl0
  | otherwise =
      let be = pollGeometry se0 geom
       in ls0 & fix \k LineSegment{segmentEdge = se, segmentLine = sl} ->
            if nearZero sl
              then if be == pollGeometry (se + sl) geom
                then se + sl
                else se
              else
                let b = pollGeometry (se + (sl / 2)) geom
                    (se', sl') = if be == b then (se + sl / 2, sl / 2) else (se, sl / 2)
                 in k LineSegment{segmentEdge = se', segmentLine = sl'}

-- | Event source with a single occurence as soon as possible after 'tEv'
-- has elapsed since we last had a True value.
--
-- If the sampling interval is wide enough for it to be ambiguous whether
-- we have timed out or not, optimistically assumes that we have not
-- timed out.
predicateTimeout :: Monad m => DTime -> SF m Bool (Event ())
predicateTimeout tEv = switch (proc b -> do
  u <- iEdge False -< not b
  returnA -< (NoEvent, u)
  ) \_ () -> switch
    (proc b -> do
      u <- edge -< b
      to <- snapAfter tEv -< ()
      returnA -< (to, u)
    )
    \_ () -> predicateTimeout tEv
