{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Simulation.Objects.Geometry (Geometry, adHocGeometry, pollGeometry, printGeometry) where

import Linear
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Maybe
import Data.Char

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
    V.generate rowLength \row -> if pollGeometry (V2 (fromIntegral $ row + offsetX) (fromIntegral $ col + offsetY)) geo then '█' else ' '
  where
    nLines = U.length blocks `div` rowLength

-- | Important! defaults to True when out of bounds
--
-- (conceptually, the levels take place in a cave)
pollGeometry :: V2 Double -> Geometry -> Bool
pollGeometry (V2 x y) Geometry{rowLength, blocks, offsetX, offsetY} =
  if xOob || yOob
    then True
    else case blocks U.!? i of
      Just b -> b
      Nothing -> True
  where
    yOob = iy < 0
    xOob = ix < 0 || ix >= rowLength
    ix = floor (x * 2) - offsetX
    iy = floor y - offsetY
    i = iy*rowLength + ix
