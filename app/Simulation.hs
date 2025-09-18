{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
module Simulation (SimState(..), simSF, ObjectIdentifier(..), VisibleObject(..)) where

import FRP.Yampa
import Input
import Data.Bifunctor (bimap)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Lens.Operators

-- | Tells the UI thread how an object should be drawn.
data ObjectIdentifier = Player

data VisibleObject = VisibleObject
  { position :: !(Int, Int) -- ^ bottom-left corner
  , occupies :: Set (Int, Int) -- ^ Cells occupied by the object, relative to the bottom-left corner.
  , objIdentifier :: ObjectIdentifier
  }

data SimState = SimState
  { objects :: [VisibleObject]
  , camera :: !(Int, Int) -- ^ the 'camera' cell should always be visible, even
                          -- if not necessarily in the centre of the screen
  }

inputDecayHalfLife :: Floating a => a
inputDecayHalfLife = 0.5

inputChangeRate :: Floating a => a
inputChangeRate = 10

-- | Magnitude decays exponentially given no further input,
-- with half-life inputDecayHalfLife.
-- Given new input, changes to the new direction vector with
-- velocity inputChangeRate.
smoothInput :: forall v k. (VectorSpace v k, Real k, Floating k) => SF (Event v) v
smoothInput = decayFrom zeroVector
  where
    decayFrom v0 = switch
      (proc e -> do
        v <- decay v0 (2 * inputDecayHalfLife) -< ()
        returnA -< (v, fmap (v,) e))
      (uncurry moveTo)

    moveTo v0 v0' =
      let a = inputChangeRate *^ (v0' ^-^ v0)
          timeToCollide = norm a / inputChangeRate
       in switch
        (proc e -> do
          c <- snapAfter (realToFrac timeToCollide) -< ()
          v <- arr (v0 ^+^) <<< integral -< a
          returnA -< (v, case e of
            -- if there is new input, halt and start moving to that input
            Event v' -> Event (Right (v, v'))
            -- if there is no new input but we have arrived at v0',
            -- start exponential decay
            NoEvent -> Left v <$ c
            )
        )
        (either decayFrom (uncurry moveTo))

-- | Exponential decay with magnitude 1/k^t at time t
--
-- >>> embed (decay (1.0,2.0,3.0) 2.0) (deltaEncode 1 [(), ()])
-- [(1.0,2.0,3.0),(0.5,1.0,1.5)]
decay :: (Floating k, VectorSpace v k) => v -> k -> SF a v
decay v0 k = proc _ -> do
  t <- time -< ()
  rec
    v <- arr (uncurry (*^)) <<< second (iPre v0) -< (exp (log k * (- realToFrac t)), v)
  returnA -< v

-- | Ensures vector magnitude is at most 1.
--
-- >>> clampMagnitude (0.1 :: Double, 0.1 :: Double)
-- (0.1,0.1)
--
-- >>> clampMagnitude (1.0 :: Double, 1.0 :: Double)
-- (0.7071067811865475,0.7071067811865475)
clampMagnitude :: (Num k, Ord k, VectorSpace v k) => v -> v
clampMagnitude v = v ^/ max 1 (norm v)

simSF :: SF (Event UserInput) SimState
simSF = proc u -> do
  vInput <- smoothInput -< clampMagnitude . (^. moveVector) <$> u

  pos <- integral -< vInput
  let playerObject = VisibleObject
        { position = bimap round round pos
        , occupies = Set.fromList [(0,0), (0,1), (0,2), (1,0), (1,1), (1,2)] -- ^ 2x 3y rectangle
        , objIdentifier = Player
        }

  returnA -< SimState
    { objects = [playerObject]
    , camera = (0, 0)
    }
