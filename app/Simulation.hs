{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
module Simulation (SimState(..), simSF, ObjectIdentifier(..), VisibleObject(..)) where

import FRP.Yampa
import Input
import Control.Lens.Operators
import Data.Text (Text)
import qualified Data.Text as T

-- | Tells the UI thread how an object should be drawn.
data ObjectIdentifier = Player deriving (Eq, Ord, Show)

data VisibleObject = VisibleObject
  { position :: !(Double, Double) -- ^ centre
  , radius :: Double -- ^ anything further away from 'position' should not be drawn
  , sdf :: (Double, Double) -> Double -- ^ signed distance function to be drawn, relative to position
  , objIdentifier :: ObjectIdentifier
  }

data SimState = SimState
  { objects :: [VisibleObject]
  , camera :: !(Double, Double) -- ^ the 'camera' position should always be visible, even
                          -- if not necessarily in the centre of the screen
  }

inputDecayHalfLife :: Floating a => a
inputDecayHalfLife = 1

inputChangeRate :: Floating a => a
inputChangeRate = 100

-- | Magnitude decays exponentially given no further input,
-- with half-life inputDecayHalfLife.
-- Given new input, changes to the new direction vector with
-- velocity inputChangeRate.
--
-- >>> embed smoothInput (deltaEncode 1 [Event (1.0,2.0), NoEvent, NoEvent, Event (3.0, -5.0), NoEvent, NoEvent])
-- [(0.0,0.0),(4.47213595499958,8.94427190999916),(2.23606797749979,4.47213595499958),(0.5590169943749475,1.118033988749895),(4.26476810109556,-8.16999102925253),(2.13238405054778,-4.084995514626265)]
smoothInput :: forall v k. (Eq v, VectorSpace v k, Real k, Floating k) => SF (Event v) v
smoothInput = decayFrom zeroVector
  where
    decayFrom v0 = switch
      (proc e -> do
        v <- decay v0 (exp (log 2 / inputDecayHalfLife)) -< ()
        returnA -< (v, fmap (v,) e))
      (\(a,b) -> const NoEvent >=- moveTo a b)

    moveTo v0 v0' =
      let disp = v0' ^-^ v0
          udisp = if disp == zeroVector then disp else normalize disp
          -- t = s / v
          timeToCollide = norm disp / inputChangeRate
          a = inputChangeRate *^ udisp
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
        ((const NoEvent >=-) . either decayFrom (uncurry moveTo))

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

simSF :: SF (Event UserInput) (SimState, Event Text)
simSF = proc u -> do
  vInput <- smoothInput -< (10 *^) . clampMagnitude . (^. moveVector) <$> u

  pos <- trapezoidIntegral -< vInput

  returnA -< (SimState
    { objects = [playerObject pos]
    , camera = (0, 0)
    }, fmap (T.pack . show) u)
  where
    playerObject pos = VisibleObject
        { position = pos
        , radius = 3
        -- square of diameter 2
        , sdf = \(x,y) -> max (abs x - 1) (abs y - 1)
        , objIdentifier = Player
        }
