{-# LANGUAGE RecordWildCards #-}

module Simulation.Input (processInput, SimInput(..), moveVector) where

import Input
import FRP.Yampa
import Control.Lens

data SimInput = SimInput
  { simMoveX :: !Double
  , simMoveY :: !Double
  , simJump :: !(Event ())
  }

-- | norm (u ^. moveVector) = min 1 (norm u)
moveVector :: Lens' SimInput (Double, Double)
moveVector = lens (\SimInput{simMoveX, simMoveY} -> (simMoveX, simMoveY)) (\u (x,y) -> u {simMoveX = x, simMoveY = y})

-- | Turns a stream of discrete input events into a continuous signal.
processInput :: SF (Event UserInput) SimInput
processInput = proc u -> do
  (simMoveX, simMoveY) <- smoothInput -< clampMagnitude . (^. userInputMoveVector) <$> u
  let simJump = tagWith () . filterE id $ fmap jump u

  returnA -< SimInput{..}

inputDecayHalfLife :: Floating a => a
inputDecayHalfLife = 1

inputChangeRate :: Floating a => a
inputChangeRate = 2

-- | Ensures vector magnitude is at most 1.
--
-- >>> clampMagnitude (0.1 :: Double, 0.1 :: Double)
-- (0.1,0.1)
--
-- >>> clampMagnitude (1.0 :: Double, 1.0 :: Double)
-- (0.7071067811865475,0.7071067811865475)
clampMagnitude :: (Num k, Ord k, VectorSpace v k) => v -> v
clampMagnitude v = v ^/ max 1 (norm v)

-- | Magnitude decays exponentially given no further input,
-- with half-life inputDecayHalfLife.
-- Given new input, changes to the new direction vector with
-- velocity inputChangeRate.
--
-- >>> embed smoothInput (deltaEncode 1 [Event (1.0,2.0), NoEvent, NoEvent, NoEvent, Event (3.0, -5.0), NoEvent, NoEvent])
-- [(0.0,0.0),(0.8944271909999159,1.7888543819998317),(1.7888543819998317,3.5777087639996634),(0.8944271909999159,1.7888543819998317),(0.22360679774997896,0.4472135954999579),(1.131821322293319,-1.3346804466384952),(2.0400358468366595,-3.116574488776948)]
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
          udisp = if disp /= zeroVector then normalize disp else disp
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
