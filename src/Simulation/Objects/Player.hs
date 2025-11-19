{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE Arrows #-}
module Simulation.Objects.Player (ObjInput(..), ObjOutput(..)) where

import Simulation.Objects
import Simulation.Objects.Firebolts
import FRP.BearRiver
import Simulation.Input
import Spell
import Control.Lens
import Control.Monad.Fix

data instance ObjInput (Player e m r) = PlayerInput
  { simInput :: SimInput -- ^ Continuous user input
  , replInput :: Event (Maybe (SpellT e m r, e -> r))
    -- ^ Nothing: cancel any current input
    --
    -- Just: cancel and interpret new input
  }
data instance ObjOutput (Player e m r) = PlayerOutput
  { playerX :: !Double
  , playerY :: !Double
  , playerHealth :: !Rational
  , playerMana :: !Rational
  , replResponse :: Event r
  , spawnFirebolt :: Event (ObjOutput FireboltsObject)
  }

playerBaseVelocity :: Fractional a => a
playerBaseVelocity = 10

playerJumpVelocity :: Fractional a => a
playerJumpVelocity = 10

playerObj :: forall e m r. Monad m => Object e m r (Player e m r)
playerObj = proc u -> do
  
  (x', y') <- (fix $ \k pos1 -> switch (fallingMovement pos1) (\pos2 -> switch (groundedMovement pos2) k)) (0, 0) -< undefined

  returnA -< undefined
  -- rec
  --   (prevX, prevY) <- iPre zeroVector -< (playerX, playerY)

  --   let grounded = cellHasTerrain (prevX, prevY) <= 0
  --       jumpImpulse = gate (tag (simJump u) playerJumpVelocity) onGround

  --   hitImpulse <- arr (uncurry tag) <<< (edge *** iPre 0) -< (onGround, -vy)
  --   vy <- impulseIntegral -< (if onGround then 0 else (-gravity), mergeBy (+) jumpImpulse hitImpulse)
  --   (playerX, playerY) <- trapezoidIntegral -< (vx, vy)

  -- returnA -< undefined
  where
    -- Movement is clamped to the surface, as long as the normal points in the
    -- same direction as the gravity vector. Allowed to move up vertical
    -- discontinuities of up to 1 unit. Player is allowed to jump.
    groundedMovement :: (Double, Double) -> SF m (ObjInput (Player e m r), ObjsOutput e m r) ((Double, Double), Event (Double, Double))
    groundedMovement pos0 = loopPre pos0 $
      proc ((PlayerInput{simInput}, _), (x,y)) -> do
        let (vx, _) = playerBaseVelocity *^ (simInput ^. moveVector)
            -- Normal of the nearest valid surface (if any)
            -- Can be ambiguous in two directions:
            -- the left direction is the limit from the left,
            -- the right direction is the limit from the right.
            surfaceNormal :: Maybe ((Double,Double), (Double, Double))
            surfaceNormal = undefined
            pos' = undefined
        returnA -< ((pos', undefined), pos')

    -- Accelerates in the direction of the gravity vector.
    fallingMovement :: (Double, Double) -> SF m (ObjInput (Player e m r), ObjsOutput e m r) ((Double, Double), Event (Double, Double))
    fallingMovement pos0 = undefined