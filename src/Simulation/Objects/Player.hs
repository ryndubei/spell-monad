{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
module Simulation.Objects.Player (playerObj, module Simulation.Objects.Player.Types) where

import Simulation.Objects
import FRP.BearRiver
import Simulation.Input
import Control.Lens
import Control.Monad.Fix
import Simulation.Coordinates
import App.Thread.SF
import Control.Monad
import Data.Maybe
import Linear.Epsilon
import qualified Data.Sequence as Seq
import Simulation.Objects.Player.Types

gravityAcceleration :: Fractional a => a
gravityAcceleration = 9.8

playerBaseVelocity :: Fractional a => a
playerBaseVelocity = 10

playerJumpVelocity :: Fractional a => a
playerJumpVelocity = 10

playerManaRegenRate :: Fractional a => a
playerManaRegenRate = 5

playerObj :: forall e m r. (Monad m, Monoid (ObjsInput e m r)) => Object e m r Player
playerObj = loopPre playerMaxMana $ proc ((playerIn, objsOutput), lastPlayerMana) -> do

  pos' <- (fix $ \k (pos1, vy) ->
    switch (generaliseSF $ fallingMovement pos1 vy) (\pos2 -> switch (generaliseSF $ groundedMovement pos2) k))
    (zeroVector, 0) -< playerIn

  -- Default facing direction is the direction of movement.
  let (vx, _) = simInput playerIn ^. moveVector
      movingLeft = vx < 0
      defaultFacingDirection = if movingLeft then V2 (-1) 0 else V2 1 0
  facingDirectionOverride <- arr (>>= \x -> guard (not (nearZero x)) >> pure (normalize x)) <<< hold Nothing -< overrideFacingDirection playerIn
  playerFacingDirection <- arr (uncurry fromMaybe) -< (defaultFacingDirection, facingDirectionOverride)

  -- Player mana is regenerated once a second at the regen rate (instead of continuously)
  -- Can't get more sophisticated than this unless I add a MonadFix constraint,
  -- or rewrite spellInterpreter to rely less on current mana state.
  manaRegenEvent <- repeatedly 1 () -< ()
  let newMana1 = event lastPlayerMana id $ min playerMaxMana (lastPlayerMana + playerManaRegenRate) <$ manaRegenEvent

  -- (objsInput, newMana2, replResponse, stdout) <- spellInterpreter -< (newMana1, NoEvent, playerIn, objsOutput)
  let stdout = undefined
      newMana2 = undefined
      objsInput = undefined

  let playerOutput = PlayerOutput
        { playerX = pos' ^. _x
        , playerY = pos' ^. _y
        , playerMana = newMana2
        , playerMaxMana
        , playerStdout = event mempty Seq.singleton stdout
        , playerFacingDirection
        }

  returnA -< ((playerOutput, objsInput), newMana2)
  where
    playerMaxMana = 100

    groundedMovement :: V -> SF Identity (ObjInput Player) (V, Event (V, Double))
    groundedMovement (V2 x0 _) = proc (PlayerInput{simInput = simInput@SimInput{simJump}}) -> do
        let (vx, _) = playerBaseVelocity *^ (simInput ^. moveVector)
        dx <- integral -< vx
        let pos' = V2 (x0 + dx) 0
        -- Horizontal velocity is fully determined by the user input, so we only
        -- return the vertical velocity.
        returnA -< (pos', (pos', playerJumpVelocity) <$ simJump)

    -- Accelerates downwards until both velocity and position would be negative,
    -- then switches back to grounded movement.
    fallingMovement :: V -> Double -> SF Identity (ObjInput Player) (V, Event V)
    fallingMovement (V2 x0 y0) vy0 = proc (PlayerInput{simInput}) -> do
      let (vx, _) = playerBaseVelocity *^ (simInput ^. moveVector)
      dx <- integral -< vx
      dvy <- integral -< gravityAcceleration
      let vy = vy0 - dvy
      dy <- integral -< vy
      rec
        pos <- iPre (V2 x0 y0) -< pos'
        let pos' = V2 (x0 + dx) (y0 + dy)
            grounded = pos' ^. _y <= 0 && (vy <= 0)
      returnA -< (pos', gate (Event pos) grounded)
