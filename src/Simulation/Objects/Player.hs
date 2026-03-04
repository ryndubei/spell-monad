{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
module Simulation.Objects.Player (playerObj, module Simulation.Objects.Player.Types) where

import Simulation.Objects
import FRP.BearRiver
import Simulation.Input
import Control.Lens
import Control.Monad.Fix
import Simulation.Coordinates
import Control.Monad
import Simulation.Objects.Player.Types
import qualified Data.IntMap.Strict as IntMap
import Simulation.Objects.TargetSelector
import Data.Bool
import Simulation.Util
import Simulation.Component
import Simulation.Objects.SpellInterpreter.Types
import Simulation.Objects.Geometry

gravityAcceleration :: Fractional a => a
gravityAcceleration = 9.8

playerCoyoteTime :: Fractional a => a
playerCoyoteTime = 0.1

playerBaseVelocity :: Fractional a => a
playerBaseVelocity = 10

playerJumpVelocity :: Fractional a => a
playerJumpVelocity = 10

playerManaRegenRate :: Fractional a => a
playerManaRegenRate = 5

-- | Unwrap a continuous Maybe signal, defaulting to the last non-Nothing value.
holdJust :: forall m a. Monad m => a -> SF m (Maybe a) a
holdJust a0 = arr maybeToEvent >>> hold a0

playerObj :: forall m. Monad m => Component Obj m PlayerInput PlayerOutput
playerObj = shrinkComponent . toComponent $ loopPre playerMaxMana $ proc ((playerIn1, objsOutput), lastPlayerMana) -> do


  -- while TargetSelector is visible, route all user input to it
  let (playerIn, targetSelectorIn) = if visible (unwrapOutputs objsOutput TargetSelector)
      then (playerIn1{simInput = mempty}, mempty{targetSelectorInput = simInput playerIn1})
      else (playerIn1, mempty)

  pos' <- generaliseSF $ playerMovement 0 -< playerIn

  -- Default facing direction is the direction of movement.
  let (vx :+ _) = simInput playerIn ^. moveVector
  movingRight <- arr (\vx -> (vx > 0) <$ guard (not $ nearZero vx) ) -< vx
  playerFacingDirection <- holdJust 0 -< bool (-pi) 0 <$> movingRight

  -- TODO: continuous regen?
  manaRegenEvent <- repeatedly 1 () -< ()
  let newMana1 = event lastPlayerMana id $ min playerMaxMana (lastPlayerMana + playerManaRegenRate) <$ manaRegenEvent

  let actions' = IntMap.fromList . map (\a@Action{actionTag} -> (actionId $ actionTag, unAction a)) <$> actions playerIn

  (playerMana, _, actionObjInputs) <- generaliseSF dynCollection -< (newMana1, actions', (NoEvent, objsOutput))

  let actionObjInputs' = mconcat $ IntMap.elems actionObjInputs

  let playerOutput = PlayerOutput
        { playerX = pos' ^. _e
        , playerY = pos' ^. _i
        , playerMana
        , playerMaxMana
        , playerFacingDirection
        }

  returnA -< ((playerOutput, actionObjInputs' <> WrappedInputs (\case TargetSelector -> targetSelectorIn; _ -> mempty)), playerMana)
  where
    playerMaxMana = 100

    playerMovement :: V -> SF Identity PlayerInput V
    playerMovement pos0 = proc PlayerInput{simInput} -> do
      pos <- geomMovementWithJump
        pos0
        gravityAcceleration
        playerCoyoteTime
        playerJumpVelocity
        adHocGeometry -< (playerBaseVelocity * simMoveX simInput, simJump simInput)
      returnA -< pos
