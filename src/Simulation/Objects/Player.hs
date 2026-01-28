{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Simulation.Objects.Player (playerObj, module Simulation.Objects.Player.Types) where

import Simulation.Objects
import FRP.BearRiver
import Simulation.Input
import Control.Lens
import Control.Monad.Fix
import Simulation.Coordinates
import App.Thread.SF
import Control.Monad
import Linear.Epsilon
import Simulation.Objects.Player.Types
import Simulation.Objects.SpellInterpreter
import Control.Monad.Trans.MSF.State
import Control.Monad.Trans.MSF.Reader
import Data.Either
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Control.Applicative
import Simulation.Objects.TargetSelector
import Data.Bool

gravityAcceleration :: Fractional a => a
gravityAcceleration = 9.8

playerBaseVelocity :: Fractional a => a
playerBaseVelocity = 10

playerJumpVelocity :: Fractional a => a
playerJumpVelocity = 10

playerManaRegenRate :: Fractional a => a
playerManaRegenRate = 5

-- | Unwrap a continuous Maybe signal, defaulting to the last non-Nothing value.
holdJust :: forall m a. Monad m => a -> SF m (Maybe a) a
holdJust a0 = arr maybeToEvent >>> hold a0

playerObj :: forall e m r. (Monad m, Monoid (ObjsInput e m r)) => Object e m r Player
playerObj = loopPre playerMaxMana $ proc ((playerIn1, objsOutput), lastPlayerMana) -> do


  -- while TargetSelector is visible, route all user input to it
  let (playerIn, targetSelectorIn) = if visible (targetSelector objsOutput)
      then (playerIn1{simInput = mempty}, mempty{targetSelectorInput = simInput playerIn1})
      else (playerIn1, mempty)

  pos' <- (fix $ \k (pos1, vy) ->
    switch (generaliseSF $ fallingMovement pos1 vy) (\_ pos2 -> switch (generaliseSF $ groundedMovement pos2) $ const k))
    (0, 0) -< playerIn

  -- Default facing direction is the direction of movement.
  let (V2 vx _) = simInput playerIn ^. moveVector
  movingRight <- arr (\vx -> (vx > 0) <$ guard (not $ nearZero vx) ) -< vx
  playerFacingDirection <- holdJust (V2 1 0) -< bool (V2 (-1) 0) (V2 1 0) <$> movingRight

  -- TODO: continuous regen?
  manaRegenEvent <- repeatedly 1 () -< ()
  let newMana1 = event lastPlayerMana id $ min playerMaxMana (lastPlayerMana + playerManaRegenRate) <$ manaRegenEvent

  (playerMana, objsInput) <- generaliseSF actionMgr' -< (newMana1, (actions playerIn, (NoEvent, objsOutput)))

  let playerOutput = PlayerOutput
        { playerX = pos' ^. _x
        , playerY = pos' ^. _y
        , playerMana
        , playerMaxMana
        , playerFacingDirection
        }

  returnA -< ((playerOutput, objsInput <> mempty{targetSelector = targetSelectorIn}), playerMana)
  where
    playerMaxMana = 100

    actionMgr' = readerS $
      arr (\(mana, (dt, a)) -> (dt, (mana, a)))
      >>> runStateS (runReaderS $ actionMgr mempty
      >>> arr (mconcat . fst . partitionEithers . fmap snd . IntMap.toList))

    actionMgr sfs = dpSwitchB sfs (first (first (iPre empty)) >>> second (iPre mempty) >>> arr (uncurry monitorActionMgr)) \sfs' (as, dones) ->
      let as' = map ((arr snd >>>) . runTask . unAction) as
          k = if IntSet.null (IntMap.keysSet sfs') then 0 else IntSet.findMax (IntMap.keysSet sfs') + 1
          as'' = IntMap.fromList $ zip [k ..] as'
          sfs'' = IntMap.withoutKeys sfs' dones
          sfs''' = as'' <> sfs''
       in actionMgr sfs'''

    monitorActionMgr (as, _) im =
      let dones = IntMap.keysSet (IntMap.filter isRight im)
          donese = if IntSet.null dones then NoEvent else Event dones
       in mergeBy (<>) ((, mempty) <$> as) ((mempty,) <$> donese)

    groundedMovement :: V -> SF Identity (ObjInput Player) (V, Event (V, Double))
    groundedMovement (V2 x0 _) = proc (PlayerInput{simInput = simInput@SimInput{simJump}}) -> do
        let (V2 vx _) = playerBaseVelocity * (simInput ^. moveVector)
        dx <- integral -< vx
        let pos' = V2 (x0 + dx) 0
        -- Horizontal velocity is fully determined by the user input, so we only
        -- return the vertical velocity.
        returnA -< (pos', (pos', playerJumpVelocity) <$ simJump)

    -- Accelerates downwards until both velocity and position would be negative,
    -- then returns an event to switch back to grounded movement.
    fallingMovement :: V -> Double -> SF Identity (ObjInput Player) (V, Event V)
    fallingMovement (V2 x0 y0) vy0 = proc (PlayerInput{simInput}) -> do
      let (V2 vx _) = playerBaseVelocity * (simInput ^. moveVector)
      dx <- integral -< vx
      dvy <- integral -< gravityAcceleration
      let vy = vy0 - dvy
      dy <- integral -< vy
      rec
        pos <- iPre (V2 x0 y0) -< pos'
        let pos' = V2 (x0 + dx) (y0 + dy)
            grounded = pos' ^. _y <= 0 && (vy <= 0)
      returnA -< (pos', gate (Event pos) grounded)
