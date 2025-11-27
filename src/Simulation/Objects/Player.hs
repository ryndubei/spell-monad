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
import Data.Maybe
import Linear.Epsilon
import Simulation.Objects.Player.Types
import Simulation.Objects.SpellInterpreter
import Control.Monad.Trans.MSF.State
import Control.Monad.Trans.MSF.Reader
import Data.Either
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Control.Applicative

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

  returnA -< ((playerOutput, objsInput), playerMana)
  where
    playerMaxMana = 100

    actionMgr' = readerS $
      arr (\(mana, (dt, a)) -> (dt, (mana, a)))
      >>> runStateS (runReaderS $ actionMgr mempty
      >>> arr (mconcat . fst . partitionEithers . fmap snd . IntMap.toList))

    actionMgr sfs = pSwitchB sfs (first (first (iPre empty)) >>> second (iPre mempty) >>> arr (uncurry monitorActionMgr)) \sfs' (as, dones) ->
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
