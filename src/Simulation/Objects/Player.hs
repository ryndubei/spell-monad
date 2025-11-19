{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE Arrows #-}
module Simulation.Objects.Player (ObjInput(..), ObjOutput(..), playerObj) where

import Simulation.Objects
import Simulation.Objects.Firebolts
import FRP.BearRiver
import Simulation.Input
import Spell
import Control.Lens
import Control.Monad.Fix
import Simulation.Coordinates
import App.Thread.SF
import Data.Functor.Product
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Exception
import Control.Applicative
import Data.Sequence (Seq)

data instance ObjInput (Player e m r) = PlayerInput
  { simInput :: SimInput -- ^ Continuous user input
  , replInput :: Event (Maybe (SpellT e (MaybeT m `Product` ReaderT SomeException m) r, e -> r))
    -- ^ Nothing: cancel any current input
    --
    -- Just: cancel and interpret new input
  }
data instance ObjOutput (Player e m r) = PlayerOutput
  { playerX :: !Double
  , playerY :: !Double
  , playerMana :: !Rational
  , replOutput :: Seq Char
  , replResponse :: Event r
  }

instance Semigroup (ObjInput (Player e m r)) where
  (<>) p1 p2 = PlayerInput { simInput = simInput p1 <> simInput p2, replInput = replInput p2 <|> replInput p1}

instance Monoid (ObjInput (Player e m r)) where
  mempty = PlayerInput { simInput = mempty, replInput = empty }

gravityAcceleration :: Fractional a => a
gravityAcceleration = 9.8

playerBaseVelocity :: Fractional a => a
playerBaseVelocity = 10

playerJumpVelocity :: Fractional a => a
playerJumpVelocity = 10

playerManaRegenRate :: Fractional a => a
playerManaRegenRate = 5

spellInterpreter :: forall e m r. (Monad m, Monoid (ObjsInput e m r)) => SF m (ObjInput (Player e m r), Rational) (ObjsInput e m r, Event Rational, Event r)
spellInterpreter = arr (const (mempty, empty, empty))

playerObj :: forall e m r. (Monad m, Monoid (ObjsInput e m r)) => Object e m r (Player e m r)
playerObj = loopPre initialPlayerMana $ proc ((playerIn, _), lastPlayerMana) -> do

  pos' <- (fix $ \k (pos1, vy) ->
    switch (generaliseSF $ fallingMovement pos1 vy) (\pos2 -> switch (generaliseSF $ groundedMovement pos2) k))
    (zeroVector, 0) -< playerIn

  (objsInput, manaCost, replResponse) <- spellInterpreter -< (playerIn, lastPlayerMana)

  playerMana <- iterFrom (\c _ t m -> min 100 ((toRational t + playerManaRegenRate) + m) - event 0 id c) initialPlayerMana -< manaCost

  let playerOutput = PlayerOutput
        { playerX = pos' ^. _x
        , playerY = pos' ^. _y
        , playerMana
        , replResponse
        , replOutput = mempty
        }

  returnA -< ((playerOutput, objsInput), playerMana)
  where
    initialPlayerMana = 100

    groundedMovement :: V -> SF Identity (ObjInput (Player e m r)) (V, Event (V, Double))
    groundedMovement (V2 x0 _) = proc (PlayerInput{simInput = simInput@SimInput{simJump}}) -> do
        let (vx, _) = playerBaseVelocity *^ (simInput ^. moveVector)
        dx <- integral -< vx
        let pos' = V2 (x0 + dx) 0
        -- Horizontal velocity is fully determined by the user input, so we only
        -- return the vertical velocity.
        returnA -< (pos', (pos', playerJumpVelocity) <$ simJump)

    -- Accelerates downwards until both velocity and position would be negative,
    -- then switches back to grounded movement.
    fallingMovement :: V -> Double -> SF Identity (ObjInput (Player e m r)) (V, Event V)
    fallingMovement (V2 x0 y0) vy0 = proc (PlayerInput{simInput}) -> do
      let (vx, _) = playerBaseVelocity *^ (simInput ^. moveVector)
      dx <- integral -< vx
      dvy <- integral -< gravityAcceleration
      let vy = vy0 + dvy
      dy <- integral -< vy
      rec
        pos <- iPre (V2 x0 y0) -< pos'
        let pos' = V2 (x0 + dx) (y0 + dy)
            grounded = pos' ^. _y <= 0 && (vy <= 0)
      returnA -< (pos', gate (Event pos) grounded)