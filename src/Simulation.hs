{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Simulation (SimState(..), SimEvent(..), simSF, ObjectIdentifier(..)) where

import FRP.BearRiver
import Input
import Simulation.Input
import Control.Lens
import GHC.Generics
import Control.DeepSeq
import Data.Sequence (Seq)
import App.Thread.Repl
import Control.Concurrent.STM
import Untrusted
import Spell.Eval
import App.Thread.SF
import Control.Monad.Fix
import Data.Maybe
import Control.Monad

-- | Tells the UI thread how an object should be drawn.
data ObjectIdentifier = Player deriving (Eq, Ord, Show, Generic)

instance NFData ObjectIdentifier

data SimState = SimState
  { sdf :: (Double, Double) -> (ObjectIdentifier, Double)
  , cameraX :: !Double
  , cameraY :: !Double
  }

data SimEvent = SimEvent
  { gameOver :: !Bool
  , spellOutput :: Seq Char -- ^ for the PutChar side effect.
                            -- Not a String, so that left or right associativity
                            -- does not matter and finiteness is enforced.
  , interpretResponse :: STM ()
  }

instance Semigroup SimEvent where
  (<>) s1 s2 = SimEvent
    { gameOver = gameOver s1 || gameOver s2
    , spellOutput = spellOutput s1 <> spellOutput s2
    , interpretResponse = interpretResponse s1 >> interpretResponse s2
    }

instance Monoid SimEvent where
  mempty = SimEvent { gameOver = False, spellOutput = mempty, interpretResponse = pure () }

simSF :: MonadFix m => SF (EvalT Untrusted m) (Event UserInput, Event (Maybe InterpretRequest)) (SimState, Event SimEvent)
simSF = proc (u, req) -> do
  simInput <- generaliseSF processInput -< u

  let req' =
        req <&> fmap \InterpretRequest{submitResponseHere, toInterpret} ->
          let toInterpret' = evalSpellUntrusted toInterpret
              submitException e = tryReadTMVar submitResponseHere >>= \case
                Nothing -> pure ()
                Just s -> s (Left e)
              submitResult a = tryReadTMVar submitResponseHere >>= \case
                Nothing -> pure ()
                Just s -> s (Right a)
           in (submitException, fmap submitResult toInterpret')

  let newReq = void req'
  reqHold <- arr (fromMaybe (const $ pure (), pure (pure ()))) <<< hold Nothing -< req'

  rec
    onGround <- iPre True <<< arr ((<= 0) . snd . playerPos) -< playerState
    playerState <- generaliseSF player -< (simInput, onGround)

  returnA -< (SimState
    {
      -- square of diameter 2
      sdf = \(x,y) -> let (x',y') = (x,y) ^-^ playerPos playerState in (Player, max (abs x' - 1) (abs y' - 1))
    , cameraX = 0
    , cameraY = 0
    }, NoEvent)

data PlayerState = PlayerState
  { playerPos :: !(Double, Double)
  , playerHealth :: !Rational
  , playerMana :: !Rational
  } deriving Show

playerBaseVelocity :: Fractional a => a
playerBaseVelocity = 10

playerJumpVelocity :: Fractional a => a
playerJumpVelocity = 10

gravity :: Fractional a => a
gravity = 9.8

player :: SF Identity (SimInput, Bool) PlayerState
player = proc (u, onGround) -> do
  let vx = fst $ playerBaseVelocity *^ (u ^. moveVector)

  let jumpImpulse = gate (tag (simJump u) playerJumpVelocity) onGround

  rec
    hitImpulse <- arr (uncurry tag) <<< (edge *** iPre 0) -< (onGround, -vy)
    vy <- impulseIntegral -< (if onGround then 0 else (-gravity), mergeBy (+) jumpImpulse hitImpulse)
  pos <- trapezoidIntegral -< (vx, vy)

  returnA -< PlayerState
    { playerPos = pos
    , playerHealth = 1
    , playerMana = 1
    }