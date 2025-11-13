{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
module Simulation (SimState(..), SimEvent(..), simSF, ObjectIdentifier(..)) where

import FRP.Yampa
import Input
import Simulation.Input
import Control.Lens
import GHC.Generics
import Control.DeepSeq
import Data.Sequence (Seq)
import App.Thread.Repl
import Control.Concurrent.STM

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

simSF :: SF (Event UserInput, Event (Maybe InterpretRequest)) (SimState, Event SimEvent)
simSF = proc (u, req) -> do
  simInput <- processInput -< u

  rec
    onGround <- iPre True <<< arr ((<= 0) . snd . playerPos) -< playerState
    playerState <- player -< (simInput, onGround)

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

player :: SF (SimInput, Bool) PlayerState
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