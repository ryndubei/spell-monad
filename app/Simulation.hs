{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
module Simulation (SimState(..), SimEvent(..), simSF, ObjectIdentifier(..)) where

import FRP.Yampa
import Input
import Data.Text (Text)
import qualified Data.Text as T
import Simulation.Input
import Control.Lens
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import GHC.Generics
import Control.DeepSeq

-- | Tells the UI thread how an object should be drawn.
data ObjectIdentifier = Player deriving (Eq, Ord, Show, Generic)

instance NFData ObjectIdentifier

data SimState = SimState
  { sdf :: (Double, Double) -> (ObjectIdentifier, Double)
  , cameraX :: !Double
  , cameraY :: !Double
  }

data SimEvent = SimEvent
  { simLogs :: Seq Text
  , gameOver :: !Bool
  }

instance Semigroup SimEvent where
  (<>) s1 s2 = SimEvent
    { simLogs = simLogs s1 <> simLogs s2
    , gameOver = gameOver s1 || gameOver s2
    }

instance Monoid SimEvent where
  mempty = SimEvent { simLogs = mempty, gameOver = False }

simSF :: SF (Event UserInput) (SimState, Event SimEvent)
simSF = proc u -> do
  simInput <- processInput -< u

  rec
    onGround <- iPre True <<< arr ((<= 0) . snd . playerPos) -< playerState
    playerState <- player -< (simInput, onGround)

  stateLogEvent <- repeatedly 1 () -< ()
  let stateLog = tag stateLogEvent (Seq.fromList [T.pack $ show playerState])
      stateLogSimEvent = (\l -> mempty {simLogs = l}) <$> stateLog

  returnA -< (SimState
    {
      -- square of diameter 2
      sdf = \(x,y) -> let (x',y') = (x,y) ^-^ playerPos playerState in (Player, max (abs x' - 1) (abs y' - 1))
    , cameraX = 0
    , cameraY = 0
    }, mergeBy (<>) (fmap evt u) stateLogSimEvent)
  where
    evt u = SimEvent { gameOver = False, simLogs = Seq.fromList [T.pack $ show u] }

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