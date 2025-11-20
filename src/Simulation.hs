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
import Simulation.Objects.All
import qualified Data.IntMap.Strict as IntMap
import Simulation.Coordinates

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

simSF :: SF (EvalT Untrusted IO) (Event UserInput, Event (Maybe InterpretRequest)) (SimState, Event SimEvent)
simSF = proc (u, req) -> do
  simInput <- generaliseSF processInput -< u

  let replInput =
        req <&> fmap \InterpretRequest{submitResponseHere, toInterpret} ->
          let toInterpret' = evalSpellUntrusted toInterpret
              submitException e = tryReadTMVar submitResponseHere >>= \case
                Nothing -> pure ()
                Just s -> s (Left e)
              submitResult a = tryReadTMVar submitResponseHere >>= \case
                Nothing -> pure ()
                Just s -> s (Right a)
           in (fmap submitResult toInterpret', submitException, pure)
      playerIn = PlayerInput { replInput, simInput, overrideFacingDirection = NoEvent, playerStdin = NoEvent }
      objsInput = mempty { player = playerIn }
  objsOut <- objectsSF objsOutput0 objs0 -< objsInput
  let playerPos = (playerX (player objsOut), playerY (player objsOut))

  returnA -< (SimState
    {
      -- square of diameter 2
      sdf = \(x,y) -> let (x',y') = (x,y) ^-^ playerPos in (Player, max (abs x' - 1) (abs y' - 1))
    , cameraX = 0
    , cameraY = 0
    }, NoEvent)
  where
    objs0 = Objects { player = Identity (PlayerObject playerObj), firebolts = Identity (FireboltsObject fireboltsObj) }
    objsOutput0 = Objects
      { player = PlayerOutput
        { playerX = 0
        , playerY = 0
        , playerMana = 100
        , playerStdout = mempty
        , replResponse = noEvent
        , playerFacingDirection = V2 1 0
        }
      , firebolts = FireboltOutputs IntMap.empty
      }
