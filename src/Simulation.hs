{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
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
import Simulation.Coordinates
import Data.Foldable
import Data.Function
import qualified Data.IntMap.Strict as IntMap

-- | Tells the UI thread how an object should be drawn.
data ObjectIdentifier = Player | Firebolt deriving (Eq, Ord, Show, Generic)

instance NFData ObjectIdentifier

data SimState = SimState
  { sdf :: (Double, Double) -> (ObjectIdentifier, Double)
  , cameraX :: !Double
  , cameraY :: !Double
  }

data SimEvent = SimEvent
  { spellOutput :: Seq Char -- ^ for the PutChar side effect.
                            -- Not a String, so that left or right associativity
                            -- does not matter and finiteness is enforced.
  , interpretResponse :: STM ()
  }

instance Semigroup SimEvent where
  (<>) s1 s2 = SimEvent
    { spellOutput = spellOutput s1 <> spellOutput s2
    , interpretResponse = interpretResponse s1 >> interpretResponse s2
    }

instance Monoid SimEvent where
  mempty = SimEvent { spellOutput = mempty, interpretResponse = pure () }

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
  let PlayerOutput{..} = player objsOut
      playerPos = (playerX, playerY)
      simEvent1 = fmap (\r -> mempty { interpretResponse = r }) replResponse
      simEvent2 = gate (Event $ mempty { spellOutput = playerStdout }) (not $ null playerStdout)
      FireboltOutputs fireboltStates = firebolts objsOut
  returnA -< (SimState
    {
      -- square of diameter 2
      sdf = \(x,y) ->
        let (playerDx,playerDy) = (x,y) ^-^ playerPos
            playerDistance = max (abs playerDx - 1) (abs playerDy - 1)
            fireboltDistances = map
              (\FireboltState{fireboltPos, fireboltRadius} ->
                let fireboltDpos = V2 x y ^-^ fireboltPos
                 in dot fireboltDpos fireboltDpos - fireboltRadius
              ) (snd <$> IntMap.toList fireboltStates)
         in minimumBy (compare `on` snd) $ (Player, playerDistance) : map (Firebolt,) fireboltDistances
    , cameraX = 0
    , cameraY = 0
    }, mergeBy (<>) simEvent1 simEvent2)
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
      , firebolts = FireboltOutputs mempty
      }
