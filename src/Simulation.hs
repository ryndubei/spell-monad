{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Simulation (SimState(..), SimEvent(..), simSF, ObjectIdentifier(..), SFInput(..)) where

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
import Simulation.Objects.All hiding (Player, TargetSelector)
import qualified Simulation.Objects.All as O
import Simulation.Coordinates
import Data.Foldable
import Data.Function
import qualified Data.IntMap.Strict as IntMap
import Control.Applicative
import Linear (dot, norm)
import Simulation.Util
import Simulation.Component
import Control.Exception

-- | Tells the UI thread how an object should be drawn.
data ObjectIdentifier = Player | Firebolt | TargetSelector | LevelGeometry deriving (Eq, Ord, Show, Generic)

instance NFData ObjectIdentifier

data SimState = SimState
  { sdf :: (Double, Double) -> (ObjectIdentifier, Double)
  , cameraX :: !Double
  , cameraY :: !Double
  , playerMana :: !Double
  , playerMaxMana :: !Double
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

data SFInput = SFInput
  { gameInput :: Event UserInput
  , termStdin :: Event (Seq Char)
  , interpretRequest :: Event (Maybe InterpretRequest)
  }

instance Semigroup SFInput where
  (<>) sfi1 sfi2 = SFInput
    { gameInput = mergeBy (<>) (gameInput sfi1) (gameInput sfi2)
    , termStdin = mergeBy (<>) (termStdin sfi1) (termStdin sfi2)
    , interpretRequest = interpretRequest sfi2 <|> interpretRequest sfi1
    }

instance Monoid SFInput where
  mempty = SFInput empty empty empty

type instance ObjectM = EvalT Untrusted IO
type instance InterpreterReturn = STM ()
type instance InterpreterError = Untrusted SomeException

simSF :: SF (EvalT Untrusted IO) (Event SFInput) (SimState, Event SimEvent)
simSF = arr (event mempty id) >>> proc SFInput{gameInput = u, termStdin = stdin, interpretRequest = req} -> do
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
      playerIn = PlayerInput { simInput, actions = NoEvent }
      spellInterpreterIn = SpellInterpreterInput { replInput, stdin, exception = NoEvent, completeActions = NoEvent, completeInputTargets = NoEvent }
      objsInput = WrappedInputs $ \case
        O.Player -> playerIn
        SpellInterpreter -> spellInterpreterIn
        _ -> mempty
  objsOut <- objectsSF objsOutput0 objs0 -< objsInput
  let !PlayerOutput{..} = unwrapOutputs objsOut O.Player
      !SpellInterpreterOutput{..} = unwrapOutputs objsOut O.SpellInterpreter
      targetSelectorOut = unwrapOutputs objsOut O.TargetSelector
      playerPos = V2 playerX playerY
      simEvent1 = fmap (\r -> mempty { interpretResponse = r }) replResponse
      simEvent2 = fmap (\cs -> mempty { spellOutput = cs }) stdout
      FireboltOutputs !fireboltStates = unwrapOutputs objsOut O.Firebolts
  
  -- Lag camera slightly behind the player position
  playerXLagged <- delay 0.5 0 -< playerX
  playerYLagged <- delay 0.5 0 -< playerY

  returnA -< (SimState
    {
      -- square of diameter 2
      sdf = \(x,y) ->
        let V2 playerDx playerDy = V2 x y - playerPos
            playerDistance = max (abs playerDx - 1) (abs playerDy - 1)
            fireboltDistances = map
              (\FireboltState{fireboltPos, fireboltRadius} ->
                let fireboltDpos = V2 x y - fireboltPos
                 in dot fireboltDpos fireboltDpos - fireboltRadius
              ) (snd <$> IntMap.toList fireboltStates)
            targetSelectorDistance = if visible targetSelectorOut
              then 
                -- relative to player position
                Just . subtract 0.5 . norm $ V2 x y - (V2 (targetX targetSelectorOut) (targetY targetSelectorOut) + V2 playerX playerY)
              else Nothing
         in minimumBy (compare `on` snd) $ (Player, playerDistance) : maybe mempty (pure . (TargetSelector,)) targetSelectorDistance ++ map (Firebolt,) fireboltDistances
    , cameraX = playerXLagged
    , cameraY = playerYLagged
    , playerMana
    , playerMaxMana
    }, mergeBy (<>) simEvent1 simEvent2)
  where
    objs0 :: Obj x y -> Component Obj ObjectM x y
    objs0 = \case
      O.Player -> playerObj
      Firebolts -> fireboltsObj
      SpellInterpreter -> spellInterpreterObj
      O.TargetSelector -> targetSelectorObj
      StaticGeometry -> error "todo"
    objsOutput0 :: ComponentOutputs Obj
    objsOutput0 = \case
      O.Player -> PlayerOutput
        { playerX = 0
        , playerY = 0
        , playerMana = 100
        , playerMaxMana = 100
        , playerFacingDirection = V2 1 0
        }
      Firebolts -> FireboltOutputs mempty
      SpellInterpreter -> SpellInterpreterOutput
        { replResponse = NoEvent
        , stdout = NoEvent
        , blocked = Nothing
        , runningActions = mempty
        }
      O.TargetSelector -> TargetSelectorOutput{targetX = 0, targetY = 0, select = NoEvent, visible = False}
      StaticGeometry -> error "todo"
