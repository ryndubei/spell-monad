{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Simulation.Objects.SpellInterpreter.Types
  ( SpellInterpreterOutput(..)
  , SpellInterpreterInput(..)
  , Blocked(..)
  , Action(..)
  , ActionTag(..)
  , InterpreterReturn
  , InterpreterError
  , timeToActionTag
  , intToActionTag
  , unAction
  ) where

import Simulation.Objects
import Spell (SpellT(..))
import Data.Sequence (Seq)
import FRP.BearRiver
import Control.Exception
import Control.Monad.Trans.Maybe
import Control.Applicative
import Data.Default
import Control.Monad.Trans.State.Strict
import Simulation.Component
import Type.Reflection
import Data.Dependent.Map (DMap)
import Data.Some (Some)
import Data.IntSet (IntSet)
import GHC.Float
import qualified Data.Dependent.Map as DMap
import Data.GADT.Compare
import qualified Data.IntSet as IntSet
import Data.Kind
import Data.Functor.Compose
import Data.GADT.Show

data ActionTag c = ActionTag
  { actionId :: !Int -- ^ Should be unique.
  , actionTy :: !(TypeRep c)
  } deriving (Eq, Ord, Show)

type role ActionTag nominal

instance GEq ActionTag where
  geq = defaultGeq
instance GCompare ActionTag where
  gcompare at1 at2 = case compare (actionId at1) (actionId at2) of
    LT -> GLT
    GT -> GGT
    EQ -> gcompare (actionTy at1) (actionTy at2)

instance GShow ActionTag where
  gshowsPrec = defaultGshowsPrec

timeToActionTag :: Typeable c => Time -> ActionTag c
timeToActionTag = intToActionTag . fromIntegral . castDoubleToWord64

intToActionTag :: Typeable c => Int -> ActionTag c
intToActionTag i = ActionTag { actionTy = typeRep, actionId = i}

data Blocked
  = BlockedOnStdin
  | BlockedOnAction !(Some @Type ActionTag)

-- | Some non-atomic, possibly-terminating operation that can take an arbitrary
-- amount of time, and can gradually modify the mana available to it.
-- Should always be run to completion when received (it will monitor whether it is
-- still valid or not by itself).
data Action = forall c. Action
  { actionTag :: !(ActionTag c)
  , actionTask :: Task
    (WrappedOutputs Obj)
    (WrappedInputs Obj)
    (State Double)
    (Either SomeException c)
  }

unAction :: Action -> Task (Event SomeException, WrappedOutputs Obj) (WrappedInputs Obj) (State Double) ()
unAction Action{actionTask, actionTag} = do
  let task = mkTask' (arr snd >>> runTask actionTask)
  c <- task `abortWhen` monitor
  let (inputs, result) = case c of
        -- action completed
        Left (Left c') -> (mempty, Just c')
        -- action cancelled and completed at the same time,
        -- should not send result
        Left (Right (_c', Left ())) -> (mempty, Nothing)
        -- action received an exception at the same time as completed,
        -- should prioritise action result
        Left (Right (c', Right _)) -> (mempty, Just c')
        -- action cancelled without completing, should
        -- preserve its intermediate outputs
        Right (ins, eex) -> (ins, either (const Nothing) (Just . Left) eex)

      completeActions = case result of
        Nothing -> NoEvent
        Just r -> Event (DMap.singleton actionTag r)

  shriek $ inputs <> WrappedInputs \case
    SpellInterpreter -> mempty{completeActions}
    _ -> mempty
  where
    monitor = proc (e1, WrappedOutputs oo) -> do
      let isActionRunning = actionId actionTag `IntSet.member` (runningActions $ oo SpellInterpreter)
      e2 <- iEdge False -< not isActionRunning
      returnA -< lMerge (fmap Left e2) (fmap Right e1)

type family InterpreterError

data SpellInterpreterInput = SpellInterpreterInput
  { replInput :: Event (Maybe (SpellT InterpreterError (ObjectM `Compose` MaybeT ObjectM `Compose` Either InterpreterError) InterpreterReturn, InterpreterError -> InterpreterReturn, SomeException -> InterpreterError))
  , stdin :: Event (Seq Char) -- ^ Unblocks BlockedOnStdin if non-empty
  , exception :: Event SomeException -- ^ Async exception. Unblocks anything.
  , completeActions :: Event (DMap ActionTag (Either SomeException)) -- ^ Unblocks BlockedOnAction, possibly with a synchronous exception.
  }

type instance ObjIn SpellInterpreter = SpellInterpreterInput

type family InterpreterReturn

data SpellInterpreterOutput = SpellInterpreterOutput
  { replResponse :: !(Event InterpreterReturn)
  , stdout :: !(Event (Seq Char))
  , blocked :: !(Maybe Blocked)
  , runningActions :: !IntSet -- ^ Action IDs
  }

type instance ObjOut SpellInterpreter = SpellInterpreterOutput

instance Default SpellInterpreterOutput where
  def = SpellInterpreterOutput empty empty empty mempty

instance Semigroup SpellInterpreterInput where
  (<>) si1 si2 = SpellInterpreterInput
    { replInput = replInput si2 <|> replInput si1
    , stdin = mergeBy (<>) (stdin si1) (stdin si2)
    , exception = exception si1 <|> exception si2
    , completeActions = mergeBy DMap.union (completeActions si1) (completeActions si2)
    }

instance Monoid SpellInterpreterInput where
  mempty = SpellInterpreterInput empty empty empty empty
