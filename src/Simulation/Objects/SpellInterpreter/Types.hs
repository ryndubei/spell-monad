{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Simulation.Objects.SpellInterpreter.Types (SpellInterpreterOutput(..), SpellInterpreterInput(..), Blocked(..), Action(..), unAction, ActionTag(..), InterpreterReturn, InterpreterError) where

import Simulation.Objects
import Spell (SpellT(..))
import Data.Sequence (Seq)
import FRP.BearRiver
import Control.Exception
import Control.Monad.Trans.Maybe
import Data.Functor.Product
import Control.Monad.Trans.Reader
import Control.Applicative
import Data.Default
import Control.Monad.Trans.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Simulation.Coordinates
import Simulation.Component

-- | If we have two ActionTags, the greater one is valid.
--
-- Wrinkle: this is only sound if later ticks produce strictly greater times.
-- That is, we never switch the top-level SpellInterpreter object, and DTime is
-- always > 0.
newtype ActionTag = ActionTag { createdAt :: Time } deriving (Eq, Ord)

data Blocked
  = BlockedOnStdin
  | BlockedOnAction !ActionTag -- ^ unit reply
  | BlockedOnInputTarget !ActionTag -- vector reply

-- | Some non-atomic, possibly-terminating operation that can take an arbitrary
-- amount of time, and can gradually modify the mana available to it.
-- Should always be run to completion when received (it will monitor whether it is
-- still valid or not by itself).
newtype Action = Action
  (Task
    (Event SomeException, ComponentOutputs Obj)
    (ComponentInputs Obj)
    (State Double)
    ())

unAction :: Action -> Task (Event SomeException, ComponentOutputs Obj) (ComponentInputs Obj) (State Double) ()
unAction (Action t) = t

type family InterpreterError

data SpellInterpreterInput = SpellInterpreterInput
  { replInput :: Event (Maybe (SpellT InterpreterError (MaybeT ObjectM `Product` ReaderT SomeException ObjectM) InterpreterReturn, InterpreterError -> InterpreterReturn, SomeException -> InterpreterError))
  , stdin :: Event (Seq Char) -- ^ Unblocks BlockedOnStdin if non-empty
  , exception :: Event SomeException -- ^ Async exception. Unblocks anything.
  , completeActions :: Event (Map ActionTag (Maybe SomeException)) -- ^ Unblocks BlockedOnAction, possibly with a synchronous exception.
  , completeInputTargets :: Event (Map ActionTag (Either SomeException V)) -- ^ Unblocks BlockedOnInputTarget.
  }

type instance ObjIn SpellInterpreter = SpellInterpreterInput

type family InterpreterReturn

data SpellInterpreterOutput = SpellInterpreterOutput
  { replResponse :: !(Event InterpreterReturn)
  , stdout :: !(Event (Seq Char))
  , blocked :: !(Maybe Blocked)
  , runningActions :: !(Set ActionTag)
    -- ^ Note: currently, only one action will be running at a time. Written
    -- this way for ease of reasoning.
  }

type instance ObjOut SpellInterpreter = SpellInterpreterOutput

instance Default SpellInterpreterOutput where
  def = SpellInterpreterOutput empty empty empty mempty

instance Semigroup SpellInterpreterInput where
  (<>) si1 si2 = SpellInterpreterInput
    { replInput = replInput si2 <|> replInput si1
    , stdin = mergeBy (<>) (stdin si1) (stdin si2)
    , exception = exception si1 <|> exception si2
    , completeActions = mergeBy Map.union (completeActions si1) (completeActions si2)
    , completeInputTargets = mergeBy Map.union (completeInputTargets si1) (completeInputTargets si2)
    }

instance Monoid SpellInterpreterInput where
  mempty = SpellInterpreterInput empty empty empty empty empty
