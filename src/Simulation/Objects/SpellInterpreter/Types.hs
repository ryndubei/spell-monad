{-# OPTIONS_GHC -Wno-orphans #-}
module Simulation.Objects.SpellInterpreter.Types (ObjOutput(..), ObjInput(..), Blocked(..), Action(..), unAction, ActionTag(..)) where

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
  (forall e m r. Task
    (State Double)
    (Event SomeException, ObjsOutput e m r)
    (ObjsInput e m r)
    ())

unAction :: Action -> Task (State Double) (Event SomeException, ObjsOutput e m r) (ObjsInput e m r) ()
unAction (Action t) = t

data instance ObjInput (SpellInterpreter e m r) = SpellInterpreterInput
  { replInput :: Event (Maybe (SpellT e (MaybeT m `Product` ReaderT SomeException m) r, e -> r, SomeException -> e))
  , stdin :: Event (Seq Char) -- ^ Unblocks BlockedOnStdin if non-empty
  , exception :: Event SomeException -- ^ Async exception. Unblocks anything.
  , completeActions :: Event (Map ActionTag (Maybe SomeException)) -- ^ Unblocks BlockedOnAction, possibly with a synchronous exception.
  , completeInputTargets :: Event (Map ActionTag (Either SomeException V)) -- ^ Unblocks BlockedOnInputTarget.
  }

data instance ObjOutput (SpellInterpreter e m r) = SpellInterpreterOutput
  { replResponse :: !(Event r)
  , stdout :: !(Event (Seq Char))
  , blocked :: !(Maybe Blocked)
  , runningActions :: !(Set ActionTag)
    -- ^ Note: currently, only one action will be running at a time. Written
    -- this way for ease of reasoning.
  }

instance Default (ObjOutput (SpellInterpreter e m r)) where
  def = SpellInterpreterOutput empty empty empty mempty

instance Semigroup (ObjInput (SpellInterpreter e m r)) where
  (<>) si1 si2 = SpellInterpreterInput
    { replInput = replInput si2 <|> replInput si1
    , stdin = mergeBy (<>) (stdin si1) (stdin si2)
    , exception = exception si1 <|> exception si2
    , completeActions = mergeBy Map.union (completeActions si1) (completeActions si2)
    , completeInputTargets = mergeBy Map.union (completeInputTargets si1) (completeInputTargets si2)
    }

instance Monoid (ObjInput (SpellInterpreter e m r)) where
  mempty = SpellInterpreterInput empty empty empty empty empty
