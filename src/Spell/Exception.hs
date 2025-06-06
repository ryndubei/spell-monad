module Spell.Exception (SpellException(..)) where

import Control.Exception (Exception(..))

-- OutOfSideEffects and CasterInterrupt will have asynchronous semantics
-- "within" Spell, but not in their implementation
-- SomeAsyncException will be reserved for the interpreter to spot exceptions
-- that we might not want the player to catch (e.g. a ThreadKilled)
-- Hence SomeAsyncException, AsyncException and any other way of constructing
-- an async exception type must not be provided to the player.

data SpellException
  = OutOfSideEffects
  -- ^ when a side-effect-consuming Spell is used when no
  -- side effects remain
  | CasterInterrupt -- ^ Manual interrupt by the caster in-game
  deriving (Eq, Show, Ord)

instance Exception SpellException

-- TODO would be worth defining our own SomeAsyncException for mask to be useful
