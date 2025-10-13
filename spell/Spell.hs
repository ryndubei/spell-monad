{-# LANGUAGE NoImplicitPrelude #-}
module Spell
  ( Spell(..)
  , SpellF(..)
  , firebolt
  , face
  ) where

import Control.Monad.Free
import Prelude hiding (getChar, putChar)
import Control.Monad.Catch
import Data.Typeable
import Control.Exception

newtype Spell a = Spell {
    unSpell :: Free SpellF a
    -- ^ exceptions are not in an ExceptT because we want to allow for catching
    -- e.g. divisions by zero
    --
    -- This means that Spell must be interpreted within IO.
  }
  deriving (Functor, Applicative, Monad, MonadFree SpellF)

data SpellF next
  = Firebolt next
  | Face (Double, Double) next
  | Catch next (SomeException -> Maybe next)
  -- ^ NOTE: (base's) async exceptions won't be caught

  -- (ThreadKilled should crash the level but not the whole game in case
  -- the player manages to somehow import it)

  -- | forall b. Mask ((forall a. Spell a -> Spell a) -> Spell b) (b -> next)
  -- -- ^ NOTE: applies only to Spell's own "async" exceptions (e.g. OutOfSideEffects)

  -- -- TODO would be funny for OutOfSideEffects to kill the caster when faced with
  -- -- uninterruptibleMask
  -- | forall b. UninterruptibleMask ((forall a. Spell a -> Spell a) -> Spell b) (b -> next)
  -- -- ^ NOTE: applies only to Spell's own "async" exceptions (e.g. OutOfSideEffects)

instance MonadThrow Spell where
  throwM = liftF . throw

instance MonadCatch Spell where
  catch (Spell s) (h :: e -> Spell a) = Spell . Free . Catch s $ \(SomeException e) ->
    case (cast e :: Maybe e) of
      Just e' -> Just (unSpell (h e'))
      Nothing -> Nothing

deriving instance Functor SpellF

firebolt :: Spell ()
firebolt = liftF (Firebolt ())

face :: (Double, Double) -> Spell ()
face v = liftF (Face v ())
