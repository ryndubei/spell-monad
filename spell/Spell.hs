{-# LANGUAGE NoImplicitPrelude #-}
module Spell
  ( Spell(..)
  , SpellF(..)
  , firebolt
  , face
  , putChar
  , getChar
  ) where

import Control.Monad.Free
import Prelude hiding (getChar, putChar)
import Control.Monad.Catch
import Data.Typeable
import Control.Exception
import GHC.Stack

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
  | forall a. Catch (Spell a) (SomeException -> Maybe (Spell a)) (a -> next)
  -- ^ NOTE: (base's) async exceptions won't be caught
  | PutChar Char next
  | GetChar (Char -> next)

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
  catch :: (HasCallStack, Exception e) => Spell a -> (e -> Spell a) -> Spell a
  catch s (h :: e -> Spell a) =
    let h' (SomeException e) =
          case (cast e :: Maybe e) of
            Just e' -> Just (h e')
            Nothing -> Nothing
     in Spell . Free $ Catch s h' pure

deriving instance Functor SpellF

firebolt :: Spell ()
firebolt = liftF (Firebolt ())

face :: (Double, Double) -> Spell ()
face v = liftF (Face v ())

putChar :: Char -> Spell ()
putChar c = liftF (PutChar c ())

getChar :: Spell Char
getChar = liftF (GetChar id)
