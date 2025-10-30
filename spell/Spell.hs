{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingVia #-}
module Spell
  ( Spell(..)
  , SpellT(..)
  , SpellF(..)
  , firebolt
  , face
  , putChar
  , getChar
  , catch
  , throwSpell
  ) where

import Control.Monad.Trans.Free
import Prelude hiding (getChar, putChar)
import Data.Typeable
import Control.Exception (SomeException (..), Exception)
import Data.Functor.Identity
import Control.Monad.Trans.Class
import Data.Coerce
import Control.Monad.IO.Class

-- | The Spell monad transformer, allowing interleaved side effects. For use by
-- compiled code. Should not be exposed to the user.
newtype SpellT m a = SpellT {
  unSpellT :: FreeT (SpellF m) m a
} deriving (Functor, Applicative, Monad, MonadFree (SpellF m), MonadIO)

instance MonadTrans SpellT where
  {-# INLINE lift #-}
  lift = SpellT . lift

newtype Spell a = Spell { unSpell :: FreeT (SpellF Identity) Identity a }
  -- list of instances deliberately kept more sparse than SpellT
  deriving (Functor, Applicative, Monad) via (SpellT Identity)

data SpellF m next
  = Firebolt next
  | Face (Double, Double) next
  | forall a. Catch (SpellT m a) (SomeException -> Maybe (SpellT m a)) (a -> next)
  -- ^ NOTE: (base's) async exceptions won't be caught
  | Throw SomeException
  -- ^ we can't just do throwSpell = liftF . throw because imprecise exceptions
  -- have different semantics from precise exceptions. Throwing precise
  -- exceptions has to be a dedicated side effect.
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

deriving instance Functor (SpellF m)

-- | 'liftF' for 'Spell'
liftSpellF :: SpellF Identity a -> Spell a
liftSpellF = coerce . (liftF @_ @(SpellT Identity))

throwSpell :: Exception e => e -> Spell a
throwSpell = liftSpellF . Throw . SomeException

catch :: forall e a. Exception e => Spell a -> (e -> Spell a) -> Spell a
catch s (h :: e -> Spell a) = 
    let s' = coerce s :: SpellT Identity a
        h' :: SomeException -> Maybe (SpellT Identity a)
        h' (SomeException e) =
          case (cast e :: Maybe e) of
            Just e' -> Just (coerce $ h e')
            Nothing -> Nothing
     in Spell . FreeT . Identity . Free $ Catch s' h' pure

firebolt :: Spell ()
firebolt = liftSpellF (Firebolt ())

face :: (Double, Double) -> Spell ()
face v = liftSpellF (Face v ())

putChar :: Char -> Spell ()
putChar c = liftSpellF (PutChar c ())

getChar :: Spell Char
getChar = liftSpellF (GetChar id)
