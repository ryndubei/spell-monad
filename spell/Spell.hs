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
import Control.Exception (SomeException (..), Exception)
import Data.Functor.Identity
import Control.Monad.Trans.Class
import Data.Coerce
import Control.Monad.IO.Class

-- | The Spell monad transformer, allowing interleaved side effects. For use by
-- compiled code. Should not be exposed to the user.
newtype SpellT lazy m a = SpellT {
  unSpellT :: FreeT (SpellF lazy m) m a
} deriving (Functor, Applicative, Monad, MonadIO)

-- For an unknown reason, the equivalent 'deriving newtype' fails with
--
-- • Couldn't match type: FreeT (SpellF lazy m) m a
--               with: SpellT lazy m a
--
-- So the instance is just declared by hand.
instance (Functor lazy, Monad m) => MonadFree (SpellF lazy m) (SpellT lazy m) where
  {-# INLINE wrap #-}
  wrap = wrap . coerce

instance Functor lazy => MonadTrans (SpellT lazy) where
  {-# INLINE lift #-}
  lift = SpellT . lift

newtype Spell a = Spell { unSpell :: FreeT (SpellF Identity Identity) Identity a }
  -- list of instances deliberately kept more sparse than SpellT
  deriving (Functor, Applicative, Monad) via (SpellT Identity Identity)

data SpellF lazy m next
  -- Anything that is to be _fully_ forced should be annotated with !,
  -- anything else should be annotated with 'lazy'.
  = Firebolt (lazy next)
  | Face !Double !Double (lazy next)
  | forall a e. Exception e => Catch (lazy (SpellT m lazy a)) (lazy (e -> SpellT m lazy a)) (lazy (a -> next))
  -- ^ NOTE: (base's) async exceptions won't be caught
  | Throw (lazy SomeException) -- marked as 'lazy' because forcing WHNF isn't necessarily sufficient
  -- ^ we can't just do throwSpell = liftF . throw because imprecise exceptions
  -- have different semantics from precise exceptions. Throwing precise
  -- exceptions has to be a dedicated side effect.
  | PutChar !Char (lazy next)
  | GetChar (lazy (Char -> next))

  -- (ThreadKilled should crash the level but not the whole game in case
  -- the player manages to somehow import it)

  -- | forall b. Mask ((forall a. Spell a -> Spell a) -> Spell b) (b -> next)
  -- -- ^ NOTE: applies only to Spell's own "async" exceptions (e.g. OutOfSideEffects)

  -- -- TODO would be funny for OutOfSideEffects to kill the caster when faced with
  -- -- uninterruptibleMask
  -- | forall b. UninterruptibleMask ((forall a. Spell a -> Spell a) -> Spell b) (b -> next)
  -- -- ^ NOTE: applies only to Spell's own "async" exceptions (e.g. OutOfSideEffects)

deriving instance Functor lazy => Functor (SpellF lazy m)

-- | 'liftF' for 'Spell'
liftSpellF :: SpellF Identity Identity a -> Spell a
liftSpellF = coerce . liftF @_ @(FreeT (SpellF Identity Identity) Identity)

throwSpell :: Exception e => e -> Spell a
throwSpell = liftSpellF . Throw . Identity . SomeException

catch :: forall e a. Exception e => Spell a -> (e -> Spell a) -> Spell a
catch s (h :: e -> Spell a) = 
    let s' = coerce s :: Identity (SpellT Identity Identity a)
        h' = Identity $ fmap (SpellT . unSpell) h
     in Spell . FreeT . Identity . Free $ Catch s' h' (Identity pure)

firebolt :: Spell ()
firebolt = liftSpellF (Firebolt (Identity ()))

face :: (Double, Double) -> Spell ()
face (a,b) = liftSpellF (Face a b (Identity ()))

putChar :: Char -> Spell ()
putChar c = liftSpellF (PutChar c (Identity ()))

getChar :: Spell Char
getChar = liftSpellF (GetChar (Identity id))
