{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Spell
  ( Spell(..)
  , SpellT(..)
  , SpellF(..)
  , hoistSpellT
  , hoistSpellT'
  , generaliseSpell
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
newtype SpellT m a = SpellT {
  unSpellT :: FreeT (SpellF m) m a
} deriving (Functor, Applicative, Monad, MonadIO)

-- For an unknown reason, the equivalent 'deriving newtype' fails with
--
-- • Couldn't match type: FreeT (SpellF m) m a
--               with: SpellT m a
--
-- So the instance is just declared by hand.
instance Monad m => MonadFree (SpellF m) (SpellT m) where
  {-# INLINE wrap #-}
  wrap = wrap . coerce

instance MonadTrans SpellT where
  {-# INLINE lift #-}
  lift = SpellT . lift

newtype Spell a = Spell { unSpell :: FreeT (SpellF Identity) Identity a }
  -- list of instances deliberately kept more sparse than SpellT
  deriving (Functor, Applicative, Monad) via (SpellT Identity)

{-# INLINE generaliseSpell #-}
generaliseSpell :: Monad m => Spell a -> SpellT m a
generaliseSpell = hoistSpellT (pure . runIdentity) . coerce

hoistSpellT :: (Functor m, Monad n) => (forall x. m x -> n x) -> SpellT m a -> SpellT n a
hoistSpellT f = SpellT . transFreeT (hoistSpellF (hoistSpellT f) f) . hoistFreeT f . unSpellT

-- | Like 'hoistSpellT', but the 'Monad' and 'Functor' requirements are reversed.
hoistSpellT' :: (Monad m, Functor n) => (forall x. m x -> n x) -> SpellT m a -> SpellT n a
hoistSpellT' f = SpellT . hoistFreeT f . transFreeT (hoistSpellF (hoistSpellT' f) f) . unSpellT

hoistSpellF :: Functor m => (forall x. SpellT m x -> SpellT n x) -> (forall x. m x -> n x) -> SpellF m a -> SpellF n a
hoistSpellF g f = \case
  Firebolt next -> Firebolt (f next)
  Face a b next -> Face a b (f next)
  Catch expr h next ->
    let expr' = f (fmap g expr)
        h' = f (fmap g <$> h)
    in Catch expr' h' (f next)
  Throw e -> Throw (f e)
  PutChar c next -> PutChar c (f next)
  GetChar next -> GetChar (f next)

data SpellF m next
  -- Anything that is to be _fully_ forced should be annotated with !,
  -- anything else should be annotated with 'm'.
  = Firebolt (m next)
  | Face !Double !Double (m next)
  | forall a e. Exception e => Catch (m (SpellT m a)) (m (e -> SpellT m a)) (m (a -> next))
  | Throw (m SomeException) -- marked as lazy because forcing WHNF isn't necessarily sufficient
  -- ^ we can't just do throwSpell = liftF . throw because imprecise exceptions
  -- have different semantics from precise exceptions. Throwing precise
  -- exceptions has to be a dedicated side effect.
  | PutChar !Char (m next)
  | GetChar (m (Char -> next))

  -- (ThreadKilled should crash the level but not the whole game in case
  -- the player manages to somehow import it)

  -- | forall b. Mask ((forall a. Spell a -> Spell a) -> Spell b) (b -> next)
  -- -- ^ NOTE: applies only to Spell's own "async" exceptions (e.g. OutOfSideEffects)

  -- -- TODO would be funny for OutOfSideEffects to kill the caster when faced with
  -- -- uninterruptibleMask
  -- | forall b. UninterruptibleMask ((forall a. Spell a -> Spell a) -> Spell b) (b -> next)
  -- -- ^ NOTE: applies only to Spell's own "async" exceptions (e.g. OutOfSideEffects)

deriving instance Functor m => Functor (SpellF m)

-- | 'liftF' for 'Spell'
liftSpellF :: SpellF Identity a -> Spell a
liftSpellF = coerce . liftF @_ @(FreeT (SpellF Identity) Identity)

throwSpell :: Exception e => e -> Spell a
throwSpell = liftSpellF . Throw . Identity . SomeException

catch :: forall e a. Exception e => Spell a -> (e -> Spell a) -> Spell a
catch s (h :: e -> Spell a) = 
    let s' = coerce s :: Identity (SpellT Identity a)
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
