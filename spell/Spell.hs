{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module Spell
  ( Spell(..)
  , SpellT(..)
  , SpellF(..)
  , SomeSpellException(..)
  , spellExceptionFromException
  , spellExceptionToException
  , mapSpellException
  , mapSpellFException
  , hoistSpellT
  , hoistSpellT'
  , generaliseSpell
  , firebolt
  , face
  , putChar
  , getChar
  , catch
  , throwSpell
  , joinSpellT
  , spellTCollapseExceptT
  ) where

import Control.Monad.Trans.Free
import Prelude hiding (getChar, putChar)
import Control.Exception (SomeException(..), Exception(..))
import Data.Functor.Identity
import Control.Monad.Trans.Class
import Data.Coerce
import Control.Monad.IO.Class
import Data.Typeable
import Data.Kind
import Control.Monad
import Control.Category ((>>>))
import Control.Monad.Trans.Except

-- | The Spell monad transformer, allowing interleaved side effects. For use by
-- compiled code. Should not be exposed to the user.
newtype SpellT (e :: Type) (m :: Type -> Type) (a :: Type) = SpellT {
  unSpellT :: FreeT (SpellF e m) m a
} deriving (Functor, Applicative, Monad, MonadIO)

-- For an unknown reason, the equivalent 'deriving newtype' fails with
--
-- • Couldn't match type: FreeT (SpellF m) m a
--               with: SpellT m a
--
-- So the instance is just declared by hand.
instance Monad m => MonadFree (SpellF e m) (SpellT e m) where
  {-# INLINE wrap #-}
  wrap = wrap . coerce

instance MonadTrans (SpellT e) where
  {-# INLINE lift #-}
  lift = SpellT . lift

newtype Spell a = Spell { unSpell :: FreeT (SpellF SomeSpellException Identity) Identity a }
  -- list of instances deliberately kept more sparse than SpellT
  deriving (Functor, Applicative, Monad) via (SpellT SomeSpellException Identity)

{-# INLINE generaliseSpell #-}
generaliseSpell :: Monad m => Spell a -> SpellT SomeSpellException m a
generaliseSpell = hoistSpellT (pure . runIdentity) . coerce

hoistSpellT :: (Functor m, Monad n) => (forall x. m x -> n x) -> SpellT e m a -> SpellT e n a
hoistSpellT f = SpellT . transFreeT (hoistSpellF (hoistSpellT f) f) . hoistFreeT f . unSpellT

-- | Like 'hoistSpellT', but the 'Monad' and 'Functor' requirements are reversed.
hoistSpellT' :: (Monad m, Functor n) => (forall x. m x -> n x) -> SpellT e m a -> SpellT e n a
hoistSpellT' f = SpellT . hoistFreeT f . transFreeT (hoistSpellF (hoistSpellT' f) f) . unSpellT

hoistSpellF :: Functor m => (forall x. SpellT e m x -> SpellT e n x) -> (forall x. m x -> n x) -> SpellF e m a -> SpellF e n a
hoistSpellF g f = \case
  Firebolt next -> Firebolt (f next)
  Face a b next -> Face a b (f next)
  Catch expr h next ->
    let expr' = f (fmap g expr)
        h' = f . fmap (fmap $ g . fmap (fmap g)) $ h
    in Catch expr' h' (f next)
  Throw e -> Throw (f e)
  PutChar c next -> PutChar c (f next)
  GetChar next -> GetChar (f next)

joinSpellT :: Monad m => SpellT e (SpellT e m) a -> SpellT e m a
joinSpellT = unSpellT >>> iterT \case
  Firebolt next -> wrap (Firebolt . pure $ join next)
  Face a b next -> wrap (Face a b . pure $ join next)
  Catch expr h next ->
    let expr' = expr >>= joinSpellT
        h' e = h >>= (fmap (fmap joinSpellT) . joinSpellT . ($ e))
        next' a = next >>= ($ a)
     in wrap (Catch (pure expr') (pure h') (pure next'))
  Throw e -> e >>= liftF . Throw . pure
  PutChar c next -> wrap (PutChar c (pure $ join next))
  GetChar next -> wrap (GetChar (pure $ \c -> next >>= \n -> n c))

spellTCollapseExceptT :: Monad m => SpellT e (ExceptT e m) a -> SpellT e m a
spellTCollapseExceptT = hoistSpellT (\(ExceptT m) -> do
  m' <- lift m
  case m' of
    Left e -> liftF (Throw (pure e))
    Right r -> pure r
  ) >>> joinSpellT

-- | The functions should induce an isomorphism (m e <-> m e') to preserve throw/catch behaviour.
mapSpellException :: Monad m => (e -> m e') -> (e' -> m e) -> SpellT e m a -> SpellT e' m a
mapSpellException f g = SpellT . transFreeT (mapSpellFException f g) . unSpellT

mapSpellFException :: Monad m => (e -> m e') -> (e' -> m e) -> SpellF e m a -> SpellF e' m a
-- TODO: vaguely resembles functor adjointness, maybe could generalise
mapSpellFException f _ (Throw e) = Throw (e >>= f)
mapSpellFException f g (Catch expr h next) =
  Catch (fmap (mapSpellException f g) expr) (fmap (\h' -> fmap lift g >=> fmap (fmap $ mapSpellException f g) . mapSpellException f g . h') h) next
mapSpellFException _ _ (Firebolt a) = Firebolt a
mapSpellFException _ _ (Face a b next) = Face a b next
mapSpellFException _ _ (PutChar c next) = PutChar c next
mapSpellFException _ _ (GetChar next) = GetChar next

data SpellF e (m :: Type -> Type) next
  -- Anything that is to be _fully_ forced should be annotated with !,
  -- anything else should be annotated with 'm'.
  = Firebolt (m next)
  | Face !Double !Double (m next)
  -- nesting of SpellT e m (Maybe (SpellT e m a)), because otherwise we lose the ability to join.
  -- TODO: simplify to SpellT e m (Maybe a)?
  | forall a. Catch (m (SpellT e m a)) (m (e -> SpellT e m (Maybe (SpellT e m a)))) (m (a -> next))
  | Throw (m e)
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

data SomeSpellException = forall e. Exception e => SomeSpellException e

instance Show SomeSpellException where
  show (SomeSpellException e) = show e

instance Exception SomeSpellException where
  toException = spellExceptionToException
  fromException = spellExceptionFromException

spellExceptionToException :: Exception e => e -> SomeException
spellExceptionToException = toException . SomeSpellException

spellExceptionFromException :: Exception e => SomeException -> Maybe e
spellExceptionFromException x = do
  SomeSpellException a <- fromException x
  cast a

deriving instance Functor m => Functor (SpellF e m)

-- | 'liftF' for 'Spell'
liftSpellF :: SpellF SomeSpellException Identity a -> Spell a
liftSpellF = coerce . liftF @_ @(FreeT (SpellF SomeSpellException Identity) Identity)

throwSpell :: Exception e => e -> Spell a
throwSpell = liftSpellF . Throw . Identity . SomeSpellException

catch :: forall e a. Exception e => Spell a -> (e -> Spell a) -> Spell a
catch s (h :: e -> Spell a) =
    let s' = coerce s :: Identity (SpellT SomeSpellException Identity a)
        h' = Identity $ \(SomeSpellException e) -> pure (generaliseSpell . h <$> cast e)
     in Spell . FreeT . Identity . Free $ Catch s' h' (Identity pure)

firebolt :: Spell ()
firebolt = liftSpellF (Firebolt (Identity ()))

face :: (Double, Double) -> Spell ()
face (a,b) = liftSpellF (Face a b (Identity ()))

putChar :: Char -> Spell ()
putChar c = liftSpellF (PutChar c (Identity ()))

getChar :: Spell Char
getChar = liftSpellF (GetChar (Identity id))
