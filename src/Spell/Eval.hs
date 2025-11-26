{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ApplicativeDo #-}
module Spell.Eval (evalSpellUntrusted, runEvalUntrusted, EvalT, untrustedAllocationLimit) where

import Spell (SpellT(..), SpellF(..), mapSpellException, Spell(..), generaliseSpell, hoistSpellT, joinSpellT, spellExceptionFromException, spellExceptionToException)
import Data.Functor.Identity
import Control.Monad.Trans.Free
import Control.DeepSeq
import Data.Some
import Data.Functor.Product
import Control.Monad
import Control.Monad.Trans.Class
import Control.Lens
import Data.Functor.Compose
import Data.Kind
import DependentSums
import Untrusted
import Control.Exception
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Foreign (Int64)
import Control.Monad.Fix

untrustedAllocationLimit :: Int64
untrustedAllocationLimit = 2^(20 :: Int64) -- 10MiB in bytes

runEvalUntrusted :: EvalT Untrusted IO a -> IO a
runEvalUntrusted (EvalT r) = r

-- | Some delayed evaluation strategy of 'u' in 'n'.
newtype EvalT u n a = EvalT (n a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

instance MonadTrans (EvalT u) where
  {-# INLINE lift #-}
  lift = EvalT

-- | EvalSpell' specialised to Untrusted
evalSpellUntrusted :: forall a.
     Untrusted (Spell a)
  -> SpellT
      (Untrusted SomeException)
      (MaybeT (EvalT Untrusted IO) `Product` ReaderT SomeException (EvalT Untrusted IO))
      (Untrusted a)
evalSpellUntrusted =
    joinSpellT
    . hoistSpellT (\(Identity a) -> SpellT $ FreeT $ Pair (pure $ Pure a) (ReaderT \e -> pure (Free $ Throw (pure e))))
    . evalSpell untrustedCommSome (Identity . unsafeFromUntrusted)
    . mapSpellException spellExceptionToException spellExceptionFromException
    . join
    . lift
    . fmap generaliseSpell

-- | Monoid instance for sequentially composing Applicatives.
newtype Semicolonable f = Semicolonable (f ())

instance Applicative f => Semigroup (Semicolonable f) where
  (<>) (Semicolonable a) (Semicolonable b) = Semicolonable $ a <* b

instance Applicative f => Monoid (Semicolonable f) where
  mempty = Semicolonable (pure ())


-- | Turn a particular unnatural transformation into a natural transformation.
evalSpell
  :: forall u n a e. (Monad u, Applicative n)
  => (forall k (f :: k -> Type). u (Some f) -> Some (Compose u f)) -- ^ u must preserve hidden types
  -> (forall x. NFData x => u x -> n x)
  -> SpellT e u a
  -> SpellT (u e) n (u a)
evalSpell uCommSome f (SpellT (FreeT m)) = do
  let u0 = fmap toDSum m
      u1 = uCommSome u0
  withSome u1 \(Compose u2) -> do
    let u3 = fmap (\(Pair (Identity a) b) -> (a,b)) u2
        uargs = fmap fst u3
        utag = fmap snd u3
    SpellT $ FreeT $ do
      tag <- f utag
      pure $ case tag of
        TPure -> Pure uargs
        TFree TFirebolt ->
          let z = join . lift . fmap SpellT $ join uargs
              z' = unSpellT $ evalSpell uCommSome f z
           in Free (Firebolt (pure z'))
        TFree (TFace a b) ->
          let z = join . lift . fmap SpellT $ join uargs
              z' = unSpellT $ evalSpell uCommSome f z
           in Free (Face a b (pure z'))
        TFree TThrow -> Free (Throw uargs)
        TFree (TPutChar c) ->
          let z = join . lift . fmap SpellT $ join uargs
              z' = unSpellT $ evalSpell uCommSome f z
           in Free (PutChar c (pure z'))
        TFree TGetChar ->
          let z c = join . lift . fmap (flip (fmap SpellT) c) $ join uargs
              z' = unSpellT . evalSpell uCommSome f <$> z
           in Free (GetChar (pure z'))
        TFree TCatch ->
          let expr = evalSpell uCommSome f . join . lift $ uargs >>= view _1
              h e = evalSpell uCommSome f . join . lift $ (uargs >>= view _2) <*> e
              next a = unSpellT . evalSpell uCommSome f . SpellT . join . lift $ (uargs >>= view _3) <*> a
           in Free (Catch (pure expr) (pure h) (pure next))
