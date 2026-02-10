{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ApplicativeDo #-}
module Spell.Eval (evalSpellUntrusted) where

import Spell (SpellT(..), SpellF(..), mapSpellException, Spell(..), generaliseSpell, spellExceptionFromException, spellExceptionToException)
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
import Control.Monad.Trans.Maybe
import Control.Concurrent.Async
import System.Mem.Weak
import Data.Bifunctor
import Simulation.Util

startEval :: NFData a => Untrusted a -> IO (EvalHandle a)
startEval u = do
  uninterruptibleMask_ do
    evalAsync <- async do
      withTrusted u wait
    -- Problem: observe that we cannot use withTrusted directly, so the
    -- async has to be cancelled explicitly.
    --
    -- Here, we choose to make it the garbage collector's problem via
    -- finalizers, so that some kind of 'cancelEval' operation does not have to
    -- be called manually.
    --
    -- An alternative approach is to make evalSpellUntrusted transform into something like
    -- SpellT _ (IO `Compose` (MaybeT IO) `Product` (ReaderT SomeException IO) `Compose` _)
    -- so that the user can cancel the async manually instead, but that is ugly and
    -- error-prone.
    addFinalizer evalAsync (uninterruptibleCancel evalAsync)
    pure $ EvalHandle {evalAsync}

newtype EvalHandle x = EvalHandle { evalAsync :: Async (Either (Untrusted SomeException) x) } deriving Functor

pollEval :: EvalHandle x -> IO (Maybe (Either (Untrusted SomeException) x))
pollEval = fmap (fmap (join . first toUntrusted)) . poll . evalAsync

-- | EvalSpell' specialised to Untrusted.
evalSpellUntrusted :: forall a.
     Untrusted (Spell a)
  -> SpellT
      (Untrusted SomeException)
      (IO `Compose` (MaybeT IO) `Compose` Either (Untrusted SomeException))
      (Untrusted a)
evalSpellUntrusted =
    evalSpell untrustedCommSome
      (\u -> Compose do
          h <- startEval u
          pure $ Compose . fmap (first $ fmap \(SomeException e) -> makeCatchable e) $ MaybeT $ pollEval h
      )
    . mapSpellException toException fromException
    . join
    . lift
    . fmap generaliseSpell

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
        TFree (TFirebolt a b) ->
          let z = join . lift . fmap SpellT $ join uargs
              z' = unSpellT $ evalSpell uCommSome f z
           in Free (Firebolt a b (pure z'))
        TFree TThrow -> Free (Throw uargs)
        TFree (TPutChar c) ->
          let z = join . lift . fmap SpellT $ join uargs
              z' = unSpellT $ evalSpell uCommSome f z
           in Free (PutChar c (pure z'))
        TFree TGetChar ->
          let z c = join . lift . fmap (flip (fmap SpellT) c) $ join uargs
              z' = unSpellT . evalSpell uCommSome f <$> z
           in Free (GetChar (pure z'))
        TFree TInputTarget ->
          let z v = join . lift . fmap (flip (fmap SpellT) v . uncurry) $ join uargs
              z' = unSpellT . evalSpell uCommSome f <$> z
           in Free (InputTarget (pure $ curry z'))
        TFree TCatch ->
          let expr = evalSpell uCommSome f . join . lift $ uargs >>= view _1
              h e = evalSpell uCommSome f . join . lift $ (uargs >>= view _2) <*> e
              next a = unSpellT . evalSpell uCommSome f . SpellT . join . lift $ (uargs >>= view _3) <*> a
           in Free (Catch (pure expr) (pure h) (pure next))
