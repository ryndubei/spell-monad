{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use =<<" #-}
module Spell.Eval (evalSpellUntrusted, runEvalUntrusted, EvalT, untrustedAllocationLimit) where

import Spell (SpellT(..), SpellF(..), mapSpellException, mapSpellFException, Spell(..), generaliseSpell, SomeSpellException(..), hoistSpellT, joinSpellT)
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
    . hoistSpellT (\(Identity a) -> SpellT $ FreeT $ Pair (pure $ Pure a) (ReaderT \e -> pure (Free $ Throw (pure $ pure e))))
    . evalSpell untrustedCommSome (Identity . unsafeFromUntrusted)
    . mapSpellException (pure . SomeException) (\(SomeException e) -> pure $ SomeSpellException e)
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
  :: forall u n a e. (Monad u, Monad n)
  => (forall k (f :: k -> Type). u (Some f) -> Some (Compose u f)) -- ^ u must preserve hidden types
  -> (forall x. NFData x => u x -> n x)
  -> SpellT e u a
  -> SpellT (u e) n (u a)
evalSpell uCommSome f (SpellT (FreeT m)) = do
  pureOrFree <- lift . pullEither' $ fmap (\case Pure a -> Left a; Free a -> Right a) m
  case pureOrFree of
    Left a -> pure a
    Right u -> do
      spellF <- lift $ pullSpellF' u
      let spellF' = unSpellT . evalSpell uCommSome f . join . lift . fmap SpellT <$> spellF
          spellT = SpellT . FreeT . pure $ Free spellF'
      spellT
  where
    pullEither :: forall x y. u (Some (Product Identity (Tag (Either x y)))) -> n (Either (u x) (u y))
    pullEither u0 = do
      let u1 = uCommSome u0
      withSome u1 \(Compose u2) -> do
        let u3 = fmap (\(Pair (Identity a) b) -> (a,b)) u2
            uargs = fmap fst u3
            utag = fmap snd u3
        tag <- f utag
        case tag of
          TLeft -> pure $ Left uargs
          TRight -> pure $ Right uargs

    pullEither' :: forall x y. u (Either x y) -> n (Either (u x) (u y))
    pullEither' = pullEither . fmap toDSum

    pullSpellF' :: forall next. u (SpellF e u next) -> n (SpellF (u e) n (u next))
    pullSpellF' = pullSpellF . fmap (toDSum . mapSpellFException (pure . pure) id)

    pullSpellF :: forall next. u (Some (Product Identity (Tag (SpellF (u e) u next)))) -> n (SpellF (u e) n (u next))
    pullSpellF u0 = do
      let u1 = uCommSome u0
      withSome u1 \(Compose u2) -> do
        let u3 = fmap (\(Pair (Identity a) b) -> (a,b)) u2
            u = fmap fst u3
        tag <- f $ fmap snd u3
        case tag of
          TFirebolt -> pure $ Firebolt (pure (join u))
          TFace -> do
            a <- f $ fmap (view _1) u
            b <- f $ fmap (view _2) u
            let next = u >>= view _3
            pure $ Face a b (pure next)
          TThrow -> do
            let e = join $ join u
            pure . Throw $ pure e
          TCatch -> do
            let expr = mapSpellException (pure . join) (pure . pure) . evalSpell uCommSome f . join . lift $ u >>= view _1
                h e =
                  mapSpellException (pure . join) (pure . pure)
                  . evalSpell uCommSome f
                  . join
                  . lift
                  $ (<*> pure e) (u >>= view _2)
                next = (<*>) (u >>= view _3)
            pure $ Catch (pure expr) (pure h) (pure next)
          TPutChar -> do
            c <- f $ fmap (view _1) u
            let next = u >>= view _2
            pure $ PutChar c (pure next)
          TGetChar -> do
            let next = (<*>) (join u)
            pure $ GetChar (pure $ next . pure)
