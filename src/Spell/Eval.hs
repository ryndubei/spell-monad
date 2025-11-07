{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Spell.Eval (evalSpell) where

import Spell (SpellT(..), SpellF(..), mapSpellException, mapSpellFException)
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
import Type.Reflection
import DependentSums

-- | Turn a particular unnatural transformation into a natural transformation.
evalSpell
  :: forall u n a e. (Monad u, Monad n, Typeable u)
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

    pullMaybe' :: forall x. u (Maybe x) -> n (Maybe (u x))
    pullMaybe' = pullMaybe . fmap toDSum

    pullMaybe :: forall x. u (Some (Product Identity (Tag (Maybe x)))) -> n (Maybe (u x))
    pullMaybe u0 = do
      let u1 = uCommSome u0
      withSome u1 \(Compose u2) -> do
        let u3 = fmap (\(Pair (Identity a) b) -> (a,b)) u2
            uargs = fmap fst u3
            utag = fmap snd u3
        tag <- f utag
        case tag of
          TJust -> pure $ Just uargs
          TNothing -> pure Nothing

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
                h e = fmap (fmap (mapSpellException (pure . join) (pure . pure) . evalSpell uCommSome f . join . lift)) . pullMaybe' . join . (<*> pure e) $ u >>= view _2
                next = (<*>) (u >>= view _3)
            pure $ Catch (pure expr) (pure h) (pure next)
          TPutChar -> do
            c <- f $ fmap (view _1) u
            let next = u >>= view _2
            pure $ PutChar c (pure next)
          TGetChar -> do
            let next = (<*>) (join u)
            pure $ GetChar (pure $ next . pure)
