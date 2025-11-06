{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Spell.Eval () where

import Spell (Spell (..), SpellT(..), SpellF(..), generaliseSpell)
import Control.Exception
import Data.Functor.Identity
import Untrusted
import Control.Monad.Trans.Free
import Control.DeepSeq
import Data.Some
import Data.Functor.Product
import Control.Monad
import Control.Monad.Trans.Class
import Data.Functor
import Data.GADT.DeepSeq
import Control.Lens
import Data.Functor.Compose
import Control.Monad.Trans.Reader

untrust :: Spell a -> SpellT Untrusted a
untrust = generaliseSpell

-- | Turn a particular unnatural transformation into a natural transformation.
--
-- We can do this because all non-NFData arguments in SpellF are wrapped with 'm'.
evalSpell :: forall n a. Monad n => (forall x. NFData x => Untrusted x -> n x) -> SpellT Untrusted a -> SpellT n (Untrusted a)
evalSpell f (SpellT (FreeT m)) = do
  -- TODO: express in terms of untrustedCommSome

  let isPure = fmap (\case Pure _ -> True; Free _ -> False) m
  isPure' <- lift $ f isPure

  if isPure'
    then -- Therefore we can collapse the action.
         -- (would not work with an arbitrary monad, this works with
         -- Untrusted because it will always compute the same result
         -- when successful)
      pure $ fmap (\case Pure a -> a; _ -> no) m
    else do
      let m' = fmap (\case Free s -> s; _ -> no) m
          mtagged = m' <&> toSpellTag
      tag <- lift $ pullOut mtagged
      undefined
  where
    no = error "impossible"

    pullOut :: forall next. Untrusted (Some (Product Identity (SpellTag Untrusted next))) -> n (SpellF n (Untrusted next))
    pullOut u0 = do
      let u1 = untrustedCommSome u0
      withSome u1 \(Compose u2) -> do
        let u3 = fmap (\(Pair (Identity a) b) -> (a,b)) u2
            uargs = fmap fst u3
            utag = fmap snd u3
        tag <- f utag
        trust tag uargs

    trust :: forall next z. SpellTag Untrusted next z -> Untrusted z -> n (SpellF n (Untrusted next))
    trust TFirebolt u = pure $ Firebolt (pure (join u))
    trust TFace u = do
      a <- f $ fmap (view _1) u
      b <- f $ fmap (view _2) u
      let next = u >>= view _3
      pure $ Face a b (pure next)
    trust TCatch u = do
      let expr = evalSpell f . join . lift $ u >>= view _1
          h e = evalSpell f . join . lift . fmap ($ e) $ u >>= view _2
          next = (<*>) (u >>= view _3)
      pure $ Catch (pure expr) (pure h) (pure next)
    trust TThrow e = do
      pure $ Throw undefined
    trust TPutChar u = undefined

data UntrustedException = UntrustedException
  { baseException :: Untrusted SomeException
  , lazyShow :: String
  }

instance Show UntrustedException where
  show UntrustedException{lazyShow} = undefined

-- | The arguments of each constructor of SpellF on the type level.
--
-- TODO: remove this boilerplate
data SpellTag m next a where
  TFirebolt :: SpellTag m next (m next)
  TFace :: SpellTag m next (Double, Double, m next)
  TCatch :: Exception e => SpellTag m next (m (SpellT m a), m (e -> SpellT m a), m (a -> next))
  TThrow :: SpellTag m next (m SomeException)
  TPutChar :: SpellTag m next (Char, m next)
  TGetChar :: SpellTag m next (m (Char -> next))

-- This isomorphism is explicitly specified to ensure the above is in sync with SpellF
toSpellTag :: SpellF m next -> Some (Product Identity (SpellTag m next))
toSpellTag (Firebolt next) = Some $ Pair (Identity next) TFirebolt
toSpellTag (Face a b next) = Some $ Pair (Identity (a,b,next)) TFace
toSpellTag (Catch expr h next) = Some $ Pair (Identity (expr,h,next)) TCatch
toSpellTag (Throw e) = Some $ Pair (Identity e) TThrow
toSpellTag (PutChar c next) = Some $ Pair (Identity (c, next)) TPutChar
toSpellTag (GetChar next) = Some $ Pair (Identity next) TGetChar

fromSpellTag :: SpellTag m next a -> a -> SpellF m next
fromSpellTag TFirebolt a = Firebolt a
fromSpellTag TFace (a,b,next) = Face a b next
fromSpellTag TCatch (expr,h,next) = Catch expr h next
fromSpellTag TThrow e = Throw e
fromSpellTag TGetChar next = GetChar next
fromSpellTag TPutChar (c, next) = PutChar c next

instance NFData (SpellTag m a b) where
  rnf TFirebolt = ()
  rnf TFace = ()
  rnf TCatch = ()
  rnf TThrow = ()
  rnf TGetChar = ()
  rnf TPutChar = ()