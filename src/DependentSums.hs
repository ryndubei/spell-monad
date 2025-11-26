{-# OPTIONS_GHC -Werror #-} -- eliminate possibilities for being out of sync with the actual types
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE BlockArguments #-}

-- | Some ad-hoc dependent sums for existing types
module DependentSums (DSum(..), Tag(..)) where

import Spell
import Data.Some.Newtype
import Data.Functor.Product
import Data.Functor.Identity
import Control.DeepSeq
import Data.Kind
import Control.Monad.Trans.Free

-- TODO: auto create instances via TH/generics

class DSum s where
  data Tag s :: Type -> Type
  toDSum :: s -> Some (Product Identity (Tag s))
  fromDSum :: Tag s args -> args -> s  

instance DSum (SpellF e m next) where
  data Tag (SpellF e m next) a where 
    TFirebolt :: Tag (SpellF e m next) (m next)
    TFace :: !Double -> !Double -> Tag (SpellF e m next) (m next)
    TCatch :: Tag (SpellF e m next) (m (SpellT e m a), m (e -> SpellT e m a), m (a -> next))
    TThrow :: Tag (SpellF e m next) e
    TPutChar :: !Char -> Tag (SpellF e m next) (m next)
    TGetChar :: Tag (SpellF e m next) (m (Char -> next))
  toDSum (Firebolt next) = Some $ Pair (Identity next) TFirebolt
  toDSum (Face a b next) = Some $ Pair (Identity next) (TFace a b)
  toDSum (Catch expr h next) = Some $ Pair (Identity (expr,h,next)) TCatch
  toDSum (Throw e) = Some $ Pair (Identity e) TThrow
  toDSum (PutChar c next) = Some $ Pair (Identity next) (TPutChar c)
  toDSum (GetChar next) = Some $ Pair (Identity next) TGetChar
  fromDSum TFirebolt a = Firebolt a
  fromDSum (TFace a b) next = Face a b next
  fromDSum TCatch (expr,h,next) = Catch expr h next
  fromDSum TThrow e = Throw e
  fromDSum TGetChar next = GetChar next
  fromDSum (TPutChar c) next = PutChar c next

instance NFData (Tag (SpellF e m next) a) where
  rnf TFirebolt = ()
  rnf (TFace !_ !_) = ()
  rnf TCatch = ()
  rnf TThrow = ()
  rnf TGetChar = ()
  rnf (TPutChar !_) = ()

instance DSum (Either a b) where
  data Tag (Either a b) c where
    TLeft :: Tag (Either a b) a
    TRight :: Tag (Either a b) b
  toDSum (Left a) = Some $ Pair (Identity a) TLeft
  toDSum (Right b) = Some $ Pair (Identity b) TRight
  fromDSum TLeft a = Left a
  fromDSum TRight b = Right b

instance NFData (Tag (Either a b) c) where
  rnf TLeft = ()
  rnf TRight = ()

instance DSum (Maybe a) where
  data Tag (Maybe a) b where
    TJust :: Tag (Maybe a) a
    TNothing :: Tag (Maybe a) ()
  toDSum (Just a) = Some $ Pair (Identity a) TJust
  toDSum Nothing = Some $ Pair (Identity ()) TNothing
  fromDSum TJust a = Just a
  fromDSum TNothing () = Nothing

instance NFData (Tag (Maybe a) b) where
  rnf TJust = ()
  rnf TNothing = ()

instance DSum (f b) => DSum (FreeF f a b) where
  data Tag (FreeF f a b) c where
    TPure :: Tag (FreeF f a b) a
    TFree :: Tag (f b) x -> Tag (FreeF f a b) x
  toDSum (Pure x) = Some $ Pair (Identity x) TPure
  toDSum (Free x) = withSome (toDSum x) \(Pair (Identity x') t) -> Some $ Pair (Identity x') (TFree t)
  fromDSum TPure a = Pure a
  fromDSum (TFree t) a = Free (fromDSum t a)

instance NFData (Tag (f b) c) => NFData (Tag (FreeF f a b) c) where
  rnf TPure = ()
  rnf (TFree t) = rnf t
