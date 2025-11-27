{-# OPTIONS_GHC -Wno-orphans #-}
module Simulation.Objects.SpellInterpreter.Types (ObjOutput(..), ObjInput(..)) where

import Simulation.Objects
import Spell (SpellT(..))
import Data.Sequence (Seq)
import FRP.BearRiver
import Control.Exception
import Control.Monad.Trans.Maybe
import Data.Functor.Product
import Control.Monad.Trans.Reader
import Control.Applicative

data instance ObjInput (SpellInterpreter e m r) = SpellInterpreterInput
  { replInput :: Event (Maybe (SpellT e (MaybeT m `Product` ReaderT SomeException m) r, e -> r, SomeException -> e))
  , stdin :: Event (Seq Char)
  , exception :: Event SomeException
  , feedMana :: Event Double
  }
data instance ObjOutput (SpellInterpreter e m r) = SpellInterpreterOutput
  { replResponse :: Event r
  , stdout :: Event (Seq Char)
  , needMana :: Event Double -- ^ An effect will not be performed until needMana is matched with a feedMana, or with an exception.
  }

instance Semigroup (ObjInput (SpellInterpreter e m r)) where
  (<>) si1 si2 = SpellInterpreterInput
    { replInput = replInput si2 <|> replInput si1
    , stdin = mergeBy (<>) (stdin si1) (stdin si2)
    , exception = exception si1 <|> exception si2
    , feedMana = mergeBy (+) (feedMana si1) (feedMana si2)
    }

instance Monoid (ObjInput (SpellInterpreter e m r)) where
  mempty = SpellInterpreterInput empty empty empty empty
