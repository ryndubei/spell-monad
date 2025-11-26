{-# OPTIONS_GHC -Wno-orphans #-}
module Simulation.Objects.SpellInterpreter.Types (ObjOutput(..), ObjInput(..)) where

import Simulation.Objects
import Spell (SpellT(..))
import Data.Sequence
import FRP.BearRiver
import Control.Exception
import Control.Monad.Trans.Maybe
import Data.Functor.Product
import Control.Monad.Trans.Reader

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
