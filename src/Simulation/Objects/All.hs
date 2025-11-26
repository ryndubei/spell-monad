{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Simulation.Objects.All
  ( module Simulation.Objects
  , module Simulation.Objects.Firebolts
  , module Simulation.Objects.Player
  ) where

import Simulation.Objects
import Simulation.Objects.Firebolts
import Simulation.Objects.Player

instance
  ( Semigroup (ObjInput Player)
  , Semigroup (ObjInput FireboltsObject)
  , Semigroup (ObjInput (SpellInterpreter e m r))
  ) => Semigroup (ObjsInput e m r) where
  (<>) a b = Objects
    { player = player a <> player b
    , firebolts = firebolts a <> firebolts b
    , spellInterpreter = spellInterpreter a <> spellInterpreter b
    }

instance
  ( Monoid (ObjInput Player)
  , Monoid (ObjInput FireboltsObject)
  , Monoid (ObjInput (SpellInterpreter e m r))
  ) => Monoid (ObjsInput e m r) where
  mempty = Objects
    { player = mempty
    , firebolts = mempty
    , spellInterpreter = mempty
    }
