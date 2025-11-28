{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Simulation.Objects.All
  ( module Simulation.Objects
  , module Simulation.Objects.Firebolts
  , module Simulation.Objects.Player
  , module Simulation.Objects.SpellInterpreter
  , module Simulation.Objects.TargetSelector
  ) where

import Simulation.Objects
import Simulation.Objects.Firebolts
import Simulation.Objects.Player
import Simulation.Objects.SpellInterpreter
import Simulation.Objects.TargetSelector

instance
  ( Semigroup (ObjInput Player)
  , Semigroup (ObjInput FireboltsObject)
  , Semigroup (ObjInput (SpellInterpreter e m r))
  , Semigroup (ObjInput TargetSelector)
  ) => Semigroup (ObjsInput e m r) where
  (<>) a b = Objects
    { player = player a <> player b
    , firebolts = firebolts a <> firebolts b
    , spellInterpreter = spellInterpreter a <> spellInterpreter b
    , targetSelector = targetSelector a <> targetSelector b
    }

instance
  ( Monoid (ObjInput Player)
  , Monoid (ObjInput FireboltsObject)
  , Monoid (ObjInput (SpellInterpreter e m r))
  , Monoid (ObjInput TargetSelector)
  ) => Monoid (ObjsInput e m r) where
  mempty = Objects
    { player = mempty
    , firebolts = mempty
    , spellInterpreter = mempty
    , targetSelector = mempty
    }
