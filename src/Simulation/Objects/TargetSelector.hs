{-# OPTIONS_GHC -Wno-orphans #-}
module Simulation.Objects.TargetSelector (targetSelectorObj, ObjOutput(..), ObjInput(..)) where

import Simulation.Objects
import FRP.BearRiver
import Simulation.Input

newtype instance ObjInput TargetSelector = TargetSelectorInput SimInput
data instance ObjOutput TargetSelector = TargetSelectorInactive | TargetSelectorActive
  { targetX :: !Double
  , targetY :: !Double
  }

instance Semigroup (ObjInput TargetSelector) where
  (<>) (TargetSelectorInput a) (TargetSelectorInput b) = TargetSelectorInput (a <> b)

instance Monoid (ObjInput TargetSelector) where
  mempty = TargetSelectorInput mempty

-- TODO
targetSelectorObj :: (Monad m, Monoid (ObjsInput e m r)) => Object e m r TargetSelector
targetSelectorObj = arr (const (TargetSelectorActive 5 5, mempty))
