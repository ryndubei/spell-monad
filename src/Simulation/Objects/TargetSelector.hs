{-# OPTIONS_GHC -Wno-orphans #-}
module Simulation.Objects.TargetSelector (targetSelectorObj, ObjOutput(..), ObjInput(..)) where

import Simulation.Objects
import FRP.BearRiver

data instance ObjInput TargetSelector = TargetSelectorInput
data instance ObjOutput TargetSelector = TargetSelectorOutput

instance Semigroup (ObjInput TargetSelector) where
  (<>) _ _ = TargetSelectorInput

instance Monoid (ObjInput TargetSelector) where
  mempty = TargetSelectorInput

-- TODO
targetSelectorObj :: (Monad m, Monoid (ObjsInput e m r)) => Object e m r TargetSelector
targetSelectorObj = arr (const (TargetSelectorOutput, mempty))
