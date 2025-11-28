{-# OPTIONS_GHC -Wno-orphans #-}
module Simulation.Objects.TargetSelector (targetSelectorObj, ObjOutput(..), ObjInput(..)) where

import Simulation.Objects
import FRP.BearRiver
import Simulation.Input
import Control.Applicative

data instance ObjInput TargetSelector = TargetSelectorInput
  { targetSelectorInput :: !SimInput
  , activate :: !(Event ())
  }
data instance ObjOutput TargetSelector = TargetSelectorInactive | TargetSelectorActive
  { targetX :: !Double
  , targetY :: !Double
  }

instance Semigroup (ObjInput TargetSelector) where
  (<>) a b = TargetSelectorInput
    { targetSelectorInput = targetSelectorInput a <> targetSelectorInput b
    , activate = activate a <|> activate b
    }

instance Monoid (ObjInput TargetSelector) where
  mempty = TargetSelectorInput mempty empty

-- TODO
targetSelectorObj :: (Monad m, Monoid (ObjsInput e m r)) => Object e m r TargetSelector
targetSelectorObj = arr (const (TargetSelectorActive 5 5, mempty))
