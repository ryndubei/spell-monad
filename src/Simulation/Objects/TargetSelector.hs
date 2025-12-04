{-# OPTIONS_GHC -Wno-orphans #-}
module Simulation.Objects.TargetSelector (targetSelectorObj, ObjOutput(..), ObjInput(..)) where

import Simulation.Objects
import FRP.BearRiver
import Simulation.Input

data instance ObjInput TargetSelector = TargetSelectorInput
  { targetSelectorInput :: !SimInput
  , active :: !Bool
  }
data instance ObjOutput TargetSelector = TargetSelectorOutput
  -- ^ Should be interpreted as relative to player position
  { targetX :: !Double
  , targetY :: !Double
  , visible :: !Bool
  , select :: !(Event ())
  }

instance Semigroup (ObjInput TargetSelector) where
  (<>) a b = TargetSelectorInput
    { targetSelectorInput = targetSelectorInput a <> targetSelectorInput b
    , active = active a || active b
    }

instance Monoid (ObjInput TargetSelector) where
  mempty = TargetSelectorInput mempty False

-- TODO
targetSelectorObj :: (Monad m, Monoid (ObjsInput e m r)) => Object e m r TargetSelector
targetSelectorObj = proc (TargetSelectorInput{active}, _) -> do
  returnA -< (defOut active, mempty)
  where
    defOut v = TargetSelectorOutput 5 5 v NoEvent
