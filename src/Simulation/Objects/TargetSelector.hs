{-# OPTIONS_GHC -Wno-orphans #-}
module Simulation.Objects.TargetSelector (targetSelectorObj, ObjOutput(..), ObjInput(..)) where

import Simulation.Objects
import FRP.BearRiver
import Simulation.Input
import Control.Lens
import Linear.V2
import Control.Monad.Fix
import Control.Applicative

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

targetSelectorSpeed :: Fractional a => a
targetSelectorSpeed = 10

targetSelectorObj :: (Monad m, Monoid (ObjsInput e m r)) => Object e m r TargetSelector
targetSelectorObj = proc (TargetSelectorInput{targetSelectorInput, active}, _) -> do
  let v = targetSelectorSpeed *^ (targetSelectorInput ^. moveVector)
      selectEvent = simEnter targetSelectorInput

  deactivateEvent <- edge -< not active
  -- Delay selectEvent for consideration of resets
  -- (so that the selection can be observed from targetX, targetY)
  resetEvent <- arr (uncurry (<|>)) <<< second (iPre NoEvent) -< (deactivateEvent, selectEvent)

  -- on resetEvent, reset dx, dy to 0
  -- (by switching into a new integral SF)
  V2 dx dy <- fix (\k -> switch (first integral) (\() -> second (initially NoEvent) >>> k))
    -< (if active then v else zeroVector, resetEvent)

  returnA -< (TargetSelectorOutput
    -- magic number: start slightly offset from the player position
    { targetX = dx + 5
    , targetY = dy + 5
    , visible = active
    , select = selectEvent
    }
    , mempty)
