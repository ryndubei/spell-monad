{-# OPTIONS_GHC -Wno-orphans #-}
module Simulation.Objects.TargetSelector (targetSelectorObj, TargetSelectorInput(..), TargetSelectorOutput(..)) where

import Simulation.Objects
import FRP.BearRiver
import Simulation.Input
import Control.Lens
import Linear.V2
import Control.Applicative
import Linear
import Simulation.Component

type instance ObjIn TargetSelector = TargetSelectorInput
data TargetSelectorInput = TargetSelectorInput
  { targetSelectorInput :: !SimInput
  , active :: !Bool
  }
type instance ObjOut TargetSelector = TargetSelectorOutput
data TargetSelectorOutput = TargetSelectorOutput
  -- ^ Should be interpreted as relative to player position
  { targetX :: !Double
  , targetY :: !Double
  , visible :: !Bool
  , select :: !(Event ())
  }

instance Semigroup TargetSelectorInput where
  (<>) a b = TargetSelectorInput
    { targetSelectorInput = targetSelectorInput a <> targetSelectorInput b
    , active = active a || active b
    }

instance Monoid TargetSelectorInput where
  mempty = TargetSelectorInput mempty False

targetSelectorSpeed :: Fractional a => a
targetSelectorSpeed = 10

targetSelectorObj :: Monad m => Component Obj m TargetSelectorInput TargetSelectorOutput
targetSelectorObj = toComponent $ proc (TargetSelectorInput{targetSelectorInput, active}) -> do
  let v = targetSelectorSpeed *^ (targetSelectorInput ^. moveVector)
      selectEvent = simEnter targetSelectorInput

  deactivateEvent <- edge -< not active
  resetEvent <- arr (uncurry (<|>)) <<< second (iPre NoEvent) -< (deactivateEvent, selectEvent)

  -- Delayed switch so that there is time for the selection to be observed from the position
  -- of targetX, targetY
  V2 dx dy <- drSwitch integral -< (if active then v else 0, integral <$ resetEvent)

  returnA -< (TargetSelectorOutput
    -- magic number: start slightly offset from the player position
    { targetX = dx + 5
    , targetY = dy + 5
    , visible = active
    , select = selectEvent
    })
