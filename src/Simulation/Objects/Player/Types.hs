{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Simulation.Objects.Player.Types (PlayerInput(..), PlayerOutput(..)) where

import Simulation.Objects
import Simulation.Input
import FRP.BearRiver
import Simulation.Coordinates
import Control.Applicative
import Simulation.Objects.SpellInterpreter.Types

type instance ObjIn Player = PlayerInput
data PlayerInput = PlayerInput
  { simInput :: SimInput -- ^ Continuous user input
  , actions :: Event [Action]
  }
type instance ObjOut Player = PlayerOutput
data PlayerOutput = PlayerOutput
  { playerX :: !Double
  , playerY :: !Double
  , playerMana :: !Double
  , playerMaxMana :: !Double
  , playerFacingDirection :: !V
  }

instance Semigroup PlayerInput where
  (<>) p1 p2 = PlayerInput
    { simInput = simInput p1 <> simInput p2
    , actions = mergeBy (++) (actions p1) (actions p2)
    }

instance Monoid PlayerInput where
  mempty = PlayerInput mempty empty
