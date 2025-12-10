{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Simulation.Objects.Player.Types (ObjInput(..), ObjOutput(..)) where

import Simulation.Objects
import Simulation.Input
import FRP.BearRiver
import Simulation.Coordinates
import Control.Applicative
import Simulation.Objects.SpellInterpreter.Types

data instance ObjInput Player = PlayerInput
  { simInput :: SimInput -- ^ Continuous user input
  , actions :: Event [Action]
  }
data instance ObjOutput Player = PlayerOutput
  { playerX :: !Double
  , playerY :: !Double
  , playerMana :: !Double
  , playerMaxMana :: !Double
  , playerFacingDirection :: !V
  }

instance Semigroup (ObjInput Player) where
  (<>) p1 p2 = PlayerInput
    { simInput = simInput p1 <> simInput p2
    , actions = mergeBy (++) (actions p1) (actions p2)
    }

instance Monoid (ObjInput Player) where
  mempty = PlayerInput mempty empty
