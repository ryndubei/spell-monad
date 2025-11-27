{-# OPTIONS_GHC -Wno-orphans #-}
module Simulation.Objects.Player.Types (ObjInput(..), ObjOutput(..)) where

import Simulation.Objects
import Simulation.Input
import FRP.BearRiver
import Data.Sequence (Seq)
import Simulation.Coordinates
import Control.Applicative

data instance ObjInput Player = PlayerInput
  { simInput :: SimInput -- ^ Continuous user input
  , overrideFacingDirection :: Event (Maybe V)
  }
data instance ObjOutput Player = PlayerOutput
  { playerX :: !Double
  , playerY :: !Double
  , playerMana :: !Double
  , playerMaxMana :: !Double
  , playerStdout :: Seq Char
  , playerFacingDirection :: !V
  }

instance Semigroup (ObjInput Player) where
  (<>) p1 p2 = PlayerInput
    { simInput = simInput p1 <> simInput p2
    , overrideFacingDirection = overrideFacingDirection p1 <|> overrideFacingDirection p2
    }

instance Monoid (ObjInput Player) where
  mempty = PlayerInput mempty empty
