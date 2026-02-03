{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Simulation.Objects.All
  ( module Simulation.Objects
  , module Simulation.Objects.Firebolts
  , module Simulation.Objects.Player
  , module Simulation.Objects.SpellInterpreter.Types
  , module Simulation.Objects.TargetSelector
  , objectsSF
  ) where

import Simulation.Objects
import Simulation.Objects.Firebolts
import Simulation.Objects.Player
import Simulation.Objects.SpellInterpreter.Types
-- import Simulation.Objects.SpellInterpreter
import Simulation.Objects.TargetSelector
