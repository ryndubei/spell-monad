{-# OPTIONS_GHC -Wno-orphans #-}
module Simulation.Objects.Firebolts (FireboltState(..), ObjInput(..), ObjOutput(..), fireboltsObj) where

import Simulation.Objects
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import FRP.BearRiver
import Data.IntSet
import Simulation.Coordinates

data instance ObjInput FireboltsObject = FireboltsInput { spawnFirebolts :: [FireboltState], killFirebolts :: IntSet }
data FireboltState = FireboltState { fireboltPos :: !V, fireboltVel :: !V, fireboltRadius :: !Double }
newtype instance ObjOutput FireboltsObject = FireboltOutputs (IntMap FireboltState)

instance Semigroup (ObjInput FireboltsObject) where
  (<>) f1 f2 = FireboltsInput
    { spawnFirebolts = spawnFirebolts f1 <> spawnFirebolts f2
    , killFirebolts = killFirebolts f1 <> killFirebolts f2
    }
instance Monoid (ObjInput FireboltsObject) where
  mempty = FireboltsInput mempty mempty

fireboltsObj :: forall e m r. (Monad m, Monoid (ObjsInput e m r)) => Object e m r FireboltsObject
fireboltsObj = arr $ const (FireboltOutputs IntMap.empty, mempty)
