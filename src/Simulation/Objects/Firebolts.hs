{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BlockArguments #-}
module Simulation.Objects.Firebolts (FireboltState(..), fireboltsObj, FireboltsInput(..), FireboltOutputs(..)) where

import Simulation.Objects
import FRP.BearRiver
import Simulation.Coordinates
import Data.Functor.Identity
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Simulation.Util
import Simulation.Component

type instance ObjIn Firebolts = FireboltsInput
data FireboltsInput = FireboltsInput { spawnFirebolts :: Event [FireboltState], killFirebolts :: Event IntSet }
type instance ObjOut Firebolts = FireboltOutputs
newtype FireboltOutputs = FireboltOutputs (IntMap FireboltState)
data FireboltState = FireboltState { fireboltPos :: !V, fireboltVel :: !V, fireboltRadius :: !Double, lifetime :: !Double } deriving (Eq, Ord, Show)

instance Semigroup FireboltsInput where
  (<>) f1 f2 = FireboltsInput
    { spawnFirebolts = mergeBy (<>) (spawnFirebolts f1) (spawnFirebolts f2)
    , killFirebolts = mergeBy (<>) (killFirebolts f1) (killFirebolts f2)
    }
instance Monoid FireboltsInput where
  mempty = FireboltsInput NoEvent NoEvent

fireboltsObj :: forall m. Monad m => Component Obj m FireboltsInput FireboltOutputs
fireboltsObj = toComponent . generaliseSF $ proc (FireboltsInput{spawnFirebolts, killFirebolts}) -> do
  let spawnSFs = map (generaliseSF . fireboltObj) (event mempty id spawnFirebolts)
      spawns im =
        let is = IntMap.keysSet im
            -- TODO: track next free id explicitly (like IL from Yampa Arcade)
            -- That way, keys can be assumed to be unique.
            k = if IntSet.null is then 0 else IntSet.findMax is + 1
         in IntMap.fromList (zip [k..] spawnSFs) <> im
      spawns' = gate (Event spawns) (isEvent spawnFirebolts)
  rec
    -- Kill firebolts with expired lifetimes
    -- (on the next tick, so firebolts with lifetime = 0 will exist for an infinitesimal time)
    dieFirebolts <- arr (IntMap.keysSet . IntMap.filter \FireboltState{lifetime} -> lifetime <= 0) <<< iPre IntMap.empty -< out
    let kills = fmap (flip IntMap.withoutKeys) killFirebolts
        deaths = gate (Event (`IntMap.withoutKeys` dieFirebolts)) (not $ IntSet.null dieFirebolts)
        endo = mergeBy (.) spawns' (mergeBy (.) deaths kills) -- first kill, then spawn
    out <- rpSwitchB IntMap.empty -< ((), endo)
  returnA -< FireboltOutputs out

fireboltObj :: FireboltState -> SF Identity () FireboltState
fireboltObj s0 = proc () -> do
  dlife <- time -< ()
  dpos <- integral -< fireboltVel s0
  let pos = fireboltPos s0  + dpos
      life = lifetime s0 - dlife
  let s = s0 { fireboltPos = pos, lifetime = life }
  returnA -< s

