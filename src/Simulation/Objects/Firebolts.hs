{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BlockArguments #-}
module Simulation.Objects.Firebolts (FireboltState(..), ObjInput(..), ObjOutput(..), fireboltsObj) where

import Simulation.Objects
import FRP.BearRiver
import Simulation.Coordinates
import Data.Functor.Identity
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import App.Thread.SF

data instance ObjInput FireboltsObject = FireboltsInput { spawnFirebolts :: Event [FireboltState], killFirebolts :: Event IntSet }
data FireboltState = FireboltState { fireboltPos :: !V, fireboltVel :: !V, fireboltRadius :: !Double, lifetime :: !Double } deriving (Eq, Ord, Show)
newtype instance ObjOutput FireboltsObject = FireboltOutputs (IntMap FireboltState)

instance Semigroup (ObjInput FireboltsObject) where
  (<>) f1 f2 = FireboltsInput
    { spawnFirebolts = mergeBy (<>) (spawnFirebolts f1) (spawnFirebolts f2)
    , killFirebolts = mergeBy (<>) (killFirebolts f1) (killFirebolts f2)
    }
instance Monoid (ObjInput FireboltsObject) where
  mempty = FireboltsInput NoEvent NoEvent

fireboltsObj :: forall e m r. (Monad m, Monoid (ObjsInput e m r)) => Object e m r FireboltsObject
fireboltsObj = generaliseSF $ proc (FireboltsInput{spawnFirebolts, killFirebolts}, _) -> do
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
  returnA -< (FireboltOutputs out, mempty)

fireboltObj :: FireboltState -> SF Identity () FireboltState
fireboltObj s0 = proc () -> do
  dlife <- time -< ()
  dpos <- integral -< fireboltVel s0
  let pos = fireboltPos s0  + dpos
      life = lifetime s0 - dlife
  let s = s0 { fireboltPos = pos, lifetime = life }
  returnA -< s

