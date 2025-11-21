{-# OPTIONS_GHC -Wno-orphans #-}
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
fireboltsObj = dpSwitchB mempty
  (proc ((FireboltsInput{ spawnFirebolts, killFirebolts }, _), states) -> do
    let lifetimeKills = IntMap.keysSet $ IntMap.filter ((<= 0) . lifetime) states
        kills = mergeBy (<>) killFirebolts $ if IntSet.null lifetimeKills then NoEvent else Event lifetimeKills
        e = mergeBy (\x y -> (fst x, snd y)) (fmap (,mempty) spawnFirebolts) (fmap (mempty,) kills)
    returnA -< e
  )
  (\sfs (spawns, kills) ->
    let sfs' = sfs `IntMap.withoutKeys` kills -- kill first
        k = maybe 0 ((+ 1) . fst) $ IntMap.lookupMax sfs'
        newObjs = map ((arr (const ()) >>>) . generaliseSF <$> fireboltObj) $ filter ((>0) . lifetime) spawns
        sfs'' = IntMap.fromList (zip [k..] newObjs) <> sfs' -- then spawn
     in par (\x ys -> fmap (x,) ys) sfs''
  )
  >>> arr (\x -> (FireboltOutputs x, mempty))

fireboltObj :: FireboltState -> SF Identity () FireboltState
fireboltObj s0 = proc () -> do
  dlife <- time -< ()
  dpos <- integral -< fireboltVel s0
  let pos = fireboltPos s0  ^+^ dpos
      life = lifetime s0 - dlife
  let s = s0 { fireboltPos = pos, lifetime = life }
  returnA -< s

