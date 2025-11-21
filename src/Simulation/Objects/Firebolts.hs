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

-- Firebolts are indexed by their states. You cannot have two firebolts with the exact same state.
data instance ObjInput FireboltsObject = FireboltsInput { spawnFirebolts :: Event [FireboltState], killFirebolts :: Event IntSet }
data FireboltState = FireboltState { fireboltPos :: !V, fireboltVel :: !V, fireboltRadius :: !Double, lifetime :: !Double } deriving (Eq, Ord, Show)
newtype instance ObjOutput FireboltsObject = FireboltOutputs (IntMap FireboltState)

instance Semigroup (ObjInput FireboltsObject) where
  (<>) f1 f2 = FireboltsInput
    { spawnFirebolts = Event $ event mempty id (spawnFirebolts f1) <> event mempty id (spawnFirebolts f2)
    , killFirebolts = Event $ event mempty id (killFirebolts f1) <> event mempty id (killFirebolts f2)
    }
instance Monoid (ObjInput FireboltsObject) where
  mempty = FireboltsInput NoEvent NoEvent

fireboltsObj :: forall e m r. (Monad m, Monoid (ObjsInput e m r)) => Object e m r FireboltsObject
fireboltsObj = dpSwitchB mempty
  (proc ((FireboltsInput{ spawnFirebolts, killFirebolts }, _), states) -> do
    let spawns i = snd . foldr (\fb (k, f) -> (k + 1, IntMap.insert k fb . f)) (i, id) <$> spawnFirebolts
        idx = maybe 0 fst $ IntMap.lookupLE (maxBound :: Int) states
        kills' = let m' = IntMap.filter ((<= 0) . lifetime) states in if IntMap.null m' then NoEvent else Event $ IntMap.keysSet m'
    returnA -< mergeBy (\(a,_) (_,b) -> (a,b)) (fmap (, mempty) (spawns idx)) ((mempty,) <$> mergeBy (<>) killFirebolts kills' )
  )
  (\sfs (spawns, kills) ->
    let newObjs = fireboltObj <$> spawns mempty
        sfs' =
          par (\x ys -> fmap (x,) ys)
          . IntSet.fold (\k -> (IntMap.delete k .)) id kills $
          (sfs <> fmap ((arr (const ()) >>>) . generaliseSF) newObjs)
     in proc u -> do
      sfs' -< u
  )
  >>> arr (\x -> (FireboltOutputs x, mempty))

fireboltObj :: FireboltState -> SF Identity () FireboltState
fireboltObj s0 = proc () -> do
  dlife <- time -< ()
  dpos <- integral -< fireboltVel s0
  let pos = fireboltPos s0 ^+^ dpos
      life = lifetime s0 - dlife
  returnA -< s0 { fireboltPos = pos, lifetime = life }

