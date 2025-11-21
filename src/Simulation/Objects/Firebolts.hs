{-# OPTIONS_GHC -Wno-orphans #-}
module Simulation.Objects.Firebolts (FireboltState(..), ObjInput(..), ObjOutput(..), fireboltsObj) where

import Simulation.Objects
import FRP.BearRiver
import Simulation.Coordinates
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Functor.Identity

-- Firebolts are indexed by their states. You cannot have two firebolts with the exact same state.
data instance ObjInput FireboltsObject = FireboltsInput { spawnFirebolts :: Event (Set FireboltState), killFirebolts :: Event (Set FireboltState) }
data FireboltState = FireboltState { fireboltPos :: !V, fireboltVel :: !V, fireboltRadius :: !Double, lifetime :: !Int } deriving (Eq, Ord, Show)
newtype instance ObjOutput FireboltsObject = FireboltOutputs [FireboltState]

instance Semigroup (ObjInput FireboltsObject) where
  (<>) f1 f2 = FireboltsInput
    { spawnFirebolts = Event $ event mempty id (spawnFirebolts f1) <> event mempty id (spawnFirebolts f2)
    , killFirebolts = Event $ event mempty id (killFirebolts f1) <> event mempty id (killFirebolts f2)
    }
instance Monoid (ObjInput FireboltsObject) where
  mempty = FireboltsInput NoEvent NoEvent

fireboltsObj :: forall e m r. (Monad m, Monoid (ObjsInput e m r)) => Object e m r FireboltsObject
fireboltsObj = dpSwitchB []
  (proc ((FireboltsInput{ spawnFirebolts, killFirebolts }, _), _) -> do
    let spawns = foldr (\fs -> (Set.insert fs .)) id <$> spawnFirebolts
        kills = foldr (\fs -> (Set.delete fs .)) id <$> killFirebolts
        endo = mergeBy (.) spawns kills -- kill first, then spawn
    returnA -< endo
  )
  (\sfs endo -> 
    let sfs' = dSwitch (parB sfs >>> arr (\c -> (c, Event c))) (\c -> undefined)
     in proc u -> do
      col <- sfs' -< u
      undefined -< undefined
  )
  >>> arr (\x -> (FireboltOutputs x, mempty))

fireboltObj :: FireboltState -> SF Identity () FireboltState
fireboltObj s0 = proc () -> do
  dlife <- time -< ()
  dpos <- integral -< fireboltVel s0
  let pos = fireboltPos s0 ^+^ dpos
      life = lifetime s0 - floor dlife
  returnA -< s0 { fireboltPos = pos, lifetime = life }

