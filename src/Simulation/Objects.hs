{-# LANGUAGE TypeData #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Simulation.Objects where

import Data.Kind
import Simulation.Component
import FRP.BearRiver
import Data.Typeable
import Control.Monad.Trans.Class

type family ObjIn (o :: ObjType)
type family ObjOut (o :: ObjType)

type data ObjType
  = Player
  | Firebolts
  | SpellInterpreter
  | TargetSelector
  | StaticGeometry

data Obj x y where
  Player :: Obj (ObjIn Player) (ObjOut Player)
  Firebolts :: Obj (ObjIn Firebolts) (ObjOut Firebolts)
  SpellInterpreter :: Obj (ObjIn SpellInterpreter) (ObjOut SpellInterpreter)
  TargetSelector :: Obj (ObjIn TargetSelector) (ObjOut TargetSelector)
  StaticGeometry :: Obj (ObjIn StaticGeometry) (ObjOut StaticGeometry)

deriving instance Eq (Obj x y)

type family ValidObjects (os :: [ObjType]) :: Constraint where
  ValidObjects '[] = ()
  ValidObjects (o:os) = (Monoid (ObjIn o), Typeable o, Typeable (ObjIn o), Typeable (ObjOut o), ValidObjects os)

-- Type aliases to be defined later
type family ObjectM :: Type -> Type

objectsSF
  :: (Monad ObjectM, ValidObjects [ Player, Firebolts, SpellInterpreter, TargetSelector ])
  => ComponentOutputs Obj
  -> (forall x y. Obj x y -> Component Obj ObjectM x y)
  -> SF ObjectM (WrappedInputs Obj) (WrappedOutputs Obj)
objectsSF objOutputs0 objs = runComponent objOutputs0 $ proc inputs1 -> do
  let WrappedInputs inputs = inputs1
  playerOut <- objs Player -< inputs Player
  fireboltsOut <- objs Firebolts -< inputs Firebolts
  -- spellInterpreterOut <- objs SpellInterpreter -< inputs SpellInterpreter
  targetSelOut <- objs TargetSelector -< inputs TargetSelector
  returnA -< WrappedOutputs \case
    Player -> playerOut
    Firebolts -> fireboltsOut
    SpellInterpreter -> objOutputs0 SpellInterpreter
    TargetSelector -> targetSelOut
    StaticGeometry -> objOutputs0 StaticGeometry