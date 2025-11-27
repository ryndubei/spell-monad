{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
module Simulation.Objects where

import FRP.BearRiver
import Data.Kind
import Data.Functor.Identity
import Data.Coerce

type Object e m r o = SF m (ObjInput o, ObjsOutput e m r) (ObjOutput o, ObjsInput e m r)
type Object' o = forall e m r. Monad m => Object e m r o

data family ObjInput o

data family ObjOutput o

type Objects = Objects1 Identity

type ObjsInput = Objects1 ObjInput

type ObjsOutput = Objects1 ObjOutput

data Objects1 f (e :: Type) m r = Objects
  { player :: !(f Player)
  , firebolts :: !(f FireboltsObject)
  , spellInterpreter :: !(f (SpellInterpreter e m r))
  }

newtype Player = PlayerObject (Object' Player)
newtype FireboltsObject = FireboltsObject (Object' FireboltsObject)
newtype SpellInterpreter (e :: Type) (m :: Type -> Type) (r :: Type) = SpellInterpreterObject (Object e m r (SpellInterpreter e m r))

objectsSF :: forall e m r. (Monad m, Monoid (ObjsInput e m r)) => ObjsOutput e m r -> Objects e m r -> SF m (ObjsInput e m r) (ObjsOutput e m r)
objectsSF objsOutput0 objs = loopPre (objsOutput0, mempty) $ proc (objsInputExternal, (objsOutput, objsInputInternal)) -> do
  let objsInput = objsInputInternal <> objsInputExternal
  (player, in1) <- pobj -< (player objsInput, objsOutput)
  (firebolts, in2) <- fobj -< (firebolts objsInput, objsOutput)
  (spellInterpreter, in3) <- sobj -< (spellInterpreter objsInput, objsOutput)
  let objsInputInternal' = in1 <> in2 <> in3
  returnA -< second (,objsInputInternal') $ dup Objects{..}
  where
    PlayerObject pobj = coerce (player objs)
    FireboltsObject fobj = coerce (firebolts objs)
    sobj = coerce (spellInterpreter objs)
