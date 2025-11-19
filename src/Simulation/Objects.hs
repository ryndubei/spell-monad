{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
module Simulation.Objects
  ( Objects1(..)
  , Object
  , Object'
  , ObjsInput
  , ObjsOutput
  , ObjInput
  , ObjOutput
  , Player(..)
  , FireboltsObject(..)
  , objectsSF
  ) where

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
  { player :: f (Player e m r)
  , firebolts :: f FireboltsObject
  }

newtype Player (e :: Type) (m :: Type -> Type) (r :: Type) = PlayerObject (Object e m r (Player e m r))
newtype FireboltsObject = FireboltsObject (Object' FireboltsObject)

instance
  ( Semigroup (ObjInput (Player e m r))
  , Semigroup (ObjInput FireboltsObject)
  ) => Semigroup (ObjsInput e m r) where
  (<>) a b = Objects
    { player = player a <> player b
    , firebolts = firebolts a <> firebolts b
    }

instance
  ( Monoid (ObjInput (Player e m r))
  , Monoid (ObjInput FireboltsObject)
  ) => Monoid (ObjsInput e m r) where
  mempty = Objects
    { player = mempty
    , firebolts = mempty
    }

objectsSF :: forall e m r. (Monad m, Monoid (ObjsInput e m r)) => ObjsOutput e m r -> Objects e m r -> SF m (ObjsInput e m r) (ObjsOutput e m r)
objectsSF objsOutput0 objs = loopPre (objsOutput0, mempty) $ proc (objsInputExternal, (objsOutput, objsInputInternal)) -> do
  let objsInput = objsInputExternal <> objsInputInternal
  (player, in1) <- pobj -< (player objsInput, objsOutput)
  (firebolts, in2) <- fobj -< (firebolts objsInput, objsOutput)
  let objsInputInternal' = in1 <> in2
  returnA -< second (,objsInputInternal') $ dup Objects{..}
  where
    pobj = coerce (player objs)
    FireboltsObject fobj = coerce (firebolts objs)
