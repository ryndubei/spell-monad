{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Simulation.Component
  ( Component
  , ComponentInputs
  , ComponentOutputs
  -- * Constructing and destroying components 
  , runComponent
  , toComponent
  -- * Combining components
  , tagComponent
  , joinComponents
  -- * Accessing components
  , WrappedInputs(..)
  , shrinkComponent
  , shrinkComponent'
  , callComponent
  ) where

import FRP.BearRiver
import Data.Typeable
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Control.Monad.Trans.Class
import Data.Foldable
import qualified Control.Category

-- | A Component is a signal function with 'neighbours' indexed by 'comp'.
-- The neighbours run in parallel, and can be taken as both inputs
-- and outputs.
data Component comp m a b = Component
  { componentSF :: SF m (a, ComponentInputs comp, ComponentOutputs comp) (b, ComponentOutputs comp, ComponentInputs comp)
  , definedOn :: Seq (SomeComp comp)
    -- ^ Inputs default to mempty, but outputs default to error. So this is needed for handling ComponentOutputs.
  }

data SomeComp comp = forall x y. (Eq (comp x y), Typeable (comp x y)) => SomeComp (comp x y)

-- | The outputs from the other components.`
type ComponentOutputs comp = forall x y. (Eq (comp x y), Typeable (comp x y)) => comp x y -> y

-- | The inputs for all the other components.
type ComponentInputs comp = forall x y. (Eq (comp x y), Typeable (comp x y), Monoid x) => comp x y -> x

-- | Give a name to a component, registering it for interaction by other components by address.
--
-- A component may have multiple names.
--
-- WARNING: Reusing the same tag (whether for the same or a different component) is undefined.
tagComponent :: (Monad m, Monoid a, Eq (comp a b), Typeable (comp a b)) => comp a b -> Component comp m a b -> Component comp m a b
tagComponent comp component = Component
  { componentSF =
      arr (\(a, selIn, selOut) -> (a <> selIn comp, selIn, selOut))
      >>> componentSF component
      >>> arr (\(b, selOut, selIn) -> (b, overrideOutput selOut comp b, selIn))
  , definedOn = SomeComp comp Seq.<| definedOn component
  }

overrideOutput :: Typeable (comp a b) => ComponentOutputs comp -> comp a b -> b -> ComponentOutputs comp
overrideOutput selOut (comp :: t1) b (c :: t2) = case eqT @t1 @t2 of
  Nothing -> selOut c
  Just Refl -> if comp == c then b else selOut c

inputFor :: Typeable (comp a b) => comp a b -> a -> ComponentInputs comp
inputFor (comp :: t1) a (c :: t2) = case eqT @t1 @t2 of 
  Nothing -> mempty
  Just Refl -> if comp == c then a else mempty

-- | 'overlayOutputs scs o1 o2' overlays 'o1' over 'o2' at each element of 'scs'.
overlayOutputs :: [SomeComp comp] -> ComponentOutputs comp -> ComponentOutputs comp -> ComponentOutputs comp
overlayOutputs scs o1 o2 = unwrapOutputs $ foldl'
  (\(WrappedOutputs o2') (SomeComp comp) -> WrappedOutputs $ overrideOutput o2' comp (o1 comp))
  (WrappedOutputs o2) scs


-- | Run two components in parallel, joining their inputs and outputs.
joinComponents :: Monad m => Component comp m a b -> Component comp m x y -> Component comp m (a,x) (b,y)
joinComponents comp1 comp2 = Component
  { definedOn = definedOn comp1 <> definedOn comp2
  , componentSF =
      arr (\((a,x), selIn, selOut) -> ((a, selIn, selOut), (x, selIn, selOut)))
      >>> componentSF comp1 *** componentSF comp2
      >>> arr (\((b, selOut1, selIn1), (y, selOut2, selIn2)) ->
                ( (b,y)
                , overlayOutputs (toList $ definedOn comp1) selOut1 selOut2
                , (\s -> selIn1 s <> selIn2 s)
                )
              )
  }

instance Monad m => Functor (Component comp m b) where
  fmap f comp = comp >>> arr f

instance Monad m => Control.Category.Category (Component comp m) where
  id = toComponent $ Control.Category.id
  (.) c1 c2 = Component
    { definedOn = definedOn c1 <> definedOn c2
    , componentSF = unwrapSF $ proc (a, selIn, selOut) -> do
        -- Observe: ComponentInputs/ComponentOutputs all arrive at the same time
        -- to all tags. The order in which Components are chained in
        -- does not matter.
        (b, selOut1, selIn1) <- wrapSF (componentSF c2) -< (a, selIn, selOut)
        (c, selOut2, selIn2) <- wrapSF (componentSF c1) -< (b, selIn, selOut)
        returnA -<
          ( c
          , WrappedOutputs $ overlayOutputs (toList $ definedOn c1) (unwrapOutputs selOut2) (unwrapOutputs selOut1)
          , WrappedInputs $ \s -> unwrapInputs selIn1 s <> unwrapInputs selIn2 s
          )
    }

instance Monad m => Arrow (Component comp m) where
  arr f = toComponent $ arr f
  (***) = joinComponents
  
-- These are needed to help type inference in some cases (especially with arrow notation)

-- | Implementation detail. Ignore.
newtype WrappedOutputs comp = WrappedOutputs { unwrapOutputs :: ComponentOutputs comp }

newtype WrappedInputs comp = WrappedInputs { unwrapInputs :: ComponentInputs comp }

instance Semigroup (WrappedInputs comp) where
  (<>) (WrappedInputs in1) (WrappedInputs in2) = WrappedInputs \s -> in1 s <> in2 s

instance Monoid (WrappedInputs comp) where
  mempty = WrappedInputs (const mempty)

wrapSF
  :: Monad m
  => SF m (a, ComponentInputs comp, ComponentOutputs comp) (b, ComponentOutputs comp, ComponentInputs comp)
  -> SF m (a, WrappedInputs comp, WrappedOutputs comp) (b, WrappedOutputs comp, WrappedInputs comp)
wrapSF sf =
  arr (\(a, WrappedInputs selIn, WrappedOutputs selOut) -> (a, selIn, selOut))
    >>> sf
    >>> arr (\(b, selOut, selIn) -> (b, WrappedOutputs selOut, WrappedInputs selIn))

unwrapSF
  :: Monad m
  => SF m (a, WrappedInputs comp, WrappedOutputs comp) (b, WrappedOutputs comp, WrappedInputs comp)
  -> SF m (a, ComponentInputs comp, ComponentOutputs comp) (b, ComponentOutputs comp, ComponentInputs comp)
unwrapSF sf = 
  arr (\(a, selIn, selOut) -> (a, WrappedInputs selIn, WrappedOutputs selOut))
    >>> sf
    >>> arr (\(b, WrappedOutputs selOut, WrappedInputs selIn) -> (b, selOut, selIn))


-- | Convert a Component into a regular SF to run it.
-- The first argument is the initial value of each output.
-- If any components have not been implemented,
-- they will output this value as a constant.
runComponent :: Monad m => m (ComponentOutputs comp) -> Component comp m a b -> SF m a b
runComponent defaultOutputs Component{..} = runTask_ do
  outputs0 <- lift $ fmap WrappedOutputs defaultOutputs
  mkTask_ $ loopPre (WrappedInputs (const mempty), outputs0) $
    arr (\(a, (WrappedInputs selIn, WrappedOutputs selOut)) -> (a, selIn, selOut))
    >>> componentSF
    >>> arr (\(b, selOut, selIn) ->
          (b, ( WrappedInputs selIn
              , WrappedOutputs $ overlayOutputs (toList definedOn) selOut (unwrapOutputs outputs0))
          ))

-- | Lift a signal function to a Component.
toComponent :: Monad m => SF m a b -> Component comp m a b
toComponent sf = Component
  { definedOn = mempty
  , componentSF =
      arr (\(a, _, _) -> a)
      >>> sf
      >>> arr (\a -> (a, const (error "Component.toComponent: uninitialised, must check definedOn"), const mempty))
  }

-- | Let a component interact with tagged components.
shrinkComponent' :: Monad m => Component comp m (a, ComponentOutputs comp) (b, ComponentInputs comp) -> Component comp m a b
shrinkComponent' Component{..} = Component
  { componentSF =
      arr (\(a, selIn, selOut) -> ((a,selOut), selIn, selOut))
      >>> componentSF
      >>> arr (\((a,selIn1), selOut, selIn2) -> (a, selOut, \s -> selIn1 s <> selIn2 s))
  , definedOn
  }

-- | Let a component interact with tagged components.
--
-- WrappedInputs exists to improve type inference with arrow notation
-- (it does not handle impredicative types well)
shrinkComponent :: Monad m => Component comp m (a, ComponentOutputs comp) (b, WrappedInputs comp) -> Component comp m a b
shrinkComponent = shrinkComponent' . fmap (\(b, WrappedInputs selIn) -> (b, selIn))

-- | Address a neighbouring component inline, like an internal arrow.
--
-- Important to remember: the component is not actually internal, and so its
-- output will appear out of sync with the input.
callComponent :: (Monad m, Eq (comp x y), Typeable (comp x y)) => comp x y -> Component comp m x y
callComponent comp = shrinkComponent' $ arr \(x, outputs) -> (outputs comp, inputFor comp x)
