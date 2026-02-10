{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- partitionEithersIM
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Simulation.Util
  ( dynCollection
  , generaliseSF
  , morphSF
  , msfToCoroutine
  , coroutineToMsf
  , makeCatchable
  ) where

import FRP.BearRiver
import Control.Monad.State.Strict (StateT)
import Control.Monad.Trans.MSF (mapReaderT, runReaderS, readerS, runStateS)
import Data.Functor.Identity
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Either
import Data.Bifunctor
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Data.Void
import Data.MonadicStreamFunction.InternalCore (MSF(..))
import Data.Functor ((<&>))
import Data.Function ((&))
import Control.Exception
import Spell (spellExceptionToException)

-- | To make it possible for player code to catch some exception 'e',
-- it should be wrapped as 'makeCatchable e'.
makeCatchable :: Exception e => e -> SomeException
makeCatchable = spellExceptionToException

-- | Isomorphism.
msfToCoroutine  :: Monad m => MSF m a b -> a -> Coroutine (Request b a) m void
msfToCoroutine msf a0 = Coroutine . fmap (\(b, msf') -> Left $ Request b (msfToCoroutine msf')) $ (unMSF msf) a0
-- | Isomorphism.
coroutineToMsf :: Monad m => (a -> Coroutine (Request b a) m Void) -> MSF m a b
coroutineToMsf f = MSF $ \a0 ->
  f a0 & \co ->
    (resume co) <&> \case
      Right v -> absurd v
      Left (Request b k) -> (b, coroutineToMsf k)

type DynCollectionSF a b s m c =
  SF m
     (s, Event (IntMap (Task a b (StateT s m) c)), a)
     (s, Event (IntMap c), IntMap b)

-- | Manages a dynamic collection of tasks that mutate some state 's'.
-- Each task is identified by an integer. Sending a task
-- with the same identifier twice will overwrite the original.
dynCollection :: Monad m => DynCollectionSF a b s m c
dynCollection = readerS $ proc (dt, (s, tks, a)) -> do
  (s', (cs, b)) <- runStateS (runReaderS dynCollection') -< (s, (dt, (tks, a))) 
  returnA -< (s', cs, b)

partitionEithersIM :: IntMap (Either a b) -> (IntMap a, IntMap b)
partitionEithersIM = bimap (fmap \(Left x) -> x) (fmap \(Right x) -> x) . IntMap.partition isLeft

dynCollection' :: Monad m => SF (StateT s m) (Event (IntMap (Task a b (StateT s m) c)), a) (Event (IntMap c), IntMap b)
dynCollection' = feedback IntMap.empty $ proc ((e,a), kills) -> do
  let
    -- Delete the entries that terminated previously
    eKills = if IntMap.null kills then NoEvent else Event (`IntMap.difference` kills)
    -- Add the new Tasks, replacing duplicate entries
    e' = IntMap.union . fmap runTask <$> e
    e'' = mergeBy (.) e' eKills
  bcs <- rpSwitchB IntMap.empty -< (a, e'')
  let (bs, cs) = partitionEithersIM bcs
  returnA -< ((if null cs then Event cs else NoEvent, bs), cs)

generaliseSF :: Monad m => SF Identity a b -> SF m a b
generaliseSF = morphSF (pure . runIdentity)

-- | Move between monads in an SF, while keeping time information.
morphSF :: (Monad m2, Monad m1) => (forall c. m1 c -> m2 c) -> SF m1 a b -> SF m2 a b
-- I think this would be as safe as morphS, but who knows
morphSF nat = morphS (mapReaderT nat) 
