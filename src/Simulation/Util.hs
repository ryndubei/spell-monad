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

module Simulation.Util (dynCollection, generaliseSF, morphSF, ActionTag, Epoch) where

import FRP.BearRiver
import Control.Monad.State.Strict (StateT)
import Control.Monad.Trans.MSF (mapReaderT, runReaderS, readerS, runStateS)
import Data.Functor.Identity
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Either
import Data.Bifunctor
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Data.Unique
import Data.Sequence (Seq)
import Data.Int (Int64)
import GHC.Generics

data ActionTag = ActionTag
  { senderId :: Unique
  , actionEpoch :: Epoch
  } deriving (Eq, Ord, Generic, Hashable)

type Action a b s m c = Task a b (StateT s m) c

actionRunner :: SF m (s, Event (Seq (ActionTag, Action a b s m c))) (s, HashMap ActionTag Int, Event (HashMap ActionTag c))
actionRunner = undefined

-- | Absolute current time in an arbitrary unit. Ord instance respects ordering of time.
newtype Epoch = Epoch Int64
  deriving stock (Eq, Ord, Generic)
  deriving newtype Hashable

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
