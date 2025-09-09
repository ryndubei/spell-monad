{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
module Orphans () where

import Control.Monad.Schedule.Class
import Control.Monad.Reader
import Data.Bifunctor
import Data.Automaton.Trans.Except
import Control.Monad.Trans.Resource
import UnliftIO (MonadUnliftIO(withRunInIO))
import Data.Profunctor
import FRP.Rhine (Rhine)
import FRP.Rhine.Reactimation.Combinators

instance (MonadUnliftIO m, MonadSchedule m) => MonadSchedule (ResourceT m) where
  schedule xs = withRunInIO \z -> do
    let xs' = fmap z xs
    xs'' <- schedule xs'
    pure $ second (map liftIO) xs''

instance MonadIO m => MonadIO (AutomatonExcept a b m) where
  liftIO = lift . liftIO

instance Monad m => Functor (Rhine m cl a) where
  fmap = flip (@>>^)

instance Monad m => Profunctor (Rhine m cl) where
  lmap = (^>>@)
  rmap = fmap
