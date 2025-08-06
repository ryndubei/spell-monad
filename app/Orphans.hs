{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
module Orphans () where

import qualified Control.Monad.Log as Log
import Control.Monad.Schedule.Class
import Control.Monad.Reader
import Data.Bifunctor
import Data.Automaton.Trans.Except
import Control.Monad.Trans.Resource
import UnliftIO (MonadUnliftIO(withRunInIO))
import Data.Profunctor
import FRP.Rhine (Rhine)
import FRP.Rhine.Reactimation.Combinators

instance (Monad m, MonadSchedule m) => MonadSchedule (Log.LoggingT msg m) where
  -- generalised newtype deriving and coerce both complain, so has to be done explicitly
  schedule xs = s'
    where
      schedule' = schedule @(ReaderT (Log.Handler m msg) m)
      s = schedule' (fmap (\(Log.LoggingT x) -> x) xs)
      s' = second (map Log.LoggingT) <$> Log.LoggingT s

instance (MonadUnliftIO m, MonadSchedule m) => MonadSchedule (ResourceT m) where
  schedule xs = withRunInIO \z -> do
    let xs' = fmap z xs
    xs'' <- schedule xs'
    pure $ second (map liftIO) xs''

instance MonadResource m => MonadResource (Log.LoggingT msg m) where
  liftResourceT = lift . liftResourceT

instance MonadIO m => MonadIO (AutomatonExcept a b m) where
  liftIO = lift . liftIO

instance Monad m => Functor (Rhine m cl a) where
  fmap = flip (@>>^)

instance Monad m => Profunctor (Rhine m cl) where
  lmap = (^>>@)
  rmap = fmap
