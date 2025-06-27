{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingVia #-}
module Orphans () where

import Control.Monad.Log
import Control.Monad.Schedule.Class
import Control.Monad.Reader
import Data.Bifunctor

instance (Monad m, MonadSchedule m) => MonadSchedule (LoggingT msg m) where
  -- generalised newtype deriving and coerce both complain, so has to be done explicitly
  schedule xs = s'
    where
      schedule' = schedule @(ReaderT (Handler m msg) m)
      s = schedule' (fmap (\(LoggingT x) -> x) xs)
      s' = second (map LoggingT) <$> LoggingT s
