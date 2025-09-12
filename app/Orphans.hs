{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
module Orphans () where

import Control.Monad.Reader
import Data.Automaton.Trans.Except
import Data.Profunctor
import FRP.Rhine (Rhine)
import FRP.Rhine.Reactimation.Combinators

instance MonadIO m => MonadIO (AutomatonExcept a b m) where
  {-# INLINE liftIO #-}
  liftIO = lift . liftIO

instance Monad m => Functor (Rhine m cl a) where
  {-# INLINE fmap #-}
  fmap = flip (@>>^)

instance Monad m => Profunctor (Rhine m cl) where
  {-# INLINE lmap #-}
  lmap = (^>>@)
  {-# INLINE rmap #-}
  rmap = fmap
