{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE BlockArguments #-}
module Untrusted (Untrusted, toUntrusted, withTrusted) where

import Control.DeepSeq
import Control.Exception
import Control.Concurrent.Async
import Control.Monad
import Data.Int
import System.Mem
import Data.Foldable

-- Deliberately not a newtype, so that it cannot be forced.
data Untrusted a = Untrusted a deriving Functor

instance Applicative Untrusted where
  pure = toUntrusted
  (<*>) = ap

instance Monad Untrusted where
  -- 'Untrusted ⊥' should be indistinguishable from any other Untrusted value 
  -- in pure code.
  -- Therefore we have to re-wrap the Untrusted after applying f, since f
  -- may be strict.
  (>>=) (Untrusted a) f = Untrusted (let Untrusted b = f a in b)

toUntrusted :: a -> Untrusted a
toUntrusted = Untrusted

newtype SomeImpreciseException = SomeImpreciseException SomeException deriving Show

instance Exception SomeImpreciseException

-- | Evaluate an Untrusted value on a separate thread. The Async will either
-- return the caught imprecise exception or a fully-forced value according to
-- the NFData instance. Obviously, the NFData instance must be trusted.
withTrusted
  :: NFData a
  => Maybe Int64 -- ^ Allocation limit in bytes. Disabled if Nothing.
  -> Untrusted a
  -> (Async (Either SomeException a) -> IO b)
  -> IO b
withTrusted allocationLimit (Untrusted a) = withAsync do
  catch
    do
      traverse_ (setAllocationCounter >=> const enableAllocationLimit) allocationLimit
      catch
        (fmap Right . evaluate . mapException SomeImpreciseException $ force a)
        \(SomeImpreciseException e) -> pure $ Left e
    \AllocationLimitExceeded -> pure $ Left (SomeException AllocationLimitExceeded)
