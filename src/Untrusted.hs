{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE BlockArguments #-}
module Untrusted (Untrusted, toUntrusted, withTrusted, untrustedCommSome) where

import Control.DeepSeq
import Control.Exception
import Control.Concurrent.Async
import Control.Monad
import Data.Int
import System.Mem
import Data.Foldable
import Data.Some.Newtype
import Data.Functor.Compose
import Foreign.StablePtr
import Control.Concurrent

-- Deliberately not a newtype, so that it cannot be forced.
data Untrusted a = Untrusted a deriving Functor

instance Applicative Untrusted where
  {-# INLINE pure #-}
  pure = toUntrusted
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Monad Untrusted where
  -- 'Untrusted ⊥' should be indistinguishable from any other Untrusted value 
  -- in pure code.
  -- Therefore we have to re-wrap the Untrusted after applying f, since f
  -- may be strict.
  {-# INLINE (>>=) #-}
  (>>=) (Untrusted a) f = Untrusted (let Untrusted b = f a in b)

{-# INLINE toUntrusted #-}
toUntrusted :: a -> Untrusted a
toUntrusted = Untrusted

newtype SomeImpreciseException = SomeImpreciseException SomeException deriving Show

instance Exception SomeImpreciseException

-- | Evaluate an Untrusted value on a separate thread. The Async will either
-- return the caught imprecise exception or a fully-forced value according to
-- the NFData instance. Obviously, the NFData instance must be trusted.
{-# INLINE withTrusted #-}
withTrusted
  :: NFData a
  => Maybe Int64 -- ^ Allocation limit in bytes. Disabled if Nothing.
  -> Untrusted a
  -> (Async (Either (Untrusted SomeException) a) -> IO b)
  -> IO b
withTrusted allocationLimit (Untrusted a) = withAsync do
  catch
    do
      -- https://well-typed.com/blog/2024/01/when-blocked-indefinitely-is-not-indefinite/
      -- https://gitlab.haskell.org/ghc/ghc/-/issues/10793
      -- This thread (and therefore any thread blocked on it) should never
      -- receive a NonTermination/BlockedIndefinitelyOnSTM/etc exception from
      -- the RTS because we are evaluating untrusted user input here, which
      -- could be non-terminating. Instead, the Async will have to be killed by hand.
      bracket
        (newStablePtr =<< myThreadId)
        freeStablePtr
        \_ -> do
          traverse_ (setAllocationCounter >=> const enableAllocationLimit) allocationLimit
          catch
            (fmap Right . evaluate . mapException SomeImpreciseException $ force a)
            \(SomeImpreciseException e) -> pure $ Left (toUntrusted e)
    \AllocationLimitExceeded -> pure $ Left (toUntrusted (SomeException AllocationLimitExceeded))

-- | Hidden types are preserved.
{-# INLINE untrustedCommSome #-}
untrustedCommSome :: Untrusted (Some f) -> Some (Compose Untrusted f)
-- since Some is a newtype, this pattern match is lazy on the contents,
-- so untrustedCommSome a `seq` () = ()
untrustedCommSome (Untrusted (Some a)) = Some (Compose (Untrusted a))
