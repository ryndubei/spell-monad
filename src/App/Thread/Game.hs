{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
module App.Thread.Game (withGameThread, sendGameThread, takeGameThread, waitGameThread, mapGameThreadResult, GameThread) where

import Control.Monad.IO.Class
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Chronos
import Data.Profunctor
import Control.Concurrent
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Data.Maybe

maxDelay :: Num a => a
maxDelay = 10 * 10^(3 :: Int) -- 10ms in microseconds

-- | Has input 'u', output 's' and final result 'r'.
data GameThread r u s = forall u' s'. Semigroup u' => GameThread
  { gameThreadAsync :: !(Async r)
  , output :: !(TMVar s')
  , userInputs :: !(TMVar u')
  , rmapper :: s' -> s
  , lmapper :: u -> u'
  }

deriving instance Functor (GameThread r u)

mapGameThreadResult :: (r -> r') -> GameThread r u s -> GameThread r' u s
mapGameThreadResult f GameThread{..} =
  GameThread{gameThreadAsync = fmap f gameThreadAsync, ..}

instance Profunctor (GameThread r) where
  lmap f GameThread{..} = GameThread{lmapper = lmapper . f, ..}
  rmap = fmap

-- | Non-retrying, sends input 'u' to the GameThread.
sendGameThread :: GameThread r u s -> u -> STM ()
sendGameThread GameThread{userInputs, lmapper} =
  -- Note: this does not force the value stored by 'userInputs'.
  -- This is a deliberate choice: it guarantees that the transaction is fast
  -- (just like a queue), without having to provide a list that gets mconcatted
  -- away anyway.
  (\u -> tryTakeTMVar userInputs >>= maybe (writeTMVar userInputs u) (writeTMVar userInputs . (<> u))) . lmapper

-- | Retries until an output 's' is available from the GameThread, then
-- removes and returns 's'.
takeGameThread :: GameThread r u s -> STM s
takeGameThread GameThread{output, rmapper} =
  rmapper <$> takeTMVar output

-- | Retries until the GameThread exits.
waitGameThread :: GameThread r u s -> STM (Maybe SomeException)
waitGameThread GameThread{..} =
  either Just (const Nothing) <$> waitCatchSTM gameThreadAsync

-- | Run the passed coroutine as a GameThread. Just like BrickThread, does
-- not guarantee that the GameThread will be running for the entire continuation.
--
-- Does not require a MonadUnliftIO, because the natural transformation needn't
-- preserve state (it could be e.g. evaluating a StateT)
withGameThread
  :: forall m u s a r.
     (MonadIO m, Monoid u, Semigroup s)
  => (forall x. m x -> IO x)
  -> Coroutine (Request s (Timespan, u)) m r
  -> (GameThread r u s -> IO a)
  -> IO a
withGameThread tf tick k = do
  userInputs <- newEmptyTMVarIO
  output <- newEmptyTMVarIO

  let gameThread = tf $ pogoStick (handleTick userInputs output) tick

  withAsync
    gameThread
    \gameThreadAsync -> k GameThread{..}
  where
    rmapper = id
    lmapper = id
    handleTick u o (Request s cont) = do

      -- Update the game state on first opportunity
      liftIO . atomically $ writeTMVar o s

      (t, userInput) <- liftIO . stopwatch $ timeoutSTM maxDelay (takeTMVar u)

      cont (t, fromMaybe mempty userInput)

-- | Retry the given STM transaction for up to the given number of microseconds
-- ('1/10^6' seconds).
--
-- Will return a 'Just' if and only if the transaction succeeded:
-- the transaction will have had no effect if 'Nothing' is returned.
--
-- Will never time out if the transaction does not retry at all.
timeoutSTM :: Int -> STM a -> IO (Maybe a)
timeoutSTM micros stm = withAsync (threadDelay micros) \a -> atomically do
  result <- newTVar Nothing
  orElse
    (stm >>= writeTVar result . Just)
    (waitSTM a)
  readTVar result
  