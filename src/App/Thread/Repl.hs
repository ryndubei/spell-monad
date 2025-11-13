{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImpredicativeTypes #-}

module App.Thread.Repl
  ( ReplThread
  , ReplStatus(..)
  , InterpretRequest(..)
  , withReplThread
  , submitRepl
  , replStatus
  , getReplResult
  , interruptRepl
  , takeInterpretRequest
  , sameStatus
  ) where

import Control.Concurrent.STM
import Control.Exception
import Spell (Spell)
import Language.Haskell.Interpreter
import Control.Concurrent.Async
import Control.Monad.Trans.Maybe
import Data.Void
import Control.Monad
import Data.Foldable
import Control.Monad.Fix
import Data.Bifunctor

-- | Compute an untrusted value on a separate thread.
-- offload :: HandleSpell :> es => Untrusted a -> (a -> )

-- | A request to carry out some side effects in the game.
data InterpretRequest =
  forall a. InterpretRequest
    { submitResponseHere :: TMVar (Either SomeException a -> STM ())
      -- ^ Exceptions in 'a' are expected and allowed: represents a
      -- lazy bottom value. SomeException when the computation terminated due
      -- to a thrown exception.
      -- Becomes empty if no longer needed (e.g. due to an interrupt):
      -- in that case should stop the Spell computation ASAP.
      -- (TODO: Becomes an AsyncException if a response is no longer needed?)
    , toInterpret :: Spell a
    }

-- | Guarantees that the InterpretRequest is invalidated when the continuation ends.
withInterpretRequest :: Spell a -> ((InterpretRequest, TMVar (Either SomeException a)) -> IO b) -> IO b
withInterpretRequest toInterpret =
  bracket
    do
      response <- newEmptyTMVarIO
      submitResponseHere <- liftIO newEmptyTMVarIO
      -- function will empty the TMVar after first submit
      atomically $ writeTMVar submitResponseHere (void . tryPutTMVar response >=> const (void $ tryTakeTMVar submitResponseHere))
      pure (InterpretRequest{..}, response)
    \(InterpretRequest{submitResponseHere}, _) -> void . atomically $ tryTakeTMVar submitResponseHere

data ReplThread = ReplThread
  { toCompile :: !(TMVar String)
  -- , spellOutputQueue :: !(TQueue Text)

  -- we are not passing this as a strict Text, because the returned value could
  -- throw an exception, which throws another exception, and so on ad infinitum.
  -- Also, we can have a type with a possibly-infinite, lazy Show instance.
  -- (e.g. [1..])
  -- That would not work well if we return a Text.
  -- But also: having the UI thread handle exceptions would be fiddly and
  -- error-prone, and interrupts would have to be handled in two places.
  -- A queue of characters is the answer.
  , replResult :: !(TQueue Char)
  , interpretRequest :: !(TMVar InterpretRequest)
  , replThreadAsync :: !(Async InterpreterError)
  , replInterrupt :: !(TVar Bool)
      -- ^ Tells the ReplThread that it must interrupt everything it is currently
      -- blocked on and accept the next input in 'toCompile'.

  , replInitialised :: !(TVar Bool)
      -- ^ Should have a loading screen preventing the user from sending
      -- anything to the REPL until it is initialised here, but this is not
      -- strictly necessary.

  , replBlocked :: !(TVar Bool)
      -- ^ Should not send any input until this is False.
  }

-- Should only set replInitialised to True when this is done
initialiseInterpreter :: MonadInterpreter m => m ()
initialiseInterpreter = do
  setImports ["Prelude.Spell"]
  
  -- The first interpreted value takes a lot longer than subsequent values,
  -- hence the repl isn't really initialised until this is done
  u <- interpret "putChar 'a'" (as :: Spell ())
  void . liftIO $ evaluate u

withReplThread :: (ReplThread -> IO a) -> IO a
withReplThread k = do
  toCompile <- newEmptyTMVarIO
  replResult <- newTQueueIO
  interpretRequest <- newEmptyTMVarIO
  replInterrupt <- newTVarIO False
  replInitialised <- newTVarIO False
  replBlocked <- newTVarIO False

  -- TODO: allow binding variables
  -- (will most likely implement by transforming subsequent lines into functions
  -- taking the bound variables as arguments)
  withAsync
    do
      e <- runInterpreter $ do
        initialiseInterpreter
        liftIO . atomically $ writeTVar replInitialised True

        forever . runMaybeT $ do

          s <- liftIO $ atomically do
            -- Nothing to interrupt. If the target of the interrupt were to be 
            -- whatever is in toCompile, then toCompile would be empty by
            -- atomicity of interruptRepl.
            writeTVar replInterrupt False
            takeTMVar toCompile

          -- Ugly hack, probably subject to escapes. Low priority because this
          -- is a prototype + whatever the player inputs is sandboxed anyway.
          -- The only danger is the player receiving a false positive on the
          -- syntax being valid.
          -- TODO: more reliable method at some point
          let s' = parens s ++ " :: Spell ()"

          lift (typeChecksWithDetails s') >>= \case
            Left errs -> do
                liftIO $ atomically do
                  b <- readTVar replInterrupt
                  unless b do
                    -- putting everything into one big 'atomically' is okay because errMsg
                    -- should be finite
                    traverse_ (writeTQueue replResult) (unlines $ map errMsg errs)
                    writeTVar replBlocked False -- only need to write if not interrupted
                hoistMaybe Nothing
            Right _ -> pure ()

          -- TODO: return values with a Show instance should be printed
          -- except for (), which should not be
          spell <- lift $ interpret s' (as :: Spell ())

          -- Get the result of interpreting 'spell', exiting early if interrupted
          -- at any point in the process
          u <- (hoistMaybe <=< liftIO . withInterpretRequest spell) \(req, response) -> runMaybeT do
            -- Send the InterpretRequest, unless we were already interrupted
            hoistMaybe =<< (liftIO . atomically) do
              b <- readTVar replInterrupt
              if b
                then pure Nothing -- early exit
                else do
                  writeTMVar interpretRequest req
                  pure $ Just ()
            -- Wait for the spell to be interpreted
            hoistMaybe =<< (liftIO . atomically) do
              orElse
                -- either we are interrupted (early exit):
                (readTVar replInterrupt >>= check >> pure Nothing)
                -- or we get the response:
                -- (UncaughtSpellException is a wrapper to not mistake an async
                -- exception for one thrown by the player)
                (Just . second (mapException UncaughtSpellException) <$> takeTMVar response)
                -- otherwise block

          liftIO $ catch
            (case u of
              -- evaluate to WHNF, do nothing if all okay
              Right () -> pure ()
              Left e -> throwIO (UncaughtSpellException e)
            )
            -- note: lazy pattern as UncaughtSpellException is strict
            -- recursive, because exceptions can throw exceptions which throw exceptions...
            (fix \this (~(UncaughtSpellException ex)) -> do
              let msg = mapException UncaughtSpellException (displayException ex)
              catch
                -- need MaybeT here again because catch is in IO
                (void . runMaybeT . for_ msg $ \c -> do
                  _ <- liftIO . evaluate $ mapException UncaughtSpellException c
                  hoistMaybe =<< (liftIO . atomically) do
                    b <- readTVar replInterrupt
                    if b
                      then pure Nothing -- early exit
                      else do
                        writeTQueue replResult c
                        pure (Just ())
                )
                this
              )
          -- done, unblock unless we were interrupted (meaning already unblocked)
          liftIO $ atomically do
            interrupted <- readTVar replInterrupt
            unless interrupted $ writeTVar replBlocked False

      pure $ either id absurd e
    \replThreadAsync -> k ReplThread{..}

-- | An equivalence relation for ReplStatus
sameStatus :: ReplStatus -> ReplStatus -> Bool
sameStatus Initialising Initialising = True
sameStatus Unblocked Unblocked = True
sameStatus Blocked Blocked = True
sameStatus (Dead _) (Dead _) = True
sameStatus _ _ = False

newtype UncaughtSpellException = UncaughtSpellException SomeException deriving Show
instance Exception UncaughtSpellException

-- | Retry until the ReplThread has a valid InterpretRequest.
takeInterpretRequest :: ReplThread -> STM InterpretRequest
takeInterpretRequest ReplThread{..} = do
  req@InterpretRequest{submitResponseHere} <- takeTMVar interpretRequest
  b <- isEmptyTMVar submitResponseHere
  check (not b)
  pure req

-- | Retries until the ReplThread can accept more input.
submitRepl :: ReplThread -> String -> STM ()
submitRepl ReplThread{replResult, replBlocked, toCompile} s = do
  b <- readTVar replBlocked
  check (not b)
  -- submitting something new for compilation invalidates whatever was in the result:
  _ <- flushTQueue replResult
  writeTMVar toCompile s
  writeTVar replBlocked True

data ReplStatus = Initialising | Unblocked | Blocked | Dead (Either SomeException InterpreterError)

replStatus :: ReplThread -> STM ReplStatus
replStatus ReplThread{..} = do
  asyncResult <- pollSTM replThreadAsync
  case asyncResult of
    Nothing -> do
      initialised <- readTVar replInitialised
      blocked <- readTVar replBlocked 
      pure $ case (initialised, blocked) of
        (False, _) -> Initialising
        (True, True) -> Blocked
        (True, False) -> Unblocked
    Just (Right e) -> pure (Dead (Right e))
    Just (Left e) -> pure (Dead (Left e))

-- | Get the next character from the ReplThread's output.
-- Retries until there is a result, or the thread is unblocked
-- (therefore can no longer send an output)
getReplResult :: ReplThread -> STM (Maybe Char)
getReplResult ReplThread{replBlocked, replResult} = do
  orElse
    (Just <$> readTQueue replResult)
    (readTVar replBlocked >>= check . not >> pure Nothing)

-- | Immediately interrupt whatever is currently blocking the REPL, allowing it
-- to accept more inputs. Never retries.
interruptRepl :: ReplThread -> STM ()
interruptRepl ReplThread{toCompile, replResult, replInterrupt, replBlocked} = do
  _ <- tryTakeTMVar toCompile
  _ <- flushTQueue replResult -- ^ if interrupted, cease all printing
  writeTVar replBlocked False
  writeTVar replInterrupt True
  pure ()
