{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module App.Thread.Repl
  ( ReplThread
  , ReplStatus(..)
  , InterpretRequest(..)
  , withReplThread
  , submitRepl
  , replStatus
  , getReplResult
  , interruptRepl
  ) where

import Control.Concurrent.STM
import Control.Exception
import Spell
import Language.Haskell.Interpreter
import Type.Reflection
import Control.Concurrent.Async
import Control.Monad.Trans.Maybe
import Data.Void
import Control.Monad
import Data.Foldable
import Control.Monad.Fix

-- | A request to carry out some side effects in the game.
data InterpretRequest =
  forall a. Typeable a => InterpretRequest
    { submitResponseHere :: TMVar (a -> STM ())
      -- ^ Exceptions in 'a' are allowed.
      -- Becomes empty if no longer needed (e.g. due to an interrupt):
      -- in that case should stop the Spell computation ASAP.
      -- (TODO: Becomes an AsyncException if a response is no longer needed?)
    , toInterpret :: Spell a
    }

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
        setImports ["Prelude.Spell"]
        liftIO . atomically $ writeTVar replInitialised True
        forever do

          s <- liftIO $ atomically do
            -- Nothing to interrupt. If the target of the interrupt were to be 
            -- whatever is in toCompile, then toCompile would be empty by
            -- atomicity of interruptRepl.
            writeTVar replInterrupt False
            readTMVar toCompile

          -- Ugly hack, probably subject to escapes. Low priority because this
          -- is a prototype + whatever the player inputs is sandboxed anyway.
          -- The only danger is the player receiving a false positive on the
          -- syntax being valid.
          -- TODO: more reliable method at some point
          let s' = parens s ++ " :: Spell ()"

          typeChecksWithDetails s' >>= \case
            Left errs -> do
                liftIO $ atomically do
                  b <- readTVar replInterrupt
                  -- putting everything into one big 'atomically' is okay because errMsg
                  -- should be finite
                  unless b $ traverse_ (writeTQueue replResult) (unlines $ map errMsg errs)

            Right t -> case t of
              -- TODO: return values with a Show instance should be printed
              -- except for (), which should not be
              "Spell ()" -> do
                spell <- interpret s' (as :: Spell ())

                response <- liftIO newEmptyTMVarIO
                submitResponse <- liftIO newEmptyTMVarIO
                liftIO . atomically $ writeTMVar submitResponse (void . tryPutTMVar response >=> const (void $ takeTMVar submitResponse))

                -- Submit the spell to be interpreted
                interrupted1 <- liftIO $ atomically do
                  b <- readTVar replInterrupt
                  if b
                    then pure True
                    else do
                      writeTMVar interpretRequest $ InterpretRequest submitResponse spell
                      pure False

                -- Wait for the spell to be interpreted
                unless interrupted1 $ liftIO do
                  -- UncaughtSpellException is a wrapper to not mistake an async
                  -- exception for one thrown by the player
                  mu <- fmap (mapException UncaughtSpellException) <$> atomically do
                    orElse
                      -- either we are interrupted:
                      (readTVar replInterrupt >>= check >> void (takeTMVar submitResponse) >> pure Nothing)
                      -- or we get the response:
                      (Just <$> takeTMVar response)
                      -- otherwise block

                  case mu of
                    Nothing -> pure ()
                    Just u ->
                      catch
                        -- evaluate to WHNF, do nothing if all okay
                        (evaluate u)
                        -- note: lazy pattern
                        -- recursive, because exceptions can throw exceptions which throw exceptions...
                        (fix \this (~(UncaughtSpellException ex)) -> do
                          let msg = mapException UncaughtSpellException displayException ex
                          catch
                            (void . runMaybeT . for_ msg $ liftIO . evaluate >=> \c -> do
                              interrupted2 <- liftIO $ atomically do
                                b <- readTVar replInterrupt
                                if b
                                  then pure True
                                  else do
                                    writeTQueue replResult c
                                    pure False
                              -- early exit if interrupted
                              when interrupted2 $ hoistMaybe Nothing
                            )
                            this
                        )

              _ -> liftIO . throwIO $ userError "impossible? (expr) :: Spell () typechecks, but isn't of type Spell ()."

      pure $ either id absurd e
    \replThreadAsync -> k ReplThread{..}

newtype UncaughtSpellException = UncaughtSpellException SomeException deriving Show
instance Exception UncaughtSpellException

-- | Retries until the ReplThread can accept more input.
submitRepl :: ReplThread -> String -> STM ()
submitRepl ReplThread{replResult, replBlocked, toCompile} s = do
  b <- readTVar replBlocked
  check (not b)
  -- submitting something new for compilation invalidates whatever was in the result:
  _ <- flushTQueue replResult
  writeTMVar toCompile s
  writeTVar replBlocked True

data ReplStatus = Initialising | Initialised | Dead (Either SomeException InterpreterError)

replStatus :: ReplThread -> STM ReplStatus
replStatus ReplThread{replThreadAsync, replInitialised} = do
  asyncResult <- pollSTM replThreadAsync
  case asyncResult of
    Nothing -> do
      a <- readTVar replInitialised
      pure $ if a then Initialised else Initialising
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
