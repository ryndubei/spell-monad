{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Untrusted
import Spell
import Spell.Eval
import Control.Monad.Trans.Free
import Data.Functor.Product
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Control.Monad
import Control.Exception
import Control.Monad.Trans.Class

sampleSpell :: Untrusted (Spell String)
sampleSpell = toUntrusted do
  firebolt
  firebolt
  c <- Spell.getChar
  pure $ c : ": abcdef"

main :: IO ()
main = runEvalUntrusted do
  let sampleSpellT = evalSpellUntrusted sampleSpell
  x <- go sampleSpellT
  liftIO $ withTrusted Nothing x $ wait >=> \case
    Right str -> putStrLn str
    Left ue -> withTrusted Nothing (fmap displayException ue) $ wait >=> \case
      Right e -> fail e
      Left _ -> fail "<exception in displayException>"
  where
    go s@(SpellT (FreeT (Pair (MaybeT m) _))) = do
      liftIO $ putStrLn "begin"
      m' <- m
      liftIO $ putStrLn "evaled"
      case m' of
        Nothing -> do
          liftIO $ putStrLn "polling again"
          go s
        Just x -> do
          liftIO $ putStrLn "got just"
          case x of
            (Pure z) -> do
              liftIO $ putStrLn "pure"
              pure z
            (Free f) -> do
              liftIO $ putStrLn "free"
              case f of
                Firebolt next -> do
                  liftIO $ putStrLn "Firebolt"
                  go (SpellT . join $ lift next)
                Face a b next -> do
                  liftIO $ putStrLn $ "Face " ++ show a ++ " " ++ show b
                  go (SpellT . join $ lift next)
                Catch {} -> do
                  liftIO . putStrLn $ "Catch (unimpl)"
                  go (liftF $ Throw (pure $ pure $ SomeException $ ErrorCall "catch unimplemented" ))
                Throw (Pair (MaybeT e) _) -> do
                  liftIO . putStrLn $ "Throw"
                  e' <- e
                  case e' of
                    Just e'' -> pure (fmap displayException e'')
                    Nothing -> do
                      liftIO $ putStrLn "polling exception again"
                      go s
                PutChar c next -> do
                  liftIO . putStrLn $ "PutChar " ++ show c
                  go (SpellT . join $ lift next)
                GetChar next -> do
                  liftIO . putStrLn $ "GetChar"
                  c <- liftIO Prelude.getChar
                  let next' = SpellT . join . lift $ fmap ($ c) next
                  go next'
