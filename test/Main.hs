{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module Main (main) where

import Spell (Spell(..), SpellF(..), SpellException(..))
import Control.Exception
import Control.Monad.Free
import Data.IORef
import Language.Haskell.Interpreter
import System.IO
import System.Console.ANSI
import Data.Maybe

maxSideEffects :: Int
maxSideEffects = 32

-- idea: annotate user-provided values with
-- data Untrusted a = Untrusted a
-- that has instances for monad etc
--
-- toTrusted :: DeepSeq a => Untrusted a -> Either SomeException a

interpretSpell :: Spell a -> IO (Either SomeException a)
interpretSpell ~(Spell s) = catch
  (fmap Right itp)
  \case
    e | Just (SomeAsyncException e') <- fromException e -> throwIO e'
    e -> pure (Left e)
  where
    itp = do
      mana <- newIORef maxSideEffects
      iterM
        (\s' -> do
          mana' <- readIORef mana
          if mana' > 0
            then do
              modifyIORef' mana (subtract 1)
              itr s'
            else throwIO OutOfSideEffects)
        s
    itr = \case
      PutChar c next -> do
        setSGR [SetColor Foreground Dull Blue]
        putChar c
        setSGR [Reset]
        next
      GetChar next -> do
        setSGR [SetColor Foreground Dull Red]
        hFlush stdout
        c <- getChar
        setSGR [Reset]
        next c
      -- Note: prefer using actual exceptions instead of Either 
      -- because we want to allow the user to catch e.g.
      -- divisions by zero
      Catch a h -> catch a (\e -> fromMaybe (throwIO e) (h e))


testSpellInterpreted :: String
testSpellInterpreted = unlines
  [ "let"
  , "  repeatLn :: Spell ()"
  , "  repeatLn = do"
  , "    putStrLn \"INPUT (<=6 chars)\""
  , "    str <- getLine"
  , "    putStrLn str"
  , "in repeatLn"
  ]

main :: IO ()
main = do
  (itpResult :: Either InterpreterError (Spell ())) <- runInterpreter do
    set [languageExtensions := [NoImplicitPrelude]]
    setImports ["Prelude.Spell"]
    liftIO $ hPutStrLn stderr "Interpreter initialised"

    interpret testSpellInterpreted (as :: Spell ())
  (s :: Spell ()) <- either (fail . displayException) pure itpResult
  sResult <- interpretSpell s
  print sResult
