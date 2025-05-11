{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module Main (main) where

import Spell (Spell(..), SpellF(..), SpellError(..))
import Control.Exception
import Control.Monad.Free
import Control.Monad.Except
import Data.IORef
import Language.Haskell.Interpreter
import System.IO
import System.Console.ANSI

maxSideEffects :: Int
maxSideEffects = 32

interpretSpell :: Spell a -> IO (Either SpellError a)
interpretSpell ~(Spell s) = catch
  itp
  \case
    -- for later: HeapOverflow should be rethrown to the thread running
    -- interpretSpell if caught elsewhere
    HeapOverflow -> pure (Left OutOfMemory)
    StackOverflow -> pure (Left OutOfMemory)
    e -> throwIO e
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
            else pure (Left OutOfSideEffects))
        (runExceptT s)
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

testSpellInterpreted :: String
testSpellInterpreted = unlines
  [ "let"
  , "  repeatLn :: Spell.Spell ()"
  , "  repeatLn = do"
  , "    Spell.putStrLn \"INPUT (<=6 chars)\""
  , "    str <- Spell.getLine"
  , "    Spell.putStrLn str"
  , "in repeatLn"
  ]

main :: IO ()
main = do
  (itpResult :: Either InterpreterError (Spell ())) <- runInterpreter do
    setImports ["Spell"]
    liftIO $ hPutStrLn stderr "Interpreter initialised"
    interpret testSpellInterpreted (as :: Spell ())
  (s :: Spell ()) <- either (fail . displayException) pure itpResult
  sResult <- interpretSpell s
  print sResult
