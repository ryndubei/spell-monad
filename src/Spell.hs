{-# LANGUAGE NoImplicitPrelude #-}
module Spell (Spell(..), SpellF(..), SpellError(..), MonadSpell, putChar, getChar, getLine, putStr, putStrLn) where

import Control.Monad.Except
import Control.Monad.Free
import Prelude hiding (putStrLn, putStr, getLine, getChar, putChar, print)

type MonadSpell m = (MonadFree SpellF m, MonadError SpellError m)

newtype Spell a = Spell { unSpell :: ExceptT SpellError (Free SpellF) a }
  deriving (Functor, Applicative, Monad, MonadFree SpellF, MonadError SpellError)

data SpellError = OutOfSideEffects | OutOfMemory
  deriving (Eq, Show, Ord, Bounded, Enum)

data SpellF next = PutChar Char next | GetChar (Char -> next)

deriving instance Functor SpellF

putChar :: Char -> Spell ()
putChar c = liftF (PutChar c ())

getChar :: Spell Char
getChar = liftF (GetChar id)

getLine :: Spell String
getLine = do
  c <- getChar
  if c == '\n'
    then pure []
    else fmap (c :) getLine

putStr :: String -> Spell ()
putStr = mapM_ putChar

putStrLn :: String -> Spell ()
putStrLn str = putStr str >> putChar '\n'
