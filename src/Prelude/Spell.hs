{-# LANGUAGE NoImplicitPrelude #-}
module Prelude.Spell
  ( Spell
  , SpellException(..)
  , putChar
  , getChar
  , getLine
  , putStr
  , putStrLn
  -- * base
  , Functor(..)
  , Applicative(..)
  , Monad(..)
  -- * exceptions
  , MonadThrow(..)
  , MonadCatch(..)
  , MonadMask(..)
  ) where

import Prelude hiding (getLine, putStr, putStrLn, putChar, getChar)
import Spell
import Control.Monad.Catch

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