{-# LANGUAGE NoImplicitPrelude #-}
module Prelude.Spell
  ( Spell
  , SpellError(..)
  , putChar
  , getChar
  , getLine
  , putStr
  , putStrLn
  -- * base
  , Functor(..)
  , Applicative(..)
  , Monad(..)
  -- * mtl
  , MonadError(..)
  ) where

import Prelude hiding (getLine, putStr, putStrLn, putChar, getChar)
import Control.Monad.Except
import Spell

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