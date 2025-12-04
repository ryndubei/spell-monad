{-# LANGUAGE NoImplicitPrelude #-}
module Spell.IO (putStrLn, getLine, putStr, print) where

import Spell
import Data.Eq
import Data.String
import Control.Monad
import Control.Applicative
import Prelude (Show(..), (.))

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

print :: Show a => a -> Spell ()
print = putStrLn . show
