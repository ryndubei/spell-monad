{-# LANGUAGE NoImplicitPrelude #-}
module Prelude.Spell
  ( module Spell
  -- * base
  , module Prelude
  -- * exceptions
  , module Control.Monad.Catch
  ) where

import Spell
  ( Spell
  , firebolt
  , face
  )
import Control.Monad.Catch
  ( MonadThrow(..)
  , MonadCatch(..)
  )
import Prelude hiding
  ( IO
  , putChar
  , putStr
  , putStrLn
  , print
  , getChar
  , getLine
  , getContents
  , interact
  , FilePath
  , readFile
  , writeFile
  , appendFile
  , readIO
  , readLn
  , IOError
  , ioError
  , userError
  )
