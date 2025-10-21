{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}

module Prelude.Spell
  ( module Spell
  , module Spell.Exception
  , module Spell.IO
  -- * base
  , module Prelude
  , module Control.Exception
  ) where

import Spell
  ( Spell
  , firebolt
  , face
  , putChar
  , getChar
  , catch
  , throwSpell
  )
import Spell.Exception
import Spell.IO
import Control.Exception
  -- a set of exceptions that may be encountered by the user, and are safe to
  -- be caught
  ( NoMethodError(..)
  , NonTermination(..)
  , PatternMatchFail(..)
  , RecConError(..)
  , RecSelError(..)
  , RecUpdError(..)
  , ErrorCall(..)
  , pattern ErrorCall
  , ArithException(..)
  , AssertionFailed(..)
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
