{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}

module Prelude.Spell
  ( module Spell
  , module Spell.Exception
  , module Spell.IO
  -- * base
  , module Prelude
  , last
  , module Control.Exception
  ) where

import Spell
  ( Spell
  , firebolt
  , putChar
  , getChar
  , catch
  , throwSpell
  , inputTarget
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

  , last
  )

import GHC.Stack

--------------------------------------------------------------------------------
-- Prelude re-definitions
--------------------------------------------------------------------------------

-- | \(\mathcal{O}(n)\). Extract the last element of a list, which must be
-- finite and non-empty.
--
-- >>> last [1, 2, 3]
-- 3
-- >>> last [1..]
-- * Hangs forever *
-- >>> last []
-- *** Exception: Spell.Prelude.last: empty list
--
-- WARNING: This function is partial. You can use 'reverse' with case-matching,
-- 'uncons' or 'listToMaybe' instead.
last :: HasCallStack => [a] -> a
last = foldl (\_ x -> x) (error "Spell.Prelude.last: empty list")
