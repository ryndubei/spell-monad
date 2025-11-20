module Spell.Exception (SpellException(..)) where

import Control.Exception
import Spell (spellExceptionToException, spellExceptionFromException)

data SpellException = OutOfSideEffects deriving Show

instance Exception SpellException where
  toException = spellExceptionToException
  fromException = spellExceptionFromException
