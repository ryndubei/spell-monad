module Spell.Exception (SpellException(..)) where

import Control.Exception

data SpellException = OutOfSideEffects deriving Show

instance Exception SpellException
