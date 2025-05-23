{-# LANGUAGE NoImplicitPrelude #-}
module Spell (Spell(..), SpellF(..), SpellError(..), putChar, getChar) where

import Control.Monad.Except
import Control.Monad.Free
import Prelude hiding (getChar, putChar)

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
