{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module Spell.Eval (evaluateSpell) where

import Spell (Spell (..), SpellT(..), unSpell, SpellF(..))
import Control.Monad.Trans.Free
import Control.Exception
import Control.Monad.Trans.Except
import Data.Functor.Identity
import Data.Coerce
import Control.Monad.Trans.Class
import Data.Bifunctor
import Control.Monad

-- Guarantees that any exceptions in _strict_ values passed to SpellF effects
-- (and in SpellF itself) are caught.
--
-- TODO: figure out whether all values passed with Spell effects will be strict
-- or themselves 'Spell a' values for some 'a'. It sounds plausible, and would
-- be a useful property if true.
--
-- Note: does not interpret anything, including neither 'catch' nor 'throwSpell'
evaluateSpell :: forall a. Spell a -> SpellT (ExceptT SomeException IO) a
evaluateSpell = SpellT . evalBranches . evalWhnf . unSpell

-- | Ensures the FreeT itself does not throw.
evalWhnf :: FreeT (SpellF m) Identity a -> FreeT (SpellF m) (ExceptT SomeException IO) a 
-- foldFreeT evaluates at every iteration:
-- 1. the side-effect returning the FreeF (by performing it)
-- 2. the FreeF to WHNF (to decide whether it's Pure/Free)
-- Everything else depends on the strictness of its arguments.
-- As the monad of 's' is Identity, which is a newtype, and FreeT is also a
-- newtype, this just evaluates the entire thing to WHNF.
-- So, all we need to do to stay ahead of foldFreeT is calling tryEvaluate
-- before letting it proceed.
evalWhnf = hoistFreeT (tryEvaluate . runIdentity)

-- | Ensures the (strict) arguments passed to Spell effects do not throw by
-- moving the exceptions to the ExceptT layer.
-- Recurses into higher-order effects, like 'Catch'.
evalBranches :: FreeT (SpellF Identity) (ExceptT SomeException IO) a -> FreeT (SpellF (ExceptT SomeException IO)) (ExceptT SomeException IO) a
-- May require 'unsafeInterleaveIO' later if any future effects take lazy values
-- as arguments
evalBranches = iterTM $
  -- Any exceptions in values passed as arguments fail in the \case,
  -- and therefore get caught by tryEvaluate
  join . lift . tryEvaluate . mapException UncaughtSpellException . \case
    Firebolt next -> liftF (Firebolt ()) >> next
    Face (!a, !b) next -> liftF (Face (a,b) ()) >> next
    Catch s h next ->
      let s' = evaluateSpell (coerce s)
          h' = fmap (evaluateSpell . coerce) h
       in liftF (Catch s' h' id) >>= next
    Throw !ex -> liftF (Throw ex)
    PutChar !c next -> liftF (PutChar c ()) >> next 
    GetChar next -> liftF (GetChar id) >>= next

tryEvaluate :: a -> ExceptT SomeException IO a
tryEvaluate
  = ExceptT
  . fmap (first coerce)
  . try @UncaughtSpellException
  . evaluate
  . mapException UncaughtSpellException

newtype UncaughtSpellException = UncaughtSpellException SomeException deriving Show

instance Exception UncaughtSpellException
