{-# LANGUAGE TypeData #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

-- # Concurrency as possession and monad transformers

-- TEXT: The door can only be opened with a switch from the other side.
-- Make the guard open it for you.

-- 'Poss' is equivalent to 'Async' from the 'async' package
-- 'Enemy' is a type tag for reflection on 'e'
-- 'PossessT e' allows for extra effects that depend on what 'e' is
possess :: (forall e. Enemy e -> Bool) -> (forall e. Enemy e -> PossessT e Spell a) -> Spell (Maybe (Poss a))

type data Guard

-- NOTE: not 100% sure if this kind of extensible GADT would actually work,
-- did not test
data instance Enemy Guard = Guard

-- | Set a target to try to navigate to. 
-- Returns the remaining displacement when done.
moveGuard :: Monad m => V2 -> PossessT Guard m V2

-- | True if at least one item was interacted with
interact :: Monad m => (Item -> Bool) -> PossessT Guard m Bool

waitPossess :: Poss a -> Spell a


main :: Spell ()
main = do
  p <- possess (\case Guard -> True; _ -> False)
    (\case
      Guard -> do
        t <- lift scan
        let switchLoc = someTreeSearchAlgorithmImplementedByPlayer t  
        d <- moveGuard switchLoc
        if (norm d <= 1)
          then do
            success <- interact (== Switch)
            unless success $ fail "Failed to interact with switch"
          else fail "Failed to move to switch"
      _ -> fail "impossible"
    )
  case p of
    Just p' -> waitPossess p
    Nothing -> fail "Failed to possess guard"
