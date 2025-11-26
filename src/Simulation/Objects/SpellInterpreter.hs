{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Simulation.Object.SpellInterpreter (spellInterpreterObj, module Simulation.Objects.SpellInterpreter.Types) where

import FRP.BearRiver
import Simulation.Objects
import Control.Exception
import Data.Functor.Product
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Spell (SpellT(..), SpellF (..))
import Control.Monad.Trans.State
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Simulation.Objects.Firebolts
import Control.Monad.Trans.Free
import Control.Monad
import Linear.V2
import Control.Lens
import Linear.Epsilon
import Spell.Exception
import Control.Applicative

import Simulation.Objects.SpellInterpreter.Types
import Simulation.Objects.Player.Types

spellInterpreterObj = undefined

spellInterpreter
  :: forall e m r.
  (Monad m, Monoid (ObjsInput e m r))
  => SF m
    (Double, Event SomeException, ObjInput Player, ObjsOutput e m r)
    (ObjsInput e m r, Double, Event r, Event Char)
spellInterpreter = proc (r, e, o, objsOutput) -> do
  (mana', result, objsInput, stdout) <- continuousInterpreter -< ((r, e, objsOutput), o)
  returnA -< (objsInput, mana', result, stdout)
  where
    nothingInterpreter = arr (\(m, _, _, _) -> (m, NoEvent, mempty, NoEvent))

    -- Switch interpreter sessions in the background.
    continuousInterpreter = proc ((cost, e, objsOutput), o) -> do
      isf <- newConstantInterpreter -< undefined
      -- Ignore external exception at time 0: it's meant for the code that is already
      -- running, not the code that we are switching to.
      let isf' = fmap ((\(a,_,c,d) -> (a,NoEvent,c,d)) >=-) isf
      -- Prioritise the user interrupt, since it cannot be caught
          e' = (SomeException UserInterrupt <$ isf) <|> e
      rSwitch nothingInterpreter -< ((cost, e', objsOutput,playerStdin o), isf')

    newConstantInterpreter = proc replInput -> do
      returnA -< maybe nothingInterpreter (\(s, collapse, collapseException) -> proc (r,e,objsOutput,playerStdin) -> do
        (mana', result, objsInput, stdout) <- constantInterpreter s collapse collapseException -< (r, e, objsOutput, playerStdin)
        -- can't use 'edgeJust', because it has the surprising definition of 'edgeBy ... (Just undefined)' (???)
        -- so a Just output on the first tick would be discarded
        result' <- edgeBy
          (\case Nothing -> (\case Just a -> Just a; _ -> Nothing); _ -> const Nothing)
          Nothing -< result
        returnA -< (mana', result', objsInput, stdout)
        ) <$> replInput

    -- SF that can assume the Spell input is constant. Returns a cancellation
    -- function, and eventually the result. Use the returned canceller in the
    -- same tick to cancel.
    constantInterpreter s0 collapse eFromException = loopPre (s0, False, mempty) $
      proc ((currentMana, e, objsOutput, playerStdin), (s, listeningStdin, stdin1)) -> do
        -- Only listen to stdin if we are blocked, otherwise discard the input
        let stdin2 = if listeningStdin then event mempty id playerStdin <> stdin1 else stdin1
            m = flip runStateT (s, currentMana, stdin2) $ handleSpell objsOutput eFromException (eventToMaybe e)

        ((result, objsInput, stdout, blockedOnStdin), (s', newMana, newStdin)) <- arrM id -< lift m

        let result' = fmap (either collapse id) result

        returnA -< ((newMana, result', objsInput, stdout), (s', blockedOnStdin, newStdin))

-- | Small-step semantics for SpellT
handleSpell
  :: forall e m r s.
     ( Monad m
     , Monoid (ObjsInput e m r)
     -- using type equality here as a type-level 'let'
     , s ~ SpellT e (MaybeT m `Product` ReaderT SomeException m) r
     )
  => ObjsOutput e m r
  -> (SomeException -> e)
  -> Maybe SomeException
  -> StateT
      ( s
      , Double -- current mana
      , Seq Char -- stdin
      ) m
      ( Maybe (Either e r) -- final result
      , ObjsInput e m r -- side-effects
      , Event Char -- stdout
      , Bool -- blocked on stdin (TODO: return larger block reason type?)
      )
handleSpell
  Objects{player = PlayerOutput{playerX, playerY, playerFacingDirection}}
  eFromException
  toThrow
  = do
    (SpellT (FreeT (Pair (MaybeT nxt) (ReaderT canc))), currentMana, playerStdin) <- get
    nxt' <- case toThrow of
      Nothing -> lift nxt
      Just e -> lift $ fmap Just (canc e)
    case nxt' of
      Nothing -> pure (Nothing, mempty, NoEvent, False) -- ^ repeat until we have the value
      Just (Pure r) -> pure (Just (Right r), mempty, NoEvent, False)
      Just (Free f) -> case f of
        Firebolt nxt'' ->
          if currentMana >= fireboltCost
            then do
              let s' = SpellT . join $ lift nxt''
                  fireboltVel = fireboltSpeed *^ playerFacingDirection
                  playerPos = V2 playerX playerY
                  fs = FireboltState { fireboltPos = playerPos, fireboltVel, fireboltRadius = 1, lifetime = 10 }
                  fin = FireboltsInput { killFirebolts = noEvent, spawnFirebolts = Event [fs] }
              _1 .= s'
              _2 %= subtract fireboltCost
              pure (Nothing, mempty{firebolts = fin}, NoEvent, False)
            else do
              let enxt = liftF (Throw $ eFromException (SomeException OutOfSideEffects))
              _1 .= enxt
              pure (Nothing, mempty, NoEvent, False)
        Throw e ->
          -- TODO: implement catch
          pure (Just (Left e), mempty, NoEvent, False)
        Catch {} -> pure (Nothing, mempty, NoEvent, False) -- TODO: implement catch
        PutChar c nxt'c -> do
          let s' = SpellT $ join $ lift nxt'c
          _1 .= s'
          pure (Nothing, mempty, Event c, False)
        GetChar nxt'c -> do
          let s' c = SpellT . join . lift $ fmap ($ c) nxt'c
          case playerStdin of
            c Seq.:<| cs -> do
              _3 .= cs
              _1 .= s' c
              pure (Nothing, mempty, NoEvent, False)
            Seq.Empty -> pure (Nothing, mempty, NoEvent, True) -- block
        Face faceX faceY nxt'f -> do
          let dir = V2 faceX faceY
              pin =
                if nearZero dir
                  then mempty { overrideFacingDirection = Event Nothing}
                  else mempty { overrideFacingDirection = Event (Just (V2 faceX faceY))}
              s' = SpellT . join $ lift nxt'f
          _1 .= s'
          pure (Nothing, Objects{player = pin, firebolts = mempty}, NoEvent, False)

fireboltCost :: Fractional a => a
fireboltCost = 10

fireboltSpeed :: Fractional a => a
fireboltSpeed = 10
