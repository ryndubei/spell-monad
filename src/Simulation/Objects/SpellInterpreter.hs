{-# OPTIONS_GHC -Wno-orphans #-}
module Simulation.Objects.SpellInterpreter (spellInterpreterObj, module Simulation.Objects.SpellInterpreter.Types) where

import FRP.BearRiver
import Simulation.Objects
import Control.Exception
import Data.Functor.Product
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Spell (SpellT(..), SpellF (..))
import Control.Monad.Trans.State.Strict
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
import Control.Monad.Trans.MSF.State (runStateS__)

fireboltCost :: Fractional a => a
fireboltCost = 10

fireboltSpeed :: Fractional a => a
fireboltSpeed = 10

spellInterpreterObj :: (Monad m, Monoid (ObjsInput e m r)) => Object e m r (SpellInterpreter e m r)
spellInterpreterObj = continuousInterpreter
  where
    nothingInterpreter = undefined
    continuousInterpreter = proc (o@SpellInterpreterInput{replInput, exception}, objsOutput) -> do
      isf <- newConstantInterpreter -< replInput
      -- Ignore external exception at time 0: it's meant for the code that is already
      -- running, not the code that we are switching to.
      let isf' = fmap (first (\s -> (s{exception = NoEvent})) >=-) isf
          -- Prioritise the user interrupt, since it cannot be caught
          e' = (SomeException UserInterrupt <$ isf) <|> exception
      rSwitch nothingInterpreter -< ((o{exception = e'}, objsOutput), isf')
    newConstantInterpreter = proc replInput -> do
      returnA -< maybe nothingInterpreter
        (\(s0, collapse, eFromException) -> constantInterpreter s0 collapse eFromException)
        <$> replInput
    constantInterpreter s0 collapse eFromException = flip runStateS__ (s0, False, mempty, 0) $
      proc u@(SpellInterpreterInput{feedMana}, _) -> do
        case feedMana of
          -- Increment available mana by feedMana
          Event d -> arrM (\d -> _4 %= (+ d)) -< d
          NoEvent -> returnA -< ()
        arrM id -< handleSpell u collapse eFromException
    
-- | Small-step semantics for SpellT
handleSpell
  :: forall e m r s t.
     ( Monad m
     , Monoid (ObjsInput e m r)
     -- using type equality here as a type-level 'let'
     , s ~ SpellT e (MaybeT m `Product` ReaderT SomeException m) r
     , MonadTrans t
     )
  => (ObjInput (SpellInterpreter e m r), ObjsOutput e m r)
  -> (e -> r)
  -> (SomeException -> e)
  -> StateT
       ( s
       , Bool -- blocked on stdin (TODO: return larger block reason type?)
       , Seq Char -- stdin
       , Double -- available mana
       ) (t m) (ObjOutput (SpellInterpreter e m r), ObjsInput e m r)
handleSpell
  (SpellInterpreterInput{exception, stdin = newStdin}, Objects{player = PlayerOutput{playerX, playerY, playerFacingDirection}})
  collapse
  eFromException
  = do
    undefined

handleSpell'
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
handleSpell'
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
