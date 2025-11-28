{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ImpredicativeTypes #-}
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
import Data.Default
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

fireboltCost :: Fractional a => a
fireboltCost = 10

fireboltSpeed :: Fractional a => a
fireboltSpeed = 10

spellInterpreterObj :: (Monad m, forall x y z. Monoid (ObjsInput x y z)) => Object e m r (SpellInterpreter e m r)
spellInterpreterObj = continuousInterpreter
  where
    nothingInterpreter = constM (pure (def, mempty))
    continuousInterpreter = proc (o@SpellInterpreterInput{replInput, exception}, objsOutput) -> do

      -- continuousInterpreter must not be switched because of this.
      -- If it ends up being switched, should move the time measurement further
      -- outward where it won't be switched.
      t <- time -< ()

      isf <- newConstantInterpreter -< replInput
      -- Ignore external exception at time 0: it's meant for the code that is already
      -- running, not the code that we are switching to.
      let isf' = fmap (_1 %~ (\s -> (s{exception = NoEvent})) >=-) isf
          -- Prioritise the user interrupt, since it cannot be caught
          e' = (SomeException UserInterrupt <$ isf) <|> exception
      rSwitch nothingInterpreter -< ((o{exception = e'}, objsOutput, t), isf')
    newConstantInterpreter = proc replInput -> do
      returnA -< maybe nothingInterpreter
        (\(s0, collapse, _eFromException) -> constantInterpreter s0 collapse)
        <$> replInput
    constantInterpreter s0 collapse = flip runStateS__ (s0, mempty, Nothing, mempty) $
        proc (u@SpellInterpreterInput{exception, stdin, completeActions}, oso, t) -> do
          -- Invariant: handleSpell is only called once unblocked for a valid reason
          (_, _, blk1, _) <- constM get -< ()

          -- Handle unblocking
          let handleSpellInputs = do
                case blk1 of
                  -- not blocked
                  Nothing -> pure $ Just (u, oso)

                  Just BlockedOnStdin ->
                    if isEvent exception || event False id (fmap (not . Seq.null) stdin)
                      then do
                        _3 .= Nothing
                        pure $ Just (u, oso)
                      else pure Nothing
                  Just (BlockedOnAction atag) ->
                    if | isEvent exception -> do
                          _3 .= Nothing
                          -- running actions
                          _4 %= Set.delete atag
                          pure $ Just (u, oso)
                       | Just mex <- Map.lookup atag =<< eventToMaybe completeActions -> do
                          _3 .= Nothing
                          _4 %= Set.delete atag
                          pure $ Just (u{exception = maybeToEvent mex}, oso)
                       | otherwise -> pure Nothing

          x <- arrM id -< handleSpellInputs
          case x of
            -- still blocked
            Nothing -> returnA -< (def, mempty)
            Just x' -> do
              -- TODO: block/unblock logic in handleSpell?
              (oo, oi, blk) <- arrM id -< handleSpell x' collapse t
              arrM (_3 .=) -< blk
              (_, _, _, runningActions) <- constM get -< ()
              returnA -< (oo{runningActions}, oi)

-- | Small-step semantics for SpellT
handleSpell
  :: forall e m r s t blk.
     ( Monad m
     , forall x y z. Monoid (ObjsInput x y z)
     -- using type equality here as a type-level 'let'
     , s ~ SpellT e (MaybeT m `Product` ReaderT SomeException m) r
     , MonadTrans t
     )
  => (ObjInput (SpellInterpreter e m r), ObjsOutput e m r)
  -> (e -> r)
  -> Time
  -> StateT
       ( s
       , Seq Char -- stdin
       , blk
       , Set ActionTag
       ) (t m) (ObjOutput (SpellInterpreter e m r), ObjsInput e m r, Maybe Blocked)
handleSpell
  (SpellInterpreterInput{exception = toThrow, stdin = newStdin}, Objects{player = PlayerOutput{playerX, playerY, playerFacingDirection}})
  collapse
  t
  = do
    _2 %= (<> event mempty id newStdin)
    (SpellT (FreeT (Pair (MaybeT nxt) (ReaderT canc))), accumStdin, _, _) <- get

    nxt' <- case toThrow of
      NoEvent -> lift $ lift nxt
      Event e -> lift . lift $ fmap Just (canc e)
    case nxt' of
      Nothing -> pure (def, mempty, Nothing) -- ^ repeat until we have the value
      Just (Pure r) ->
        pure
          ( def{replResponse = Event r} -- we only send the value as an output
          , mempty -- nothing to perform
          , Nothing -- not blocking
          )
      Just (Free f) -> case f of
        Throw e ->
          -- TODO: implement catch
          pure (def{ replResponse = Event (collapse e)}, mempty, Nothing)
        Firebolt nxt'' -> do
          let s' = SpellT . join $ lift nxt''
              fireboltVel = fireboltSpeed *^ playerFacingDirection
              playerPos = V2 playerX playerY
              fs = FireboltState { fireboltPos = playerPos, fireboltVel, fireboltRadius = 1, lifetime = 10 }
              fin = FireboltsInput { killFirebolts = noEvent, spawnFirebolts = Event [fs] }
              atag = ActionTag t
              a :: Action
              a = makeAtomicAction atag $ \_ -> do
                mana <- get
                if mana >= fireboltCost
                  then do
                    modify' (subtract fireboltCost)
                    pure (mempty{firebolts = fin}, Nothing)
                  else pure (mempty, Just (SomeException OutOfSideEffects))
          _1 .= s'
          _4 %= Set.insert atag
          pure
            ( def
            , mempty{player = mempty{actions = Event [a]}}
            , Just (BlockedOnAction atag)
            )
        Catch {} -> pure (def, mempty, Nothing) -- TODO: implement catch
        PutChar c nxt'c -> do
          let s' = SpellT $ join $ lift nxt'c
          _1 .= s'
          pure (def{stdout = Event (Seq.singleton c)}, mempty, Nothing)
        GetChar nxt'c -> do
          let s' c = SpellT . join . lift $ fmap ($ c) nxt'c
          case accumStdin of
            c Seq.:<| cs -> do
              _2 .= cs
              _1 .= s' c
              pure (def, mempty, Nothing) -- continue
            Seq.Empty -> pure (def, mempty, Just BlockedOnStdin) -- block
        Face faceX faceY nxt'f -> do
          let dir = V2 faceX faceY
              pin =
                if nearZero dir
                  then mempty { overrideFacingDirection = Event Nothing}
                  else mempty { overrideFacingDirection = Event (Just (V2 faceX faceY))}
              s' = SpellT . join $ lift nxt'f
          _1 .= s'
          pure (def, mempty{player = pin}, Nothing)
        InputTarget _ -> pure (def, mempty, Nothing) -- TODO

-- | If an exception is received at t=0, the action will not be performed.
makeAtomicAction
  :: (forall e m r. Monoid (ObjsInput e m r))
  => ActionTag
  -> (forall e m r. ObjsOutput e m r -> State Double (ObjsInput e m r, Maybe SomeException)) -> Action
makeAtomicAction atag a = Action $ mkTask $ proc (e, oo) -> do
  -- mkTask uses 'switch' instead of 'dSwitch' internally, so we have to delay the finishing event by 1 tick
  -- so the ObjsInput output is sent at t=0
  tick <- NoEvent --> constant (Event ()) -< ()
  case (e, tick) of
    (NoEvent, NoEvent) -> do
      (oi, e') <- arrM (lift . a) -< oo
      returnA -< (oi{spellInterpreter = spellInterpreter oi <> mempty{ completeActions = Event (Map.singleton atag e') }}, NoEvent)
    (Event e', NoEvent) -> do
      returnA -< (mempty{spellInterpreter = mempty{ completeActions = Event (Map.singleton atag (Just e')) }}, NoEvent)
    (_, Event ()) -> do
      returnA -< (mempty, Event ())
