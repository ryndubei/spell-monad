{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Simulation.Objects.SpellInterpreter (spellInterpreterObj, module Simulation.Objects.SpellInterpreter.Types) where

import FRP.BearRiver
import Simulation.Objects
import Control.Exception
import Data.Functor.Product
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Spell (SpellT(..), SpellF (..))
import Control.Monad.State.Class
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
import Simulation.Coordinates
import Simulation.Objects.TargetSelector
import Data.Either
import Data.Maybe
import Control.Monad.Trans.State.Strict (StateT, State)

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
        proc (u@SpellInterpreterInput{exception, stdin, completeActions, completeInputTargets}, oso, t) -> do
          -- Invariant: handleSpell is only called once unblocked for a valid reason
          (_, _, blk1, _) <- constM get -< ()

          -- Handle unblocking
          let handleSpellInputs = do
                case blk1 of
                  -- not blocked
                  Nothing -> pure $ Just (u, oso, empty)

                  Just BlockedOnStdin ->
                    if isEvent exception || event False id (fmap (not . Seq.null) stdin)
                      then do
                        _3 .= Nothing
                        pure $ Just (u, oso, empty)
                      else pure Nothing
                  Just (BlockedOnAction atag) ->
                    if | isEvent exception -> do
                          _3 .= Nothing
                          -- running actions
                          _4 %= Set.delete atag
                          pure $ Just (u, oso, empty)
                       | Just mex <- Map.lookup atag =<< eventToMaybe completeActions -> do
                          _3 .= Nothing
                          _4 %= Set.delete atag
                          pure $ Just (u{exception = maybeToEvent mex}, oso, empty)
                       | otherwise -> pure Nothing
                  Just (BlockedOnInputTarget atag) ->
                    if | isEvent exception -> do
                          _3 .= Nothing
                          _4 %= Set.delete atag
                          pure $ Just (u, oso, empty)
                       | Just (Left e) <- Map.lookup atag =<< eventToMaybe completeInputTargets -> do
                          _3 .= Nothing
                          _4 %= Set.delete atag
                          pure $ Just (u{exception = Event e}, oso, empty)
                       | Just (Right v) <- Map.lookup atag =<< eventToMaybe completeInputTargets -> do
                          _3 .= Nothing
                          _4 %= Set.delete atag
                          -- TODO: ugly. generalise Actions so this is easier.
                          pure $ Just (u, oso, Event v)
                       | Just mex <- Map.lookup atag =<< eventToMaybe completeActions -> do
                          _3 .= Nothing
                          _4 %= Set.delete atag
                          let e = fromMaybe
                                (SomeException $ AssertionFailed
                                  "internal bug: action completed without exception, but did not unblock BlockedOnInputTarget")
                                mex
                          pure $ Just (u{exception = Event e}, oso, empty)
                       | otherwise -> pure Nothing

          x <- arrM id -< handleSpellInputs
          case x of
            -- still blocked
            Nothing -> do
              (_, _, _, runningActions) <- constM get -< ()
              returnA -< (def{runningActions}, mempty)
            Just x' -> do
              -- TODO: block/unblock logic in handleSpell?
              (oo, oi, blk) <- arrM id -< handleSpell x' collapse t
              arrM (_3 .=) -< blk
              (_, _, _, ras) <- constM get -< ()
              returnA -< (oo{runningActions = runningActions oo <> ras}, oi)

-- | Small-step semantics for SpellT
handleSpell
  :: forall e m r s t blk.
     ( Monad m
     , forall x y z. Monoid (ObjsInput x y z)
     -- using type equality here as a type-level 'let'
     , s ~ SpellT e (MaybeT m `Product` ReaderT SomeException m) r
     , MonadTrans t
     )
  => (ObjInput (SpellInterpreter e m r), ObjsOutput e m r, Event V)
  -> (e -> r)
  -> Time
  -> StateT
       ( s
       , Seq Char -- stdin
       , blk
       , Set ActionTag
       ) (t m) (ObjOutput (SpellInterpreter e m r), ObjsInput e m r, Maybe Blocked)
handleSpell
  ( SpellInterpreterInput{exception = toThrow, stdin = newStdin}
  , Objects{player = PlayerOutput{playerX, playerY, playerFacingDirection}}
  , inputTargetV
  )
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
        InputTarget nxt'v -> do
          let s' a b = SpellT . join . lift $ fmap (`uncurry` (a,b)) nxt'v
              atag = ActionTag t
          case inputTargetV of
            Event (V2 a b) -> do
              _1 .= s' a b
              pure (def, mempty, Nothing) -- continue
            NoEvent -> do
              _4 %= Set.insert atag
              pure
                ( def
                , mempty{player = mempty{actions = Event [inputTargetAction atag]}}
                , Just (BlockedOnInputTarget atag)
                )

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

-- | Run the given Task to completion, interrupting immediately without handling on an exception.
--
-- Monitors runningActions in SpellInterpreter, halts silently when invalid.
-- Tells SpellInterpreter about the exit status of the task if terminated for
-- another reason.
--
-- Calls runTask internally, meaning the final auxiliary output on the task will be ignored.
-- For an action to have any effects, the underlying task therefore requires to
-- run for at least 2 ticks.
makeLongAction
-- side note on the topic of the final output being ignored: I really dislike how
-- Yampa tends to silently swallow outputs by default instead of the opposite
  :: (forall e m r. Monoid (ObjsInput e m r))
  => ActionTag
  -> (forall e m r. Task (State Double) (ObjsOutput e m r) (ObjsInput e m r) (Maybe SomeException))
  -> Action
makeLongAction atag t = Action $ mkTask $ proc (e, oo) -> do
  let SpellInterpreterOutput{runningActions} = spellInterpreter oo
      -- important: we should be able to verify by observation that the action
      -- is running already at t=0
      actionRunning = atag `Set.member` runningActions
  if not actionRunning 
    then do
      returnA -< (mempty, Event ())
    else case e of
      Event e' -> do
        returnA -< (mempty{spellInterpreter = mempty{completeActions = Event (Map.singleton atag (Just e'))}}, Event ())
      NoEvent -> do
        eoi <- runTask t -< oo
        let oi1 = fromLeft mempty eoi
            eoi' = fmap (Event . Map.singleton atag) eoi
            oi = oi1{spellInterpreter = mempty{completeActions = fromRight empty eoi'}}
        completeEvent <- iPre NoEvent -< guard (isRight eoi)
        returnA -< (oi, completeEvent)

inputTargetCost :: Num a => a
inputTargetCost = 25

inputTargetAction :: (forall e m r. Monoid (ObjsInput e m r)) => ActionTag -> Action
-- activate input target, then wait for a response.
inputTargetAction atag = makeLongAction atag $ mkTask $ switch
  -- pay mana cost once
  (constM do
    mana <- get
    if mana >= inputTargetCost
      then do
        modify' (subtract inputTargetCost)
        pure ((mempty, NoEvent), Event ())
      else pure ((mempty, Event (Just (SomeException OutOfSideEffects))), NoEvent)
  )
  \() -> proc Objects{targetSelector = TargetSelectorOutput{targetX, targetY, select}} -> do
    -- while the task is running, target selector should be active
    let oi1 = mempty{targetSelector = mempty{active = True}}

    -- the first select event is definitely not addressed to us
    selectEvent <- initially NoEvent -< select

    -- output on selectEvent
    let oi2 = mempty{spellInterpreter = mempty{completeInputTargets = Event (Map.singleton atag (Right (V2 targetX targetY)))}}

    -- Delay selectEvent so that we can perform the necessary finishing effect
    selectEvent' <- iPre NoEvent -< selectEvent

    returnA -< (if isEvent selectEvent then oi2 else oi1, tag selectEvent' Nothing)
