{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Simulation.Objects.SpellInterpreter (spellInterpreterObj, module Simulation.Objects.SpellInterpreter.Types) where

import FRP.BearRiver
import Simulation.Objects
import Control.Exception
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
import Data.Default
import Simulation.Coordinates
import Simulation.Objects.TargetSelector
import Control.Monad.Trans.State.Strict (StateT, State)
import Linear.Metric (normalize)
import Control.Monad.Trans.Except
import Simulation.Component
import Simulation.Util (coroutineToMsf)
import Data.IntSet
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.Coroutine
import qualified Data.IntSet as IntSet
import Data.Some
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Kind
import GHC.Float
import Control.Monad.Coroutine.Nested
import Data.Functor.Sum
import Data.Maybe
import Control.Monad.Trans.MSF (runStateS__)
import Data.Functor.Compose
import Control.Monad.Fix

-- TODO move the orphan instance somewhere else
instance (Functor f, MonadState s m) => MonadState s (Coroutine f m) where
  state f = lift $ state f


fireboltCost :: Fractional a => a
fireboltCost = 10

fireboltSpeed :: Fractional a => a
fireboltSpeed = 10

catchCost :: Fractional a => a
catchCost = 10

data HOut = HOut
  { hStdout :: Seq Char
  , objIns :: WrappedInputs Obj
  }

instance Semigroup HOut where
  (<>) (HOut a b) (HOut c d) = HOut (a <> c) (b <> d)

instance Monoid HOut where
  mempty = HOut mempty mempty

data HIn = HIn
  { newId :: !Int
  , outputs :: ComponentOutputs Obj
  , toThrow :: Maybe SomeException
  , actionResults :: DMap ActionTag (Either SomeException)
  }

data HState = HState
  { _hStdin :: Seq Char
  , _block :: Maybe Blocked
  , _activeActions :: !IntSet
  , _currentInput :: !HIn
  }

newtype HContext = HContext { convertError :: SomeException -> InterpreterError }

makeLenses ''HState

type HandlerM = ReaderT HContext (ExceptT InterpreterError (Coroutine (Request HOut HIn) (StateT HState ObjectM)))

handleSpell
  :: Monad ObjectM
  => SpellT InterpreterError (ObjectM `Compose` MaybeT ObjectM `Compose` Either InterpreterError) r
  -> HandlerM r
handleSpell (SpellT (FreeT nxt)) = do
  nxt' <- waitEvaluate nxt
  case nxt' of
    Pure r -> pure r
    Free f -> do
      handleSpell' f \onxt -> do

        onxt' <- waitEvaluate onxt

        handleSpell (SpellT onxt')

handleSpell'
  :: Monad ObjectM
  => SpellF InterpreterError (ObjectM `Compose` MaybeT ObjectM `Compose` Either InterpreterError) r
  -> ((ObjectM `Compose` MaybeT ObjectM `Compose` Either InterpreterError) r -> HandlerM a)
  -> HandlerM a
handleSpell' f cont = case f of
  Throw e -> lift $ throwE e
  Firebolt faceX faceY nxt -> do
    HIn{newId} <- use currentInput
    let atag = intToActionTag newId :: ActionTag ()
    () <- sendAction atag (fireboltAction (V2 faceX faceY) atag)
    cont nxt
  PutChar c nxt -> do
    _ <- request' mempty{hStdout = pure c}
    cont nxt
  GetChar nxt'c ->
    let nxt c = fmap ($ c) nxt'c
     in fix \k -> use hStdin >>= \case 
          (c Seq.:<| cs) -> do
            hStdin .= cs
            _ <- request' mempty
            cont (nxt c)
          Seq.Empty -> do
            blockWith mempty BlockedOnStdin
            k
  InputTarget nxt'v -> do
    HIn{newId} <- use currentInput
    let nxt (V2 a b) = fmap (($ b) . ($ a)) nxt'v
        atag = intToActionTag newId :: ActionTag V
    v <- sendAction atag (inputTargetAction atag)
    cont (nxt v)
  Catch expr h nxt'r -> do
    HIn{newId} <- use currentInput
    let nxt r = fmap ($ r) nxt'r
        atag = intToActionTag newId :: ActionTag ()
    -- Cost of catch is paid ahead of time
    () <- sendAction atag (billFor catchCost atag)
    r <- ReaderT \ctx -> catchE
      do
        flip runReaderT ctx do
          expr' <- waitEvaluate expr
          handleSpell expr'
      \e -> flip runReaderT ctx do
        h' <- waitEvaluate h
        handleSpell (h' e)
    cont (nxt r)

blockWith :: Monad ObjectM => HOut -> Blocked -> HandlerM ()
blockWith hout b = do
  block .= Just b
  _ <- request' hout
  fix \k -> do
    b' <- use block
    case b' of
      Just _ -> do
        _ <- request' mempty
        k
      Nothing -> pure ()

waitEvaluate :: Monad ObjectM => (ObjectM `Compose` MaybeT ObjectM `Compose` Either InterpreterError) r -> HandlerM r
waitEvaluate (Compose o0) = do
  Compose (MaybeT o) <- lift . lift . lift $ lift o0
  fix \k -> do
    (lift . lift . lift $ lift o) >>= \case
      Nothing -> request' mempty >> k -- block until evaluated
      Just r -> do
        r' <- lift $ except r
        pure r'

request' :: Monad ObjectM => HOut -> HandlerM HIn
request' hout = do
  HContext{convertError} <- ask
  hin@HIn{toThrow} <- lift $ lift $ request hout
  currentInput .= hin
  case toThrow of
    Nothing -> pure ()
    Just e -> lift $ throwE (convertError e)
  pure hin

billFor :: Double -> ActionTag () -> Action
billFor cost atag = makeAtomicAction atag \_ -> do
  mana <- get
  if mana >= cost
    then do
      modify' (subtract cost)
      pure (\case _ -> mempty, Right ())
    else pure (mempty, Left (SomeException OutOfSideEffects))

fireboltAction :: V2 Double -> ActionTag () -> Action
fireboltAction (V2 faceX faceY) atag = makeAtomicAction atag \oo -> do
  let PlayerOutput{..} = oo Player
      fireboltVel = if nearZero (V2 faceX faceY)
        then fireboltSpeed * playerFacingDirection
        else fireboltSpeed * normalize (V2 faceX faceY)
      fs = FireboltState { fireboltPos = V2 playerX playerY, fireboltVel, fireboltRadius = 1, lifetime = 10 }
      fin = FireboltsInput { killFirebolts = noEvent, spawnFirebolts = Event [fs] }
  mana <- get
  if mana >= fireboltCost
    then do
      modify' (subtract fireboltCost)
      pure (\case Firebolts -> fin; _ -> mempty, Right ())
    else pure (mempty, Left (SomeException OutOfSideEffects))

-- | INVARIANT: actionTag must be the one contained in the Action
sendAction
  :: Monad ObjectM
  => ActionTag c
  -> Action
  -> HandlerM c
sendAction atag a =
  let hout = mempty{ objIns = WrappedInputs \case
        Player -> mempty{actions = Event[a]}
        _ -> mempty}
   in do
    HContext{convertError} <- ask
    activeActions %= IntSet.insert (actionId atag)
    blockWith hout $ BlockedOnAction (Some atag)
    activeActions %= IntSet.delete (actionId atag)
    HIn{actionResults} <- use currentInput
    case DMap.lookup atag actionResults of
      Nothing ->
        lift . throwE . convertError . SomeException $ AssertionFailed "internal bug: falsely unblocked on action"
      Just (Left e) ->
        lift $ throwE (convertError e)
      Just (Right c) ->
        pure c

-- | If an exception is received at t=0, the action will not be performed.
makeAtomicAction
  :: ActionTag c
  -> (ComponentOutputs Obj -> State Double (ComponentInputs Obj, Either SomeException c)) -> Action
makeAtomicAction atag a = Action atag do
  (oi, c) <- mkTask $ arrM \(WrappedOutputs oo) -> do
      (oi, eex) <- lift $ a oo
      pure (WrappedInputs oi, Event eex)
  shriek oi
  pure c

-- | Run the given Task to completion, interrupting immediately without handling on an exception.
makeLongAction
  :: ActionTag (c :: Type)
  -> Task (WrappedOutputs Obj) (WrappedInputs Obj) (State Double) (Either SomeException c)
  -> Action
makeLongAction atag t = Action atag . mkTask' $ runTask t

inputTargetCost :: Num a => a
inputTargetCost = 25

inputTargetAction :: ActionTag V -> Action
-- activate input target, then wait for a response.
inputTargetAction atag =
  makeLongAction atag $ runExceptT do
    mana <- lift . lift $ get
    if mana >= inputTargetCost
      then do
        lift . lift $ modify' (subtract inputTargetCost)
      else do
        throwE $ SomeException OutOfSideEffects 
    lift $ mkTask' $ proc oo -> do
      let TargetSelectorOutput{targetX, targetY, select} = unwrapOutputs oo TargetSelector
      -- while the task is running, target selector should be active
      let oi = WrappedInputs \case TargetSelector -> mempty{active = True}; _ -> mempty
      
      -- the first select event, if it exists, is definitely not addressed to us
      selectEvent <- initially NoEvent -< select

      returnA -< if isEvent selectEvent then Right (V2 targetX targetY) else Left oi

spellInterpreterObj :: Monad ObjectM => Component Obj ObjectM SpellInterpreterInput SpellInterpreterOutput
spellInterpreterObj = continuousInterpreter
  where
    nothingInterpreter = constM (pure (def, mempty))
    continuousInterpreter = shrinkComponent $ proc (o@SpellInterpreterInput{replInput, exception}, objsOutput) -> do
      t <- toComponent time -< ()
      let newId = fromIntegral $ castDoubleToWord64 t

      isf <- toComponent newConstantInterpreter -< replInput
      -- Prioritise the user interrupt, since it cannot be caught
      let e' = (SomeException UserInterrupt <$ isf) <|> exception
      toComponent (drSwitch nothingInterpreter) -< ((o{exception = e'}, objsOutput, newId), isf)
    newConstantInterpreter = proc replInput -> do
      returnA -< maybe nothingInterpreter
        (\(s0, collapse, eFromException) -> morphS lift $ constantInterpreter s0 collapse eFromException)
        <$> replInput

constantInterpreter
  :: Monad ObjectM
  => SpellT InterpreterError (ObjectM `Compose` MaybeT ObjectM `Compose` Either InterpreterError) InterpreterReturn
  -> (InterpreterError -> InterpreterReturn)
  -> (SomeException -> InterpreterError)
  -> MSF ObjectM (SpellInterpreterInput, WrappedOutputs Obj, Int) (SpellInterpreterOutput, WrappedInputs Obj)
constantInterpreter s0 collapse convertError =
  flip runStateS__ (error "constantInterpreter: uninitialised") $
    coroutineToMsf \(si, oo, newId) -> do
      let hin0 = inputToHIn si oo newId
          ctx = HContext {convertError}
          state0 = HState { _activeActions = IntSet.empty, _hStdin = event Seq.empty id (stdin si), _block = Nothing, _currentInput = hin0 }
          co = mapSuspension (\(Request hout k) -> InR (Request hout k)) . runExceptT $ runReaderT (handleSpell s0) ctx

      lift $ put state0

      result <- co & pogoStickNested \(Request hout k) -> do
        s <- get
        (si', oo', newId') <- suspend (InL (Request (hOutToOutput s hout) pure))

        case stdin si' of
          Event cs -> hStdin %= (<> cs)
          NoEvent -> pure ()

        let hin'@HIn{actionResults, toThrow} = inputToHIn si' oo' newId'

        stdinCs <- use hStdin
        b <- use block
        case b of
          -- not blocked
          Nothing -> k hin'

          -- exception always unblocks
          Just _ | isJust toThrow -> do
            block .= Nothing
            k hin'

          Just BlockedOnStdin | not (Seq.null stdinCs) -> do
            block .= Nothing
            k hin'
          Just (BlockedOnAction (Some atag)) | isJust (DMap.lookup atag actionResults) -> do
            block .= Nothing
            k hin'

          -- blocked and no reason to unblock yet
          Just _ -> do
            -- Recurse into the same handler arbitrarily long. It returns when we are unblocked.
            hin'' <- suspend (InR (Request mempty pure))
            k hin''

      _ <- request $ (def{ replResponse = Event (either collapse id result)}, mempty)

      -- Do nothing else interesting forever
      forever do
        _ <- request (def, mempty)
        pure ()

inputToHIn :: SpellInterpreterInput -> WrappedOutputs Obj -> Int -> HIn
inputToHIn si oo newId = HIn { newId, outputs = unwrapOutputs oo, toThrow = eventToMaybe (exception si), actionResults = event DMap.empty id $ completeActions si}

hOutToOutput :: HState -> HOut -> (SpellInterpreterOutput, WrappedInputs Obj)
hOutToOutput HState{..} HOut{..} =
  (SpellInterpreterOutput
      { stdout = if Seq.null hStdout then NoEvent else Event hStdout
      , replResponse = NoEvent
      , blocked = _block
      , runningActions = _activeActions
      }, objIns)
