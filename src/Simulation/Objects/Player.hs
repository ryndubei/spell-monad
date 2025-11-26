{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Simulation.Objects.Player (ObjInput(..), ObjOutput(..), playerObj) where

import Simulation.Objects
import Simulation.Objects.Firebolts
import FRP.BearRiver
import Simulation.Input
import Spell
import Control.Lens
import Control.Monad.Fix
import Simulation.Coordinates
import App.Thread.SF
import Data.Functor.Product
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Exception
import Control.Applicative
import Data.Sequence (Seq)
import Control.Monad.Trans.Class
import Control.Monad
import Control.Monad.Trans.Free
import Data.Maybe
import Spell.Exception (SpellException(..))
import Linear.Epsilon
import Control.Monad.Trans.State.Strict
import qualified Data.Sequence as Seq

data instance ObjInput (Player e m r) = PlayerInput
  { simInput :: SimInput -- ^ Continuous user input
  , replInput :: Event (Maybe (SpellT e (MaybeT m `Product` ReaderT SomeException m) r, e -> r, SomeException -> e))
  , overrideFacingDirection :: Event (Maybe V)
  , playerStdin :: Event (Seq Char)
  }
data instance ObjOutput (Player e m r) = PlayerOutput
  { playerX :: !Double
  , playerY :: !Double
  , playerMana :: !Double
  , playerMaxMana :: !Double
  , playerStdout :: Seq Char
  , replResponse :: Event r
  , playerFacingDirection :: V
  }

instance Semigroup (ObjInput (Player e m r)) where
  (<>) p1 p2 = PlayerInput
    { simInput = simInput p1 <> simInput p2
    , replInput = replInput p2 <|> replInput p1
    , overrideFacingDirection = overrideFacingDirection p1 <|> overrideFacingDirection p2
    , playerStdin =  mergeBy (<>) (playerStdin p1) (playerStdin p2)
    }

instance Monoid (ObjInput (Player e m r)) where
  mempty = PlayerInput mempty empty empty empty

gravityAcceleration :: Fractional a => a
gravityAcceleration = 9.8

playerBaseVelocity :: Fractional a => a
playerBaseVelocity = 10

playerJumpVelocity :: Fractional a => a
playerJumpVelocity = 10

playerManaRegenRate :: Fractional a => a
playerManaRegenRate = 5

spellInterpreter
  :: forall e m r.
  (Monad m, Monoid (ObjsInput e m r))
  => SF m
    (Double, Event SomeException, ObjInput (Player e m r), ObjsOutput e m r)
    (ObjsInput e m r, Double, Event r, Event Char)
spellInterpreter = proc (r, e, o, objsOutput) -> do
  (mana', result, objsInput, stdout) <- continuousInterpreter -< ((r, e, objsOutput), o)
  returnA -< (objsInput, mana', result, stdout)
  where
    nothingInterpreter = arr (\(m, _, _, _) -> (m, NoEvent, mempty, NoEvent))

    -- Switch interpreter sessions in the background.
    continuousInterpreter = proc ((cost, e, objsOutput), o) -> do
      isf <- newConstantInterpreter -< replInput o
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
              let enxt = liftF (Throw . pure $ eFromException (SomeException OutOfSideEffects))
              _1 .= enxt
              pure (Nothing, mempty, NoEvent, False)
        -- TODO: throw doesn't really need to be wrapped in 'm':
        --
        -- it would make sense to have an isomorphism to ExceptT e (SpellF ... r)
        -- which in turn should give an isomorphism ExceptT r (SpellF ... e)
        -- and the 'r' in FreeT (Pure r) is not wrapped in 'm'.
        --
        -- (explained alternatively: evalSpell already wraps the exceptions in 'u')
        Throw (Pair (MaybeT nxt'e) canc'e) -> do
          nxt''e <- lift nxt'e
          case nxt''e of
            Just e ->
              -- TODO: implement catch
              pure (Just (Left e), mempty, NoEvent, False)
            Nothing -> do
              -- update outer cancellation function, keep everything else the same
              let s' = SpellT (FreeT (Pair (MaybeT nxt) (fmap (Free . Throw . pure) canc'e)))
              _1 .= s'
              -- keep trying again until the exception to be thrown is evaluated
              pure (Nothing, mempty, NoEvent, False)
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
fireboltCost = 50

fireboltSpeed :: Fractional a => a
fireboltSpeed = 10

playerObj :: forall e m r. (Monad m, Monoid (ObjsInput e m r)) => Object e m r (Player e m r)
playerObj = loopPre playerMaxMana $ proc ((playerIn, objsOutput), lastPlayerMana) -> do

  pos' <- (fix $ \k (pos1, vy) ->
    switch (generaliseSF $ fallingMovement pos1 vy) (\pos2 -> switch (generaliseSF $ groundedMovement pos2) k))
    (zeroVector, 0) -< playerIn

  -- Default facing direction is the direction of movement.
  let (vx, _) = simInput playerIn ^. moveVector
      movingLeft = vx < 0
      defaultFacingDirection = if movingLeft then V2 (-1) 0 else V2 1 0
  facingDirectionOverride <- arr (>>= \x -> guard (not (nearZero x)) >> pure (normalize x)) <<< hold Nothing -< overrideFacingDirection playerIn
  playerFacingDirection <- arr (uncurry fromMaybe) -< (defaultFacingDirection, facingDirectionOverride)

  -- Player mana is regenerated once a second at the regen rate (instead of continuously)
  -- Can't get more sophisticated than this unless I add a MonadFix constraint,
  -- or rewrite spellInterpreter to rely less on current mana state.
  manaRegenEvent <- repeatedly 1 () -< ()
  let newMana1 = event lastPlayerMana id $ min playerMaxMana (lastPlayerMana + playerManaRegenRate) <$ manaRegenEvent

  (objsInput, newMana2, replResponse, stdout) <- spellInterpreter -< (newMana1, NoEvent, playerIn, objsOutput)

  let playerOutput = PlayerOutput
        { playerX = pos' ^. _x
        , playerY = pos' ^. _y
        , playerMana = newMana2
        , playerMaxMana
        , replResponse
        , playerStdout = event mempty Seq.singleton stdout
        , playerFacingDirection
        }

  returnA -< ((playerOutput, objsInput), newMana2)
  where
    playerMaxMana = 100

    groundedMovement :: V -> SF Identity (ObjInput (Player e m r)) (V, Event (V, Double))
    groundedMovement (V2 x0 _) = proc (PlayerInput{simInput = simInput@SimInput{simJump}}) -> do
        let (vx, _) = playerBaseVelocity *^ (simInput ^. moveVector)
        dx <- integral -< vx
        let pos' = V2 (x0 + dx) 0
        -- Horizontal velocity is fully determined by the user input, so we only
        -- return the vertical velocity.
        returnA -< (pos', (pos', playerJumpVelocity) <$ simJump)

    -- Accelerates downwards until both velocity and position would be negative,
    -- then switches back to grounded movement.
    fallingMovement :: V -> Double -> SF Identity (ObjInput (Player e m r)) (V, Event V)
    fallingMovement (V2 x0 y0) vy0 = proc (PlayerInput{simInput}) -> do
      let (vx, _) = playerBaseVelocity *^ (simInput ^. moveVector)
      dx <- integral -< vx
      dvy <- integral -< gravityAcceleration
      let vy = vy0 - dvy
      dy <- integral -< vy
      rec
        pos <- iPre (V2 x0 y0) -< pos'
        let pos' = V2 (x0 + dx) (y0 + dy)
            grounded = pos' ^. _y <= 0 && (vy <= 0)
      returnA -< (pos', gate (Event pos) grounded)
