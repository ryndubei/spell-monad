{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf #-}

module Game (runSystem, game, initWorld, World) where

import Apecs
import Input
import Control.Monad.IO.Class
import Chronos
import Control.Monad.Fix
import Linear
import Simulation.Input
import FRP.BearRiver
import Data.Monoid
import Control.Lens
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.Coroutine
import Data.Void
import qualified Control.Monad.Trans.State.Lazy as State.Lazy
import Control.Monad.Trans.Reader (mapReaderT)
import Control.Monad.Trans.Except
import Control.Monad.Trans.MSF (reactimateB, runReaderS)
import Control.Monad

data Player = Player
instance Component Player where
  type Storage Player = Map Player

newtype Position = Position (V2 Double)
instance Component Position where
  type Storage Position = Map Position

-- | Velocity caused by gravity.
--
-- Not possessing this component causes the entity to be unaffected by gravity.
data GravVelocity = Grounded | GravVelocity (V2 Double)
instance Component GravVelocity where
  type Storage GravVelocity = Map GravVelocity

data Model = WizardModel | FireboltModel
instance Component Model where
  type Storage Model = Map Model

data Mana = Mana { currentMana :: Double, maximumMana :: Double }
instance Component Mana where
  type Storage Mana = Map Mana

newtype Gravity = Gravity (Sum (V2 Double)) -- The Sum wrapper specifies the monoid, as Global components default to mempty.
  deriving (Semigroup, Monoid)
instance Component Gravity where
  type Storage Gravity = Global Gravity

-- | The entity possessing this unique component is the one to which user input
-- gets routed to.
newtype Input = Input SimInput
instance Component Input where
  type Storage Input = Unique Input

newtype ElapsedTime = ElapsedTime Timespan deriving (Semigroup, Monoid)
instance Component ElapsedTime where
  type Storage ElapsedTime = Global ElapsedTime

makeWorld "World"
  [ ''Player, ''Position, ''GravVelocity, ''Model, ''Mana, ''Gravity, ''Input, ''ElapsedTime]

newtype VisibleGameState = VisibleGameState
  { models :: [(Model, Position)]
  }

-- | Move between monads in an SF, while keeping time information.
morphSF :: (Monad m2, Monad m1) => (forall c. m1 c -> m2 c) -> SF m1 a b -> SF m2 a b
-- I think this would be as safe as morphS, but who knows
morphSF nat = morphS (mapReaderT nat) 

msfToCoroutine :: Monad m => a -> MSF m a b -> Coroutine (Request b a) m void
msfToCoroutine a0 msf = do
  -- Lazy because we don't want to do anything odd with semantics
  -- that 'reactimateB' doesn't already do.
  flip State.Lazy.evalStateT a0 $
    reactimateB $ proc () -> do
      a <- constM State.Lazy.get -< ()
      b <- morphS (lift . lift) msf -< a
      a' <- arrM (lift . request) -< b
      arrM State.Lazy.put -< a'
      returnA -< False -- do not terminate
  error "unreachable"

sfToCoroutine :: Monad m => a -> SF m a b -> Coroutine (Request b (Timespan, a)) m void
sfToCoroutine a0 sf = msfToCoroutine (Timespan 0, a0) sf'
  where
    sf' = arr (first seconds) >>> runReaderS sf

taskToCoroutine :: Monad m => a -> Task m a b r -> Coroutine (Request b (Timespan, a)) m r
taskToCoroutine a0 task = do
  e <- runExceptT $ pogoStick (\(Request e k) -> case e of
    Left b -> lift (lift (request b)) >>= k
    Right r -> lift (throwE r)
    ) (mapMonad (lift . lift) co)
  case e of
    Right v -> absurd v
    Left r -> pure r
  where
    sf = runTask task
    co = sfToCoroutine a0 sf

generaliseSF :: Monad m => SF Identity a b -> SF m a b
generaliseSF = morphSF (pure . runIdentity)

seconds :: Timespan -> Double
seconds Timespan{getTimespan} = (fromIntegral getTimespan) / 10^(9 :: Int)

getVisibleGameState :: MonadIO m => SystemT World m VisibleGameState
getVisibleGameState = do
  models <- cfold (flip (:)) []
  pure VisibleGameState{..}

game :: MonadIO m => Coroutine (Request VisibleGameState (Timespan, UserInput)) (SystemT World m) void
game = do
  lift initGame
  w inputSmoother $ w updateTimespan $
    fix \k -> do
      s <- lift getVisibleGameState
      _ <- request s

      lift tick

      k
  where
    w = weave sequentialBinder weaveBroadcast
    -- | Weave stepper that combines a nonterminating await-request pair
    -- into a single Request. Broadcasts input to both.
    weaveBroadcast weaver result1 result2 =
      case (result1, result2) of
        (Right v, _) -> absurd v
        (_, Right v) -> absurd v
        (Left (Await k1), Left (Request s k2)) -> do
          u <- request s
          weaver (k1 u) (k2 u)

-- Update any 'Input' components with the current smoothed user input.
inputSmoother :: MonadIO m => Coroutine (Await (Timespan, UserInput)) (SystemT World m) void
inputSmoother =
  flip pogoStickM processInput' \(Request si k) -> do
      lift $ cmap \(Input _) -> Input si
      (t, u) <- await
      let u' = eventIfNonNull u
      pure $ k (t, u')
  where
    eventIfNonNull u = if nullInput u then NoEvent else Event u
    processInput' = sfToCoroutine NoEvent (generaliseSF processInput)

updateTimespan :: MonadIO m => Coroutine (Await (Timespan, u)) (SystemT World m) void
updateTimespan = forever do
  (dt', _) <- await
  lift $ modify global \(ElapsedTime dt) -> ElapsedTime (dt <> dt')

initGame :: MonadIO m => SystemT World m ()
initGame = do
  newEntity_ (Player, Input mempty, WizardModel, Position 0, GravVelocity 0, Mana 100 100)
  modify global \(Gravity _) -> Gravity (Sum (V2 0 (-9.8)))

tick :: MonadIO m => SystemT World m ()
tick = do
  cmap \(ggv :: GravVelocity, Position p) -> case ggv of
    Grounded ->
      if nearZero (p ^. _y) || (p ^. _y) < 0
        then Left ()
        else Right $ Right (GravVelocity 0)
    GravVelocity gv ->
      if (gv ^. _y < 0) && (nearZero (p ^. _y) || (p ^. _y) < 0)
        then Right $ Left (Grounded, Position $ (_y .~ 0) p)
        else Left ()
  
  -- cmap \(GravVelocity gv, Gravity (Sum g)) -> (GravVelocity (gv + g ^* (seconds dt)))
  
  -- cmap \(GravVelocity gv, Position p) -> (Position (p + gv ^* (seconds dt)))
