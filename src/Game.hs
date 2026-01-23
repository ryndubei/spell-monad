{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}

module Game (runSystem, game, initWorld, World) where

import Apecs
import Input
import Control.Monad.IO.Class
import Chronos
import Control.Monad.Fix
import Linear
import Simulation.Input
import FRP.BearRiver
import Control.Lens
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.Coroutine
import Data.Void
import Control.Monad.Trans.Reader (mapReaderT)
import Control.Monad.Trans.Except
import Control.Monad.Trans.MSF (runReaderS)
import Data.MonadicStreamFunction.InternalCore (MSF(..))
import Apecs.Physics

data Player = Player
instance Component Player where
  type Storage Player = Map Player

data Model = WizardModel | FireboltModel
instance Component Model where
  type Storage Model = Map Model

data Mana = Mana { currentMana :: Double, maximumMana :: Double }
instance Component Mana where
  type Storage Mana = Map Mana

-- | The entity possessing this unique component is the one to which user input
-- gets routed to.
newtype Input = Input SimInput
instance Component Input where
  type Storage Input = Unique Input

makeWorld "World"
  [ ''Player, ''Physics, ''Model, ''Mana, ''Input]

type All = (Player, Position, Model, Mana, Gravity, Input)

destroyEntity :: forall w m. Destroy w m All => Entity -> SystemT w m ()
destroyEntity ety = destroy ety (Proxy @All)

newtype VisibleGameState = VisibleGameState
  { models :: [(Model, Position)]
  }

-- | Move between monads in an SF, while keeping time information.
morphSF :: (Monad m2, Monad m1) => (forall c. m1 c -> m2 c) -> SF m1 a b -> SF m2 a b
-- I think this would be as safe as morphS, but who knows
morphSF nat = morphS (mapReaderT nat) 

-- | Isomorphism.
msfToCoroutine  :: Monad m => MSF m a b -> a -> Coroutine (Request b a) m void
msfToCoroutine MSF{unMSF} a0 = Coroutine . fmap (\(b, msf') -> Left $ Request b (msfToCoroutine msf')) $ unMSF a0
-- | Isomorphism.
coroutineToMSF :: Monad m => (a -> Coroutine (Request b a) m Void) -> MSF m a b
coroutineToMSF f = MSF \a0 ->
  f a0 & \(Coroutine{resume}) ->
    resume <&> \case
      Right v -> absurd v
      Left (Request b k) -> (b, coroutineToMSF k)

sfToCoroutine :: Monad m => a -> SF m a b -> Coroutine (Request b (Timespan, a)) m void
sfToCoroutine a0 sf = msfToCoroutine sf' (Timespan 0, a0)
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
  w inputSmoother $
    fix \k -> do
      s <- lift getVisibleGameState
      (dt, _) <- request s

      lift $ tick dt

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

initGame :: MonadIO m => SystemT World m ()
initGame = do
  newEntity_ (Player, Input mempty, WizardModel, Mana 100 100)
  Apecs.set global earthGravity

tick :: MonadIO m => Timespan -> SystemT World m ()
tick dt = do
  undefined