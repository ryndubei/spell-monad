{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}

module Game (runSystem, game, initWorld, World, VisibleGameState(..), Model(..)) where

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
import Data.Foldable
import Data.Sequence
import App.Thread.Repl (InterpretRequest(..))
import Control.Applicative

data Player = Player
instance Component Player where
  type Storage Player = Map Player

data Model = WizardModel | FireboltModel | ShapeModel [Convex]
instance Component Model where
  type Storage Model = Map Model

-- | The entity possessing this unique component is the one to which user input
-- gets routed to.
newtype Input = Input SimInput
instance Component Input where
  type Storage Input = Unique Input

makeWorld "World" [ ''Player, ''Physics, ''Model, ''Input]

data VisibleGameState = VisibleGameState
  { models :: [(Model, Position)]
  , cameraX :: !Double
  , cameraY :: !Double
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
  let cameraX = 0
      cameraY = 0
  pure VisibleGameState{..}

data GameInput = GameInput
  { gameInput :: Event UserInput
  , termStdin :: Event (Seq Char)
  , interpretRequest :: Event (Maybe InterpretRequest)
  }

instance Semigroup GameInput where
  (<>) gi1 gi2 = GameInput
    { gameInput = mergeBy (<>) (gameInput gi1) (gameInput gi2)
    , termStdin = mergeBy (<>) (termStdin gi1) (termStdin gi2)
    , interpretRequest = mergeBy (<|>) (interpretRequest gi1) (interpretRequest gi2)
    }

game :: MonadIO m => Coroutine (Request VisibleGameState (Timespan, GameInput)) (SystemT World m) void
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
inputSmoother :: MonadIO m => Coroutine (Await (Timespan, GameInput)) (SystemT World m) void
inputSmoother =
  flip pogoStickM processInput' \(Request si k) -> do
      lift $ cmap \(Input _) -> Input si
      (t, GameInput{gameInput = u}) <- await
      pure $ k (t, u)
  where
    processInput' = sfToCoroutine NoEvent (generaliseSF processInput)

playerShape :: [Convex]
playerShape = [oCircle (V2 0 0.9) 0.9, oCircle (V2 0 1.5) 1]

levelGeometry :: [Convex]
levelGeometry = [ oRectangle (V2 (-10) (-10)) (V2 20 20) ]

initGame :: MonadIO m => SystemT World m ()
initGame = do

  player <- newEntity
    ( Player
    , ShapeModel playerShape
    , DynamicBody
    , Position (V2 0 0)
    , Density 1
    , Moment (1 / 0) -- forces player to always remain upright
    , Friction 1
    )
  cmap \Player -> Input mempty
  traverse_ (newEntity_ . Shape player) playerShape

  level <- newEntity (KinematicBody, Position 0, ShapeModel levelGeometry, Friction 1)
  traverse_ (newEntity_ . Shape level) levelGeometry

  Apecs.set global earthGravity

tick :: MonadIO m => Timespan -> SystemT World m ()
tick dt = do
  cmap \(Input si) -> SurfaceVelocity (si ^. moveVector)
  cmapIf (\(Input SimInput{simJump}) -> (simJump == Event ())) $ \(Input _) -> Force (V2 0 (1000*seconds dt))

  stepPhysics (seconds dt)
  
  