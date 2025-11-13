{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Spell.Eval (evalSpell) where

import Spell (SpellT(..), SpellF(..), mapSpellException, mapSpellFException, Spell(..), generaliseSpell, SomeSpellException(..), hoistSpellT')
import Data.Functor.Identity
import Control.Monad.Trans.Free
import Control.DeepSeq
import Data.Some
import Data.Functor.Product
import Control.Monad
import Control.Monad.Trans.Class
import Control.Lens
import Data.Functor.Compose
import Data.Kind
import DependentSums
import Untrusted
import Control.Exception
import Control.Monad.Trans.Reader
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Concurrent.Async
import Foreign (Int64)
import Control.Monad.Trans.Writer
import Data.Bifunctor (second)
import Control.Monad.Trans.Except
import Control.Category ((>>>))
import Control.Monad.Fix
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.Void
import Control.Monad.Trans.State.Strict
import Data.Dependent.Map (DMap)
import qualified Data.Unique.Tag as PrimUniq
import Control.Monad.ST (RealWorld)
import Data.IORef
import System.IO.Unsafe

data Instance c a = c a => Instance

untrustedAllocationLimit :: Int64
untrustedAllocationLimit = 2^(20 :: Int64) -- 10MiB in bytes

runEvalUntrusted :: EvalT Untrusted IO a -> IO a
runEvalUntrusted (EvalT r) = do
  -- Sorry for the type signature
  q <- newTQueueIO :: IO (TQueue (TVar Bool, Some (Untrusted `Product` Compose TMVar (Either SomeException) `Product` Instance NFData)))
  withAsync
    do
      forever do
        -- TODO: evaluate multiple Untrusted at once
        (alive, s) <- atomically $ readTQueue q
        withSome s \(Pair (Pair u (Compose response)) Instance) -> do
          withTrusted (Just untrustedAllocationLimit) u $ \a -> do
            atomically $ orElse
              (readTVar alive >>= check . not) -- response no longer needed 
              (waitSTM a >>= writeTMVar response)
    \a -> do
      link a
      let env = EvalEnv {
        startEval = \u -> do
          alive <- newTVarIO True
          response <- newEmptyTMVarIO
          atomically $ writeTQueue q (alive, Some (u `Pair` Compose response `Pair` Instance))
          let pollEval = atomically $ tryReadTMVar response
              cancelEval e = atomically do
                writeTVar alive False
                writeTMVar response (Left e)
          pure $ EvalHandle { pollEval, cancelEval }
      }
      x <- runReaderT r undefined
      undefined

data EvalHandle n x = EvalHandle
  { pollEval :: n (Maybe (Either SomeException x))
  , cancelEval :: SomeException -> n ()
  } deriving Functor

newtype EvalEnv u n = EvalEnv { startEval :: forall x. NFData x => u x -> n (EvalHandle n x) }

-- | Some delayed evaluation strategy of 'u' in 'n'.
newtype EvalT u n a = EvalT { runEvalT :: ReaderT (EvalEnv u n) n a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (EvalT u) where
  {-# INLINE lift #-}
  lift = EvalT . lift

-- | Evaluate using EvalT. _Failed evaluations are silently truncated with Throw_.
--
-- In the monad product, the left choice is poll, and the right choice is to
-- cancel any evaluation that is running and return the passed exception as a
-- Throw, unless the result had already been evaluated to an exception.
-- In the latter case, the returned Throw is for the earlier exception.
--
-- If the evaluation is never cancelled, then it could just keep running indefinitely.
-- Run EvalT with an allocation limit or traverse every SpellT fully to avoid this.
-- (both is best)
evalSpell'
  :: forall u n a. (Monad u, Monad n, MonadIO n)
  => (forall k (f :: k -> Type). u (Some f) -> Some (Compose u f)) -- ^ u must preserve hidden types
  -> u (Spell a)
  -> SpellT (u SomeException) (MaybeT (EvalT u n) `Product` ReaderT SomeException (EvalT u n)) (u a)
evalSpell' uCommSome u = undefined
  where
    {-# NOINLINE unsafeIORef #-}
    unsafeIORef :: forall x. IO (IORef x) -> IORef x
    unsafeIORef = unsafePerformIO

    u' :: SpellT SomeException u a
    u' = join . lift $ fmap (mapSpellException (\(SomeSpellException e) -> pure $ SomeException e) (\(SomeException e) -> pure $ SomeSpellException e) . generaliseSpell) u

    u'' :: SpellT (u SomeException) (ExceptT SomeException (MaybeT (WriterT (Semicolonable (ReaderT SomeException (EvalT u n))) (EvalT u n)))) (u a)
    u'' = evalSpell uCommSome (
      -- After a lot of thinking, I could not think of a better way of doing this.
      -- The only alternative is rewriting 'evalSpell' to accept an Applicative for n,
      -- so that we can compose monads.
      let rh = unsafeIORef (newIORef Nothing)
      in \x -> do
        EvalHandle{pollEval, cancelEval} <- do
          liftIO (readIORef rh) >>= \case
            Nothing -> do
              EvalEnv{startEval} <- lift . lift . lift $ EvalT ask
              h <- lift . lift . lift . lift $ startEval x
              liftIO $ writeIORef rh (Just h)
              pure h
            Just h -> pure h
        ExceptT . MaybeT $ WriterT do
          p' <- lift pollEval
          let c' = mapReaderT lift $ ReaderT cancelEval
          pure (p', Semicolonable c')
      ) u'
    
    u''' :: SpellT (u SomeException) (MaybeT (EvalT u n) `Product` ReaderT SomeException (EvalT u n)) (u a)
    u''' = unSpellT u'' & fix \k -> \case
      FreeT (ExceptT (MaybeT (WriterT m))) -> SpellT . FreeT $ Pair
        do
          (a, _) <- lift m
          let a' = fmap (fmap $ fmap k) a
              a'' = fmap (\case Left e -> Throw undefined) a'
          MaybeT undefined
        do undefined

frontloadFreeT :: (Functor f, Functor m) => FreeT f m a -> Free (Compose m (Free f)) a
frontloadFreeT (FreeT m) =
  FreeT (Identity (Free (Compose $ fmap (FreeT . fmap (bimap pure (pure . frontloadFreeT)) . Identity) m)))

-- | Monoid instance for sequentially composing Applicatives.
newtype Semicolonable f = Semicolonable (f ())

instance Applicative f => Semigroup (Semicolonable f) where
  (<>) (Semicolonable a) (Semicolonable b) = Semicolonable $ a <* b

instance Applicative f => Monoid (Semicolonable f) where
  mempty = Semicolonable (pure ())


-- | Turn a particular unnatural transformation into a natural transformation.
evalSpell
  :: forall u n a e. (Monad u, Monad n)
  => (forall k (f :: k -> Type). u (Some f) -> Some (Compose u f)) -- ^ u must preserve hidden types
  -> (forall x. NFData x => u x -> n x)
  -> SpellT e u a
  -> SpellT (u e) n (u a)
evalSpell uCommSome f (SpellT (FreeT m)) = do
  pureOrFree <- lift . pullEither' $ fmap (\case Pure a -> Left a; Free a -> Right a) m
  case pureOrFree of
    Left a -> pure a
    Right u -> do
      spellF <- lift $ pullSpellF' u
      let spellF' = unSpellT . evalSpell uCommSome f . join . lift . fmap SpellT <$> spellF
          spellT = SpellT . FreeT . pure $ Free spellF'
      spellT
  where
    pullEither :: forall x y. u (Some (Product Identity (Tag (Either x y)))) -> n (Either (u x) (u y))
    pullEither u0 = do
      let u1 = uCommSome u0
      withSome u1 \(Compose u2) -> do
        let u3 = fmap (\(Pair (Identity a) b) -> (a,b)) u2
            uargs = fmap fst u3
            utag = fmap snd u3
        tag <- f utag
        case tag of
          TLeft -> pure $ Left uargs
          TRight -> pure $ Right uargs

    pullEither' :: forall x y. u (Either x y) -> n (Either (u x) (u y))
    pullEither' = pullEither . fmap toDSum

    pullMaybe' :: forall x. u (Maybe x) -> n (Maybe (u x))
    pullMaybe' = pullMaybe . fmap toDSum

    pullMaybe :: forall x. u (Some (Product Identity (Tag (Maybe x)))) -> n (Maybe (u x))
    pullMaybe u0 = do
      let u1 = uCommSome u0
      withSome u1 \(Compose u2) -> do
        let u3 = fmap (\(Pair (Identity a) b) -> (a,b)) u2
            uargs = fmap fst u3
            utag = fmap snd u3
        tag <- f utag
        case tag of
          TJust -> pure $ Just uargs
          TNothing -> pure Nothing

    pullSpellF' :: forall next. u (SpellF e u next) -> n (SpellF (u e) n (u next))
    pullSpellF' = pullSpellF . fmap (toDSum . mapSpellFException (pure . pure) id)

    pullSpellF :: forall next. u (Some (Product Identity (Tag (SpellF (u e) u next)))) -> n (SpellF (u e) n (u next))
    pullSpellF u0 = do
      let u1 = uCommSome u0
      withSome u1 \(Compose u2) -> do
        let u3 = fmap (\(Pair (Identity a) b) -> (a,b)) u2
            u = fmap fst u3
        tag <- f $ fmap snd u3
        case tag of
          TFirebolt -> pure $ Firebolt (pure (join u))
          TFace -> do
            a <- f $ fmap (view _1) u
            b <- f $ fmap (view _2) u
            let next = u >>= view _3
            pure $ Face a b (pure next)
          TThrow -> do
            let e = join $ join u
            pure . Throw $ pure e
          TCatch -> do
            let expr = mapSpellException (pure . join) (pure . pure) . evalSpell uCommSome f . join . lift $ u >>= view _1
                h e = fmap (fmap (mapSpellException (pure . join) (pure . pure) . evalSpell uCommSome f . join . lift)) . pullMaybe' . join . (<*> pure e) $ u >>= view _2
                next = (<*>) (u >>= view _3)
            pure $ Catch (pure expr) (pure h) (pure next)
          TPutChar -> do
            c <- f $ fmap (view _1) u
            let next = u >>= view _2
            pure $ PutChar c (pure next)
          TGetChar -> do
            let next = (<*>) (join u)
            pure $ GetChar (pure $ next . pure)
