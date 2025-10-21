{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import App.Thread
import Control.Concurrent.STM
import Control.Exception
import App.UI.GameScreen.Terminal
import Control.Lens.Operators
import Brick
import Data.Void
import Graphics.Vty.Attributes
import Graphics.Vty.Input
import qualified Data.Text as T
import Control.Lens
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Spell (Spell(..), SpellF(..))
import Spell.IO
import Control.Monad.Free
import Data.Foldable
import Data.Maybe
import Control.Applicative
import Control.Monad.State

newtype AppState = AppState { _term :: Terminal }

makeLenses ''AppState

data AppEvent = ReplStatusUpdate | Output String

-- | An equivalence relation for ReplStatus
sameStatus :: ReplStatus -> ReplStatus -> Bool
sameStatus Initialising Initialising = True
sameStatus Unblocked Unblocked = True
sameStatus Blocked Blocked = True
sameStatus (Dead _) (Dead _) = True
sameStatus _ _ = False

main :: IO ()
main =
  newTQueueIO >>= \oq ->
  withAppThread \th ->
  withReplThread \rth ->
  withBrickThread th (theapp rth) s0 \bth ->
  withAsync (interpreter rth bth oq) \ith ->
  -- sink for parent bth output
  withAsync (forever . atomically $ takeBrickThread bth) \bthSink ->
  withAsync
    -- Inform BrickThread of ReplStatus or of repl output
    do
      lastReplStatus <- newEmptyTMVarIO
      forever $ atomically do
        b <- isBrickQueueEmpty bth
        check b
        sentOutput <- isJust <$> optional do
          out <- flushTQueue oq
          err <- many (maybe retry pure =<< getReplResult rth)
          let s = out ++ err
          check (not $ null s)
          sendBrickEvent bth (Output s)
        sentUpdate <- isJust <$> optional do
          rs <- replStatus rth
          tryReadTMVar lastReplStatus >>= check . maybe True (not . sameStatus rs)
          writeTMVar lastReplStatus rs
          sendBrickEvent bth ReplStatusUpdate
        check (sentOutput || sentUpdate)
    \rthToBth -> do
      link bthSink
      link rthToBth
      res <- race (atomically $ waitBrickThread bth) (atomically $ waitCatchSTM ith)
      case res of
        -- bth
        Left (Left e) -> throwIO e
        Left (Right _) -> pure ()
        -- ith
        Right (Left e) -> throwIO e
        Right (Right v) -> absurd v
  where
    s0 = AppState { _term = (prompt .~ unblockedPrompt) terminal}

unblockedPrompt :: String
unblockedPrompt = "foo> "

blockedPrompt :: String
blockedPrompt = ""

initialisingPrompt :: String
initialisingPrompt = "Initialising..."

blockTerm :: MonadState Terminal m => m ()
blockTerm = do
  prompt .= blockedPrompt
  blocked .= True

unblockTerm :: MonadState Terminal m => m ()
unblockTerm = do
  prompt .= unblockedPrompt
  blocked .= False

interpreter :: ReplThread -> BrickThread s AppEvent Char -> TQueue Char -> IO a
interpreter rth bth oq = forever do
  InterpretRequest{..} <- atomically $ takeInterpretRequest rth
  let ~(Spell s0) = toInterpret
      itr :: Free SpellF a -> IO a
      itr = wrapExceptions . \case
        Pure a -> pure a -- done
        -- Anything that a side effect is strict in, should be annotated with a
        -- bang pattern on the match so that it gets covered by wrapExceptions.
        Free (PutChar !c next) -> do
          atomically $ writeTQueue oq c
          itr next
        Free (GetChar next) -> do
          -- record next user input:
          bth' <- atomically $ dupBrickThread bth
          -- This is brittle: if we have multiple consecutive GetChar effects,
          -- there may be brief intervals between them where we are not listening
          -- for user input.
          -- However, I don't care, because the alternative is prone to leaking
          -- memory through holding on to user input indefinitely, and GetChar
          -- is not an effect expected to be used often.
          -- Maybe having another thread write input to a TBChan is an option,
          -- though.
          c <- atomically $ takeBrickThread bth'
          itr (next c)
        Free (Face (!a,!b) next) -> do
          itr $ unSpell do
            Spell.IO.putStrLn $ "Face: " ++ wrapExceptions (show (a,b))
            Spell next
        Free (Firebolt next) -> do
          itr $ unSpell do
            Spell.IO.putStrLn "Firebolt"
            Spell next
        Free (Catch ~(Spell expr) h next) -> do
          a <- catch
            (itr expr)
            \(~(UncaughtSpellException e)) -> case wrapExceptions (h e) of
              Nothing -> throwIO (UncaughtSpellException e)
              Just ~(Spell x) -> itr x
          itr (next a)
        Free (Throw !e) -> throwIO (UncaughtSpellException e)
  catch
    do withAsync
        (itr s0)
        \a -> atomically do
          submit <- tryReadTMVar submitResponseHere
          -- in the case that submit is Nothing, means we do not wait for 'a'
          -- and exit the atomically block, killing the async thread.
          traverse_ ((waitSTM a >>=) . (. Right)) submit
    -- waitSTM will rethrow exceptions from 'a'
    \(~(UncaughtSpellException e)) -> atomically do
      submit <- tryReadTMVar submitResponseHere
      traverse_ ($ Left e) submit
  where
    wrapExceptions = mapException UncaughtSpellException

newtype UncaughtSpellException = UncaughtSpellException SomeException deriving Show
instance Exception UncaughtSpellException

theapp :: ReplThread -> TChan Char -> App AppState AppEvent ()
theapp rth ic = App {..}
  where
    appDraw s = [viewport () Vertical $ drawTerminal () (s ^. term)]

    appAttrMap _ = attrMap defAttr []

    appChooseCursor = showFirstCursor

    appHandleEvent (VtyEvent (EvKey KEsc _)) = halt
    appHandleEvent (VtyEvent (EvKey (KChar 'c') ms)) | MCtrl `elem` ms = do
      interrupted <- liftIO $ atomically do
        b <- replStatus rth
        case b of
          Blocked -> do
            interruptRepl rth
            pure True
          _ -> pure False
      when interrupted do
        -- running interruptRepl guarantees an immediate unblock for submitRepl
        Brick.zoom term unblockTerm
        Brick.zoom term $ pushOutputLine "Interrupted."
    appHandleEvent (AppEvent (Output s)) =
      Brick.zoom term $ traverse_ pushOutput s
    appHandleEvent (AppEvent ReplStatusUpdate) = do
      s <- get
      s' <- liftIO . atomically $ handleReplStatusChange s
      put s'

    appHandleEvent e = do
      s <- get
      s' <- liftIO $ atomically do
        s' <- handleReplStatusChange s
        let (bs, s'')
              = flip runState s'
              . fmap T.unlines
              . Brick.zoom term
              $ handleTerminalEvent e
        -- since we've handled the repl status change, term will be blocked iff
        -- the REPL is blocked, so submitRepl will not block
        if T.null bs || bs == "\n"
          then pure s''
          else do
            submitRepl rth $ T.unpack bs
            pure $ execState (Brick.zoom term blockTerm) s''
      put s'
      case e of
        -- push char input to stdout, whether we are blocked or not
        (VtyEvent (EvKey (KChar c) _)) -> liftIO . atomically $ writeTChan ic c
        _ -> pure ()

    appStartEvent = do
      s <- get
      s' <- liftIO . atomically $ handleReplStatusChange s
      put s'

    handleReplStatusChange s = do
      rs <- replStatus rth
      let f = pure . flip execState s
      case rs of
        Initialising -> f $ Brick.zoom term do
          blocked .= True
          prompt .= initialisingPrompt
        Blocked -> f (Brick.zoom term blockTerm)
        Unblocked -> f (Brick.zoom term unblockTerm)
        Dead (Left e) -> throwSTM e
        Dead (Right e) -> throwSTM . userError $ "Interpreter error: " ++ show e

-- commented out because there is a problem:
-- if we allow for :{, :}-style multiline input, and if we also allow for pasting
-- multiple lines into the terminal: that means we need a queue for REPL inputs,
-- as we could submit multiple :{, :} blocks at once.
-- Currently, there is no queue: it's just one input at a time.
-- May have to just allow entering multiple lines in one line into the REPL, by
-- e.g. taking a new line on shift+enter.
-- This also has a potential usability advantage over GHCi by allowing the
-- player to edit previously-entered lines in a block.


-- | Convert a list of lines into a list of blocks of lines which should be
-- interpreted together, separated by :{ and :} like in GHCi.
--
-- Leading and trailing whitespace is ignored for :{ and :}.
--
-- The second list is the remainder in case :{ is not terminated by a :}.
--
-- (partially based on TidalCycles' Main module)
-- blocks :: [Text] -> ([Text], [Text])
-- blocks [] = ([], [])
-- blocks ((T.strip -> ":{") : ls) =
--   if null ls'
--     then ([], b)
--     else first (T.unlines b :) $ blocks (drop 1 ls')
--   where
--     (b, ls') = span ((/= ":}") . T.strip) ls
-- blocks (l : ls) = first (l :) $ blocks ls