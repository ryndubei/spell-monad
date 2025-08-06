{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE LambdaCase #-}
-- | Module that runs haskeline in a separate POSIX process by necessity
-- (haskeline only supports user interaction via stdin/stdout)
--
-- Breaks windows compatibility, so should be replaced at some point
module App.UI.GameScreen.Haskeline
  ( module Some
  , HaskelineThread(interactHandleR, interactHandleW, responseHandle, linesHandle)
  , newHaskelineThread
  , haskelineRhine
  , HaskelineThreadExitClock(..)
  , HaskelineThreadLineClock(..)
  , HaskelineThreadOutputClock(..)
  , ReplResult(..)
  ) where

-- base
import System.IO
-- resourcet
import Control.Monad.Trans.Resource
-- unix
import System.Posix.Process
import System.Posix
import Control.Monad
import System.Console.Haskeline
import FRP.Rhine.Clock
import Control.Monad.IO.Class
import Data.Time
import Data.Void
import Data.Automaton
import Control.Monad.Trans.Except
import Control.Monad.Fix
import Data.Text (Text)
import FRP.Rhine
import qualified Data.Text.IO as T
import Data.Some.Newtype as Some
import qualified Data.Text as T
import System.Terminal.Emulator.KeyboardInput
import Data.Sequence (Seq)
import System.Terminal.Emulator.Term.Process
import Control.Monad.Schedule.Class
import qualified Data.ByteString as B
import App.UI.GameScreen.TerminalEmulator
import Data.Word
import qualified Data.Automaton.Trans.Except as A
import Orphans ()
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Control.Monad.Trans.Class
import Control.Exception (finally, bracket)
import LogClock
import TextShow

-- maybe
-- foreign import capi "pty.h/login_tty" a :: CInt -> IO ()

-- |
-- - raw user input should be read and terminal state displayed from interactHandle
--
-- - after each line of user input, HaskelineThread will additionally wait for
-- feedback from the main program. It should be written to userInputResponseHandle.
-- A NUL character written to userInputResponseHandle unblocks user input.
--
-- - input lines as interpreted by Haskeline will be sent to userLinesHandle
data HaskelineThread s = HaskelineThread
  { linesHandle :: !Handle -- ^ read
  , interactHandleR :: !Handle -- ^ read (terminal output)
  , interactHandleW :: !Handle -- ^ write (kb)
  , responseHandle :: !Handle -- ^ write
  , pid :: !ProcessID
  }
type role HaskelineThread nominal

newtype HaskelineThreadExitClock s = HaskelineThreadExitClock (HaskelineThread s)

instance MonadIO m => Clock (ExceptT (Maybe ProcessStatus) m) (HaskelineThreadExitClock s) where
  type Time (HaskelineThreadExitClock s) = UTCTime
  type Tag (HaskelineThreadExitClock s) = Void
  initClock (HaskelineThreadExitClock HaskelineThread{pid}) = do
    t0 <- liftIO getCurrentTime
    let rc = constM do
          ps <- liftIO $ getProcessStatus
            False -- no WNOHANG (blocks until status change)
            True -- WUNTRACED (throw if HaskelineThread is stopped but not terminated)
            pid
          throwE ps
    pure (rc, t0)

instance GetClockProxy (HaskelineThreadExitClock s)

newtype HaskelineThreadOutputClock s = HaskelineThreadOutputClock (HaskelineThread s)

instance MonadIO m => Clock m (HaskelineThreadOutputClock s) where
  type Time (HaskelineThreadOutputClock s) = UTCTime
  type Tag (HaskelineThreadOutputClock s) = Word8
  initClock (HaskelineThreadOutputClock HaskelineThread{interactHandleR}) = do
    t0 <- liftIO getCurrentTime
    let rc = safely do
          A.try $ constM do
            bs <- liftIO $ B.hGetSome interactHandleR 1
            case B.uncons bs of
              Nothing -> throwE ()
              Just (c, _) -> do
                t <- liftIO getCurrentTime
                pure (t, c)
          c <- fmap fst . lift $ initClock Never
          A.safe (c >>> arr (absurd . snd))
    pure (rc, t0)

instance GetClockProxy (HaskelineThreadOutputClock s)

newtype HaskelineThreadLineClock s = HaskelineThreadLineClock (HaskelineThread s)

instance MonadIO m => Clock m (HaskelineThreadLineClock s) where
  type Time (HaskelineThreadLineClock s) = UTCTime
  type Tag (HaskelineThreadLineClock s) = Text
  initClock (HaskelineThreadLineClock HaskelineThread{linesHandle}) = do
    t0 <- liftIO getCurrentTime
    let rc = constM do
          l <- liftIO $ T.hGetLine linesHandle
          t <- liftIO getCurrentTime
          pure (t, l)
    pure (rc, t0)

instance GetClockProxy (HaskelineThreadLineClock s)

newHaskelineThread :: MonadResource m => m (ReleaseKey, Some HaskelineThread)
newHaskelineThread =
  allocate
    do
      -- (read/write relative to this thread)
      (responseR, responseW) <- createPipe
      (linesR, linesW) <- createPipe
      (interactR, interactR') <- createPipe
      (interactW', interactW) <- createPipe
      pid <- forkProcessWithUnmask \u -> do
        finally
          do u do
              void $ interactW' `dupTo` stdInput
              void $ interactR' `dupTo` stdOutput
              void $ interactR' `dupTo` stdError
              closeFd responseW
              closeFd linesR
              closeFd interactR
              closeFd interactW
              setEnv "TERM" "xterm" True
              responseRH <- fdToHandle responseR
              linesWH <- fdToHandle linesW
              -- TODO: provide proper environment for terminal-style interaction to work
              runInputTBehaviorWithPrefs defaultBehavior defaultPrefs defaultSettings (haskelineProgram responseRH linesWH)
          do
            closeFd interactW'
            closeFd interactR'
      closeFd responseR
      closeFd linesW
      closeFd interactR'
      closeFd interactW'
      interactHandleR <- fdToHandle interactR
      interactHandleW <- fdToHandle interactW
      responseHandle <- fdToHandle responseW
      linesHandle <- fdToHandle linesR
      pure $ Some HaskelineThread {..}
    \case
      (Some HaskelineThread{..}) -> do
        hClose linesHandle
        hClose responseHandle
        hClose interactHandleR
        hClose interactHandleW
        signalProcess sigTERM pid

haskelineProgram
  :: Handle -- ^ response read handle
  -> Handle -- ^ lines write handle
  -> InputT IO a
haskelineProgram responseRH linesWH = Control.Monad.forever do
  minput <- getInputLine "> "
  case minput of
    Nothing -> pure ()
    Just input -> do
      liftIO $ bracket
        (createFile "foo" regularFileMode)
        closeFd
        \fd -> do
          h <- fdToHandle fd
          hPutStrLn h input
      liftIO $ hPutStrLn linesWH input
      outputStr "Aaaaaaaa"
      -- | Print input from response handle until the first NUL character
      fix \k -> do
        c <- liftIO $ hGetChar responseRH
        if c == '\NUL'
          then outputStr "\n"
          else do
            outputStr [c]
            k

haskelineRhine
  :: forall m cl s.
     (MonadIO m, MonadSchedule m, MonadLog (LogMessage Text) m, Clock m cl, cl ~ Out cl, cl ~ In cl, Time cl ~ UTCTime, GetClockProxy cl)
  => HaskelineThread s
  -> cl -- ^ synchronous input clock of user's choice
  -> (Int, Int) -- ^ initial terminal size
  -> Rhine m
      (_ `ParallelClock` cl) -- In of hidden clock type ~ Never, so the Rhine
                             -- will take input at exactly 'cl' in practice
      (Maybe ReplResult, Maybe KeyPress, Maybe (Int, Int))
      (Seq TermLine) -- terminal window output
haskelineRhine hth cl size0 = rh
  where
    rh = (rhProcessOut +@+ (arr id @@ cl)) @>-^ clsfTerm
    clsfTerm :: ClSF m _ (Either Word8 (Maybe ReplResult, Maybe KeyPress, Maybe (Int, Int))) (Seq TermLine)
    clsfTerm = proc x -> do
      let (processOut, replResult, keyPress, newSize) = case x of
            Left w -> (Just w, Nothing, Nothing, Nothing)
            Right (rr, kp, ns) -> (Nothing, rr, kp, ns)
      (termLines, processIn) <- terminalEmulator size0 -< (keyPress, maybe LB.empty LB.singleton processOut, newSize)
      arrM (maybe (pure ()) (liftIO . consumeReplResult hth)) -< replResult
      arrM (liftIO . consumeProcessIn hth) -< processIn
      returnA -< termLines
    -- Using Never so that it does not take input
    rhProcessOut = (tagS @@ Never) @>>^ absurd
      >-- trivialResamplingBuffer
      --> tagS @@ logClockDebug @m "HaskelineThreadOutputClock" (HaskelineThreadOutputClock hth) showt

data ReplResult
  = ReplUnblock Text -- ^ if repl is currently blocked on response
                     -- (after each line input by the user), unblock it
                     -- after the response is displayed
  | ReplStatus Text -- ^ give incremental output to the user without unblocking

consumeReplResult :: HaskelineThread t -> ReplResult -> IO ()
consumeReplResult HaskelineThread{responseHandle} = T.hPutStr responseHandle . \case
  ReplUnblock t -> T.filter (/= '\NUL') t `T.snoc` '\NUL'
  ReplStatus t -> T.filter (/= '\NUL') t

consumeProcessIn :: HaskelineThread t -> ByteString -> IO ()
consumeProcessIn HaskelineThread{interactHandleW} = B.hPut interactHandleW
