{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
module App.UI.GameScreen.TerminalEmulator (terminalEmulator) where

import System.Terminal.Emulator.Term
import Data.Automaton
import System.Terminal.Emulator.KeyboardInput
import Data.ByteString (ByteString)
import Data.Sequence
import System.Terminal.Emulator.Term.Process
import Control.Arrow
import System.Terminal.Emulator.Term.Resize (resizeTerm)
import Control.Lens.Operators
import System.Terminal.Emulator.TermLines (toSeq)
import System.Terminal.Emulator.KeyboardInput.KeyPressToPty (keyPressToPty)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Data.Attoparsec.Text hiding (Result)
import qualified Data.Automaton.Trans.Except as A
import Control.Monad.Trans.Except
import System.Terminal.Emulator.Parsing
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Text.Lazy (LazyText)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LT
import Data.Stream (StreamT(StreamT))
import Control.Monad.Reader
import Control.Monad.Fix
import Data.Stream.Result

-- | (user input, process output, resize) -> (terminal output, process input)
terminalEmulator
  :: Monad m
  => (Int, Int) -- ^ Initial size
  -> Automaton m (Maybe KeyPress, LazyByteString, Maybe (Int, Int)) (Seq TermLine, ByteString)
terminalEmulator (width0, height0) = feedback term0 $
  proc ((userInput, processOut, newSize), term) -> do
    let term' = maybe term (resizeTerm term) newSize
        rows = toSeq $ (if term ^. altScreenActive
            then mempty
            else term ^. scrollBackLines) <> term ^. activeScreen
        kbs = termGetKeyboardState term'
        bsIn = maybe mempty (keyPressToPty kbs) userInput
    poutChunks <- arr LT.toChunks <<< automatonDecodeUtf8Lenient -< processOut
    atoms <- arr concat <<< foreach atomParser -< poutChunks
    let (extraInput, term'') = processTermAtoms atoms term'
    returnA -< ((rows, bsIn <> extraInput), term'')
  where
    -- parseTermAtom always succeeds
    atomParser =
      (A.exceptS . A.runAutomatonExcept $ automatonParser parseTermAtom)
      >>> arr (either (\p -> error $ "impossible parse error: " ++ show p) id)
    term0 = mkTerm (width0, height0)

automatonDecodeUtf8Lenient :: Monad m => Automaton m LazyByteString LazyText 
automatonDecodeUtf8Lenient =
  feedback (T.streamDecodeUtf8With T.lenientDecode) $
    arr \(lbs, f) ->
      let res = LB.foldrChunks (\bs (t, f') -> let T.Some t' _ next = f' bs in (t <> LT.fromText t', next)) (mempty, f) lbs
       in first LT.toLazyText res

data ParseError = ParseError { unconsumed :: Text, errorContexts :: [String], errorMessage :: String } deriving Show

automatonParser :: Monad m => Parser b -> A.AutomatonExcept Text [b] m ParseError
automatonParser parser0 = A.try . feedback (parse parser0) $ arrM (go mempty)
  where
    go leftover (t, p) = 
      case p (leftover <> t) of
        Fail unconsumed errorContexts errorMessage -> throwE ParseError{..}
        Partial k -> pure (mempty, k)
        Done leftover' result -> first (result:) <$> go leftover' (t, parse parser0)

-- | Sequentially execute the automaton on each element of the list, until the list is empty.
foreach :: Monad m => Automaton m a b -> Automaton m [a] [b]
foreach = handleAutomaton f
  where
    f (StreamT s0 g) = StreamT s0 $ fix \k s -> do
      aas <- ask
      case aas of
        [] -> pure (Result s [])
        (a:as) -> do
          Result s' b <- lift $ runReaderT (g s) a
          Result s'' rest <- local (const as) $ k s'
          pure $ Result s'' (b:rest)
