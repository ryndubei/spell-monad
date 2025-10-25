{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module App.UI.GameScreen.Terminal
  ( Terminal
  , drawTerminal
  , handleTerminalEvent
  , terminal

  -- * Lenses
  , prompt
  , blocked
  , history
  , forceVisibleInputLine

  -- * Operations
  , enterLine
  , insertChar
  , deleteChar
  , historyPrev
  , historyNext
  , cursorRight
  , cursorLeft
  , cursorTo
  , cursorToEnd
  , pushOutputLine
  , pushOutput
  ) where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Control.Lens
import Brick
import Data.Foldable
import Control.Monad
import Control.Monad.State.Class
import Graphics.Vty.Input.Events
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (maybeToList, fromMaybe)
import Control.Monad.Fix
import Data.Bifunctor

-- | Expected to be used with a viewport.
--
-- TODO: not rely on using a viewport because it redraws constantly,
-- will be horrible when history gets large
data Terminal = Terminal
  { _history :: Seq Text -- ^ The visible terminal lines. Includes non-user output. Last output first. User inputs have 'prompt' prepended.
  , _outputLine :: Seq Char
      -- ^ The last output line. Visible when non-empty. Appended by pushOutput.
      -- Emptied and added to history on pushOutputLine and enterLine, or on
      -- pushOutput '\n'.
  , _inputHistory :: Seq Text -- ^ History for specifically inputs.
  , _inputHistoryIndex :: !Int -- ^ Index of the _current_ history entry. A completely fresh line
                               -- has index -1.
  , _lineChanged :: !Bool -- ^ If the line has been modified, it has to be recorded in history so that
                          -- we can return to it.
  , _inputLine :: Seq Char
  , _prompt :: String
  , _cursorOffset :: !Int -- ^ Offset from the end of inputLine, nonnegative.
  , _blocked :: !Bool
  , _forceVisibleInputLine :: !Bool -- ^ Whether to make a visibility request on the input line.
  }

terminal :: Terminal
terminal = Terminal
  { _history = mempty
  , _outputLine = mempty
  , _inputHistory = mempty
  , _inputHistoryIndex = -1
  , _lineChanged = False
  , _inputLine = mempty
  , _prompt = mempty
  , _cursorOffset = 0
  , _blocked = False
  , _forceVisibleInputLine = True
  }

-- | The next character will be inserted at this index.
cursorColumn :: Lens' Terminal Int
cursorColumn = lens (\Terminal{_cursorOffset, _inputLine} -> length _inputLine - _cursorOffset) (\t col -> t { _cursorOffset = length (_inputLine t) - col })

makeLenses ''Terminal

-- | Empty the input line and append the contents to the history.
-- (represents entering the line)
--
-- Nothing if terminal is blocked, or if the line is empty.
enterLine :: MonadState Terminal m => m (Maybe Text)
enterLine = do
  b <- use blocked
  if b
    then pure Nothing
    else do
      l <- use inputLine
      pr <- use prompt
      inputLine .= Seq.empty
      cursorOffset .= 0

      ol <- T.pack . toList <$> use outputLine

      let t = T.pack $ toList l
          tp = T.pack $ toList pr

      -- Push the current output line to the history, unless it's empty.
      unless (T.null ol) do
        outputLine .= mempty
        history %= (ol Seq.<|)

      -- Add 't' to history with the prompt prepended
      let hl = tp <> t
      if T.null hl
        then history %= (" " Seq.<|)
        else history %= (tp <> t Seq.<|)

      -- Add just the raw input to input history,
      -- unless the line is empty
      unless (T.null t) $ inputHistory %= (t Seq.<|)

      inputHistoryIndex .= (-1)
      lineChanged .= False
      pure (Just t)

-- | Insert a character at the current cursor position.
insertChar :: MonadState Terminal m => Char -> m ()
insertChar c = do
  b <- use blocked
  unless b do
    col <- use cursorColumn
    inputLine %= Seq.insertAt col c
    lineChanged .= True

insertLines :: MonadState Terminal m => ByteString -> m [Text]
insertLines l = do
  b <- use blocked
  if b then pure [] else do
    col <- use cursorColumn
    -- choice of foldr is deliberate so we insert last-first,
    -- means we can keep using the same column index
    inputLine %= BC.foldr (\c -> (Seq.insertAt col c .)) id l

    fix \k -> do
      iln <- use inputLine
      if null iln
        then pure []
        else do
          let
            -- remove at least one newline on each iteration so that this terminates
            (ilnl, ilnr) = second (Seq.drop 1) $ Seq.spanl (/= '\n') iln
          if null ilnr
            then
              -- we are on the last line, nothing remaining to insert
              pure []
            else do
              inputLine .= ilnl
              t <- enterLine
              inputLine .= ilnr
              (fromMaybe mempty t :) <$> k

-- | Push a non-input string immediately to the history (will not be shown
-- when repeating previous commands with up arrow). Represents output.
pushOutputLine :: MonadState Terminal m => Text -> m ()
pushOutputLine t = do
  l <- T.pack . toList <$> use outputLine
  outputLine .= mempty
  if T.null $ l <> t
    then
      -- Brick turns (txt mempty) into an empty widget, so we need to add an
      -- extra space to the line.
      history %= (" " Seq.<|)
    else history %= ((l <> t) Seq.<|)

-- | Push a single character to the output.
pushOutput :: MonadState Terminal m => Char -> m ()
pushOutput '\t' = replicateM_ 4 (pushOutput ' ')
pushOutput '\n' = pushOutputLine mempty
pushOutput c = outputLine %= (Seq.|> c)

-- | Go back in history, backing up the current line if modified.
historyPrev, historyNext :: MonadState Terminal m => m ()
historyPrev = use blocked >>= \b -> unless b do
  l <- use inputLine
  idx <- use inputHistoryIndex
  modified <- use lineChanged
  h <- use inputHistory
  case h Seq.!? (idx + 1) of
    -- no history to go back to, do nothing
    Nothing -> pure ()
    Just e -> do
      -- if modified, insert current line so we can go back to it
      -- (needed in case the key press was accidental)
      -- (alternative is replacing the history entry, which can be just as bad)
      when modified do
        inputHistory %= Seq.insertAt (idx + 1) (T.pack $ toList l)
        -- ensure it remains pointing at e
        inputHistoryIndex %= (+ 1)

      -- switch to next entry 
      inputHistoryIndex %= (+ 1)

      -- replace input with line from history
      inputLine .= Seq.fromList (T.unpack e)
      lineChanged .= False

      -- cursor may become invalid when we go backwards/forward in history.
      -- Best to just reset it to 0.
      cursorOffset .= 0
-- By duality...
historyNext = use blocked >>= \b -> unless b do
  l <- use inputLine
  idx <- use inputHistoryIndex
  modified <- use lineChanged
  h <- use inputHistory

  -- no need to correct the index this time since insertAt only offsets the right side
  when modified $ inputHistory %= Seq.insertAt (max 0 idx) (T.pack $ toList l)

  case h Seq.!? (idx - 1) of
    -- no history to go forward to, so just empty the line
    Nothing -> do
      inputLine .= mempty
      inputHistoryIndex .= (-1)
      cursorColumn .= 0
      lineChanged .= False
    Just e -> do
      -- switch to next entry 
      inputHistoryIndex %= subtract 1
      -- replace input with line from history
      inputLine .= Seq.fromList (T.unpack e)
      lineChanged .= False
      cursorOffset .= 0

-- | Delete the character before the cursor, if any.
deleteChar :: MonadState Terminal m => m ()
deleteChar = do
  b <- use blocked
  unless b do
    col <- use cursorColumn
    when (col > 0) do
      inputLine %= Seq.deleteAt (col - 1)
      cursorColumn %= max 0
      lineChanged .= True

cursorLeft, cursorRight :: MonadState Terminal m => m ()
-- | Move the cursor 1 column left, if possible.
cursorLeft = do
  b <- use blocked
  unless b $
    cursorColumn %= max 0 . subtract 1
-- | Move the cursor 1 column right, if possible.
cursorRight = do
  b <- use blocked
  unless b $
    cursorOffset %= max 0 . subtract 1

-- | Move the cursor to the specified column, clamped to [0, length inputLine].
cursorTo :: MonadState Terminal m => Int -> m ()
cursorTo col = use blocked >>= flip unless do
  l <- use inputLine
  cursorColumn .= clamp 0 (length l) col

-- | Move the cursor to the specified end offset, clamped to [0, length inputLine]
cursorToEnd :: MonadState Terminal m => Int -> m ()
cursorToEnd col = use blocked >>= flip unless do
  l <- use inputLine
  cursorOffset .= clamp 0 (length l) col

drawTerminal
  :: Ord n
  => n -- ^ Cursor name
  -> Terminal
  -> Widget n
drawTerminal cursorName t
  = makeCursor
  . vBox
  $ vBox hs
  : [ outputLineStr | not (null $ t ^. outputLine) ]
  ++ [ str finalLine ]
  where
    makeCursor =
      (if t ^. forceVisibleInputLine then visibleRegion cursorLoc (1,1) else id)
      . (if t ^. blocked then putCursor cursorName cursorLoc else showCursor cursorName cursorLoc)
    hs = map txt . toList $ Seq.reverse (t ^. history)
    outputLineStr = str $ toList (t ^. outputLine)
    finalLine = (t ^. prompt) ++ toList (t ^. inputLine)
    cursorLoc = Location (cursorX, cursorY)
    cursorX = length (t ^. prompt) + (t ^. cursorColumn)
    cursorY = length (t ^. history) + if null $ t ^. outputLine then 0 else 1

handleTerminalEvent :: MonadState Terminal m => BrickEvent n e -> m [Text]
handleTerminalEvent (VtyEvent (EvKey KUp _)) = [] <$ historyPrev
handleTerminalEvent (VtyEvent (EvKey KDown _)) = [] <$ historyNext
handleTerminalEvent (VtyEvent (EvKey KLeft _)) = [] <$ cursorLeft
handleTerminalEvent (VtyEvent (EvKey KRight _)) = [] <$ cursorRight
handleTerminalEvent (VtyEvent (EvKey (KChar '\t') _)) = pure [] -- TODO: tab completion?
handleTerminalEvent (VtyEvent (EvKey (KChar c) _)) = [] <$ insertChar c
handleTerminalEvent (VtyEvent (EvKey KDel _)) = [] <$ deleteChar
handleTerminalEvent (VtyEvent (EvKey KBS _)) = [] <$ deleteChar
handleTerminalEvent (VtyEvent (EvKey KHome _)) = [] <$ cursorTo 0
handleTerminalEvent (VtyEvent (EvKey KEnd _)) = [] <$ cursorToEnd 0
handleTerminalEvent (VtyEvent (EvKey KEnter _)) = maybeToList <$> enterLine
handleTerminalEvent (VtyEvent (EvPaste bs)) = do
  let bs' = BC.filter (/= '\t') bs
  insertLines bs'
handleTerminalEvent _ = pure []
