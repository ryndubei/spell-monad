{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
module App.UI.GameScreen.Terminal
  ( Terminal
  , drawTerminal
  , handleTerminalEvent
  , terminal

  -- * Lenses
  , prompt
  , blocked

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

-- | Expected to be used with a viewport.
data Terminal = Terminal
  { _history :: Seq Text -- ^ The visible terminal lines. Includes non-user output. Last output first. User inputs have 'prompt' prepended.
  , _inputHistory :: Seq Text -- ^ History for specifically inputs.
  , _inputHistoryIndex :: !Int -- ^ Index of the _current_ history entry. A completely fresh line
                               -- has index -1.
  , _lineChanged :: !Bool -- ^ If the line has been modified, it has to be recorded in history so that
                          -- we can return to it.
  , _inputLine :: Seq Char
  , _prompt :: String
  , _cursorOffset :: !Int -- ^ Offset from the end of inputLine, nonnegative.
  , _blocked :: !Bool
  }

terminal :: Terminal
terminal = Terminal
  { _history = mempty
  , _inputHistory = mempty
  , _inputHistoryIndex = -1
  , _lineChanged = False
  , _inputLine = mempty
  , _prompt = mempty
  , _cursorOffset = 0
  , _blocked = False
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
      let t = T.pack $ toList l
          tp = T.pack $ toList pr

      -- Add 't' to history with the prompt prepended
      history %= (tp <> t Seq.<|)

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

-- | Push a non-input string immediately to the history (will not be shown
-- when repeating previous commands with up arrow). Represents output.
pushOutput :: MonadState Terminal m => Text -> m ()
pushOutput t = history %= (t Seq.<|)

-- | Go back in history, backing up the current line if modified.
historyPrev, historyNext :: MonadState Terminal m => m ()
historyPrev = do
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
-- By duality...
historyNext = do
  l <- use inputLine
  idx <- use inputHistoryIndex
  modified <- use lineChanged
  h <- use inputHistory

  case h Seq.!? (idx - 1) of
    -- no history to go forward to, do nothing
    Nothing -> pure ()
    Just e -> do
      when modified $ inputHistory %= Seq.insertAt idx (T.pack $ toList l)
        -- no need to correct the index this time since insertAt only offsets the right side

      -- switch to next entry 
      inputHistoryIndex %= subtract 1
      -- replace input with line from history
      inputLine .= Seq.fromList (T.unpack e)
      lineChanged .= False

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
cursorTo col = do
  l <- use inputLine
  cursorColumn .= clamp 0 (length l) col

-- | Move the cursor to the specified end offset, clamped to [0, length inputLine]
cursorToEnd :: MonadState Terminal m => Int -> m ()
cursorToEnd col = do
  l <- use inputLine
  cursorOffset .= clamp 0 (length l) col

drawTerminal :: n -> Terminal -> Widget n
drawTerminal n t = showCursor n cursorLoc (vBox hs <=> str finalLine)
  where
    hs = map txt . toList $ Seq.reverse (t ^. history)
    finalLine = (t ^. prompt) ++ toList (t ^. inputLine)
    cursorLoc = Location (cursorX, cursorY)
    cursorX = length (t ^. prompt) + (t ^. cursorColumn)
    cursorY = length (t ^. history)

handleTerminalEvent :: MonadState Terminal m => BrickEvent n e -> m (Maybe Text)
handleTerminalEvent (VtyEvent (EvKey KUp _)) = Nothing <$ historyPrev
handleTerminalEvent (VtyEvent (EvKey KDown _)) = Nothing <$ historyNext
handleTerminalEvent (VtyEvent (EvKey KLeft _)) = Nothing <$ cursorLeft
handleTerminalEvent (VtyEvent (EvKey KRight _)) = Nothing <$ cursorRight
handleTerminalEvent (VtyEvent (EvKey (KChar '\t') _)) = pure Nothing -- TODO: tab completion?
handleTerminalEvent (VtyEvent (EvKey (KChar c) _)) = Nothing <$ insertChar c 
handleTerminalEvent (VtyEvent (EvKey KDel _)) = Nothing <$ deleteChar
handleTerminalEvent (VtyEvent (EvKey KBS _)) = Nothing <$ deleteChar
handleTerminalEvent (VtyEvent (EvKey KHome _)) = Nothing <$ cursorTo 0
handleTerminalEvent (VtyEvent (EvKey KEnd _)) = Nothing <$ cursorToEnd 0
handleTerminalEvent (VtyEvent (EvKey KEnter _)) = enterLine
handleTerminalEvent _ = pure Nothing
