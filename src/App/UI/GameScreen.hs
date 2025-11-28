{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module App.UI.GameScreen (withGameUI, GameExit(..), AppUserInput(..)) where

import Control.Monad.IO.Class
import App.Thread
import Simulation
import Input
import Brick
import Control.Lens
import Graphics.Vty (Vty (outputIface), displayBounds)
import Graphics.Vty.Attributes
import qualified Control.Lens as L
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Concurrent.STM
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Graphics.Vty.Image
import Data.Foldable
import Control.Parallel.Strategies (parMap, rdeepseq)
import App.UI.GameScreen.Terminal
import Graphics.Vty.Input
import Control.Monad
import Control.Concurrent.Async
import Control.Monad.State.Strict
import Data.Char
import Control.Applicative
import Data.Maybe
import Brick.Widgets.ProgressBar (progressBar, progressCompleteAttr, progressIncompleteAttr)
import Control.Monad.Reader
import Data.Time
import Numeric (showGFloat)

data Name
  = TerminalCursor
  | TerminalInputLine
  | TerminalViewport
  | GameWindow
  | LogViewport
  deriving (Eq, Ord, Show)

data AppState = AppState
  { _gameExit :: !(Maybe GameExit)
  , _simState :: !SimState
  , _windowSize :: DisplayRegion
  , _terminalFocus :: TerminalFocus
  , _term :: Terminal
  , _displayFPS :: Double -- ^ In CPU time. (Why Double? Because it's closed)
  , _lastFPSTimestamp :: UTCTime
  }

data AppUserInput = GameInput UserInput | TermStdin Char

data TerminalFocus = Invisible | VisibleUnfocused | VisibleFocused deriving Eq

data GameExit = ExitDesktop | ExitMainMenu

makeLenses ''AppState

withGameUI :: AppThread -> ReplThread -> UTCTime -> SimState -> (BrickThread (Maybe GameExit) (Either SimState [SimEvent]) AppUserInput -> IO a) -> IO a
withGameUI th rth t0 ss0 k = do
  v <- appThreadVty th
  _windowSize <- displayBounds (outputIface v)
  let s0 = AppState
        { _gameExit = Nothing
        , _simState = ss0
        , _windowSize
        , _terminalFocus = VisibleUnfocused
        , _term = (prompt .~ "Spell> ") terminal
        , _displayFPS = 0
        , _lastFPSTimestamp = t0
        }
  withBrickThread th (theapp rth) s0 \bth -> do
    withAsync
      do
        lastStatus <- atomically $ replStatus rth >>= newTVar
        -- dupBrickThreadIn prevents competition with threads sending to bth
        bth' <- lmap Left <$> atomically (dupBrickThreadIn bth)
        forever $ atomically do
          isBrickQueueEmpty bth' >>= check
          statusChanged <- isJust <$> optional do
            status1 <- readTVar lastStatus
            status <- replStatus rth
            check (not $ status `sameStatus` status1)
            writeTVar lastStatus status
            sendBrickEvent bth' ReplStatusChange
          receivedOutput <- isJust <$> optional do
            cs <- some (getReplResult rth >>= maybe retry pure)
            sendBrickEvent bth' (ReplOutput cs)
          check (statusChanged || receivedOutput)
      \rthMonitor -> do
        link rthMonitor
        k . mapBrickResult (^. gameExit) $ lmap Right bth

data ReplEvent = ReplStatusChange | ReplOutput String

updateFPS :: EventM Name AppState ()
updateFPS = do
  t <- use lastFPSTimestamp
  t' <- liftIO getCurrentTime
  lastFPSTimestamp .= t'
  let dt = realToFrac $ nominalDiffTimeToSeconds (t' `diffUTCTime` t)
      fps = 1 / dt
  displayFPS .= fps

theapp :: ReplThread -> TChan AppUserInput -> App AppState (Either ReplEvent (Either SimState [SimEvent])) Name
theapp rth c = App {..}
  where
    appDraw s =
      [
        if s ^. terminalFocus == Invisible
          then emptyWidget
          else vLimit 10
             . hCenterLayer
             . border
             . withVScrollBars OnRight
             . clickable TerminalViewport
             . viewport TerminalViewport Vertical
             $ drawTerminal TerminalCursor (s ^. term)
      , clickable GameWindow (gameWindow s <=> hBorder <=> vLimit 2 (statusBars s <+> vBorder <+> fpsInfo s))
      ]

    statusBars s =
      let SimState{playerMana, playerMaxMana} = s ^. simState
          manaBar = (clamp 0 1 $ realToFrac playerMana / realToFrac playerMaxMana)
       in hLimitPercent 30
        $ progressBar (Just "Side effects") manaBar

    fpsInfo s =
      let fpsString = showGFloat (Just 1) (s ^. displayFPS) ""
       in padLeft Max $ str "display fps: " <+> padLeft (Pad . max 0 $ 10 - textWidth fpsString) (str fpsString) 

    gameWindow s = drawSimState (s ^. simState)

    appAttrMap _ = attrMap defAttr
      [ (attrName "Player", withForeColor defAttr cyan)
      , (attrName "Firebolt", withForeColor defAttr brightYellow)
      , (attrName "TargetSelector", withForeColor defAttr red)
      , (progressCompleteAttr, withBackColor (withForeColor defAttr black) cyan)
      , (progressIncompleteAttr, withBackColor (withForeColor defAttr white) brightBlack)
      ]

    -- TODO: replace with dialogue-based exit
    appHandleEvent (VtyEvent (EvKey (KChar 'q') _)) = do
      L.assign gameExit (Just ExitMainMenu)
      halt

    appHandleEvent (VtyEvent (EvKey (KChar 't') ms)) | MCtrl `elem` ms = do
      fc <- use terminalFocus
      Brick.zoom term $ forceVisibleInputLine .= True
      terminalFocus .= case fc of
        VisibleFocused -> Invisible
        _ -> VisibleFocused
    appHandleEvent (VtyEvent (EvKey (KChar 'w') ms)) | MCtrl `elem` ms = do
      fc <- use terminalFocus
      case fc of
        VisibleFocused -> terminalFocus .= VisibleUnfocused
        VisibleUnfocused -> do
          Brick.zoom term $ forceVisibleInputLine .= True
          terminalFocus .= VisibleFocused
        Invisible -> terminalFocus .= Invisible

    appHandleEvent (VtyEvent (EvKey KPageDown _)) = do
      fc <- use terminalFocus
      case fc of
        VisibleFocused -> do
          vScrollBy (viewportScroll TerminalViewport) 1
        _ -> pure ()
    appHandleEvent (VtyEvent (EvKey KPageUp _)) = do
      fc <- use terminalFocus
      case fc of
        VisibleFocused -> do
          Brick.zoom term $ forceVisibleInputLine .= False
          vScrollBy (viewportScroll TerminalViewport) (-1)
        _ -> pure ()

    appHandleEvent (AppEvent (Right (Left ss))) = do
      updateFPS
      L.assign simState ss
    appHandleEvent (AppEvent (Right (Right se))) = do
      let se' = mconcat se
       in case se' of
        SimEvent { spellOutput } -> do
          Brick.zoom term (traverse_ pushOutput spellOutput)
    appHandleEvent (AppEvent (Left ReplStatusChange)) =
      get >>= liftIO . atomically . handleReplStatusChange rth >>= put
    appHandleEvent (AppEvent (Left (ReplOutput cs))) =
      Brick.zoom term $ traverse_ pushOutput cs

    appHandleEvent (VtyEvent (EvResize w h)) =
      windowSize .= (w,h)

    appHandleEvent (MouseDown TerminalViewport BScrollUp _ _) = do
      Brick.zoom term $ forceVisibleInputLine .= False
      vScrollBy (viewportScroll TerminalViewport) (-1)
    appHandleEvent (MouseDown TerminalViewport BScrollDown _ _) = do
      vScrollBy (viewportScroll TerminalViewport) 1
    appHandleEvent (MouseDown TerminalViewport BLeft _ _) = do
      Brick.zoom term $ forceVisibleInputLine .= True
      terminalFocus .= VisibleFocused
    appHandleEvent (MouseDown GameWindow BLeft _ _) = do
      fc <- use terminalFocus
      when (fc == VisibleFocused) $
        terminalFocus .= VisibleUnfocused

    appHandleEvent e = handleOutgoingEvent rth c e

    appChooseCursor s =
      if s ^. terminalFocus == VisibleFocused
        then showCursorNamed TerminalCursor
        else pure Nothing

    appStartEvent = do
      s <- get
      s' <- liftIO . atomically $ handleReplStatusChange rth s
      put s'
      makeVisible TerminalCursor

handleReplStatusChange :: ReplThread -> AppState -> STM AppState
handleReplStatusChange rth s = do
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

-- | Events that relay anything to either the ReplThread or the TChan are handled here.
--
-- Assumed: this is the only function handling the given event, so we are free to call
-- continueWithoutRedraw where appropriate.
handleOutgoingEvent :: ReplThread -> TChan AppUserInput -> BrickEvent n a -> EventM n AppState ()
handleOutgoingEvent rth tc = \e -> do
  s <- get
  s' <- go e s
  put s'
  where
    go (VtyEvent (EvKey (KChar 'c') ms)) s@((^. terminalFocus) -> VisibleFocused) | MCtrl `elem` ms = do
      interrupted <- liftIO $ atomically do
        b <- replStatus rth
        case b of
          Blocked -> do
            interruptRepl rth
            pure True
          _ -> pure False
      if interrupted
        then do
          -- running interruptRepl guarantees an immediate unblock for submitRepl
          pure $ flip execState s do
            Brick.zoom term unblockTerm
            Brick.zoom term $ pushOutputLine "Interrupted."
        else pure s

    go e s@((^. terminalFocus) -> VisibleFocused) = do
      s' <- liftIO $ atomically do
        s' <- handleReplStatusChange rth s
        let (t, s'')
              = flip runState s'
              . fmap T.unlines
              . Brick.zoom term
              $ handleTerminalEvent e
        -- since we've handled the repl status change, term will be blocked iff
        -- the REPL is blocked, so submitRepl will not block
        if T.null (T.filter (not . isSpace) t)
          then pure s''
          else do
            submitRepl rth $ T.unpack t
            pure $ execState (Brick.zoom term blockTerm) s''

      -- send entered keys to 'stdin' for the GetChar effect
      case e of
        VtyEvent (EvKey (KChar c) _) -> liftIO . atomically $ writeTChan tc (TermStdin c)
        _ -> pure ()

      case e of
        -- Force visible input line if it's a key
        -- TODO: ugly, should instead have Terminal.hs return if the input event
        -- recognised and responded to or not, then scroll to input according to
        -- that
        VtyEvent (EvKey _ _) -> pure $ execState (Brick.zoom term $ forceVisibleInputLine .= True) s'
        _ -> pure s'

    go (VtyEvent (EvKey k m)) s = do
      traverse_ (liftIO . atomically . writeTChan tc . GameInput) (directInput k m)
      -- We assume there is no event handling other than this, hence no visible
      -- effects.
      continueWithoutRedraw
      pure s

    go _ s = do
      continueWithoutRedraw
      pure s

unblockedPrompt :: String
unblockedPrompt = "spell> "

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

drawSimState :: SimState -> Widget Name
drawSimState SimState{..} =
  Widget
    { hSize = Greedy
    , vSize = Greedy
    , render = do
        ctx <- ask
        let height = ctx ^. availHeightL
            width = ctx ^. availWidthL
            attrmap = ctx ^. ctxAttrMapL
        pure $ emptyResult { image = simStateImage attrmap width height }
    }
  where
    simStateImage attrmap width height =
      let
        ys :: Vector Double
        ys = V.map ((+ cameraY) . negate . fromIntegral) $ V.enumFromN (- (height `div` 2)) height

        xs :: V.Vector Double
        xs = V.map ((+ cameraX) . (/ 2) . fromIntegral) $ V.enumFromN (- (width `div` 2)) width

        dists :: V.Vector (V.Vector (ObjectIdentifier, Double))
        dists = flip V.map ys \y -> flip V.map xs \x -> sdf (x,y)

        -- ObjectIdentifiers together with the string lengths to draw
        lineSliceLengths :: [V.Vector (Maybe ObjectIdentifier, Int)]
        lineSliceLengths = flip (parMap rdeepseq) (V.toList dists) $ V.unfoldrN width \row ->
          let u = V.uncons row
          in case u of
            Just (hrow, trow) ->
              let toDraw (oid, d) = if d < 0.5 then Just oid else Nothing
                  (lrow, rrow) = V.span (\t -> toDraw t == toDraw hrow) trow
               in Just ((toDraw hrow, 1 + length lrow), rrow)
            Nothing -> Nothing

        lineSlice :: Maybe ObjectIdentifier -> Int -> Image
        -- cannot simply do "translateX len mempty". I don't know why.
        lineSlice Nothing len = translateX len $ text' defAttr mempty
        lineSlice (Just oid) len = text' (attrMapLookup (objectIdToAttr oid) attrmap) $ T.replicate len "█"

        displayedSimState :: Image
        displayedSimState = foldr' vertJoin mempty $ map (V.foldr' horizJoin mempty . V.map (uncurry lineSlice)) lineSliceLengths

       in displayedSimState

objectIdToAttr :: ObjectIdentifier -> AttrName
objectIdToAttr Player = attrName "Player"
objectIdToAttr Firebolt = attrName "Firebolt"
objectIdToAttr TargetSelector = attrName "TargetSelector"

directInput :: Key -> [Modifier] -> Maybe UserInput
directInput KUp _ = Just $ mempty { moveY = 1 }
-- normalising to magnitude 1 is the responsibility of the game
directInput KUpRight _ = Just mempty { moveX = 1, moveY = 1}
directInput KRight _ = Just mempty { moveX = 1 }
directInput KDownRight _ = Just mempty { moveX = 1, moveY = -1}
directInput KDown _ = Just mempty { moveY = -1 }
directInput KDownLeft _ = Just mempty { moveX = -1, moveY = -1}
directInput KLeft _ = Just mempty { moveX = -1 }
directInput KUpLeft _ = Just mempty { moveX = -1, moveY = 1}
directInput (KChar ' ') _ = Just mempty { jump = True }
directInput KEnter _ = Just mempty { enter = True }
directInput _ _ = Nothing
