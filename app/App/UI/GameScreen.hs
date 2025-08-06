{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeAbstractions #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
module App.UI.GameScreen (newGameUI, GameExit(..)) where

import Control.Monad.IO.Class
import App.Thread
import FRP.Rhine
import Simulation
import Input
import Data.Void
import UnliftIO
import Brick
import Control.Monad.Schedule.Class (MonadSchedule)
import Control.Monad.Trans.Resource
import Data.Maybe
import Control.Lens (makeLenses, _1, _2, _Left, _Right)
import Brick.Widgets.Dialog
import App.UI.GameScreen.Haskeline
import System.Posix (ProcessStatus)
import Data.Profunctor
import Data.Sequence (Seq)
import System.Terminal.Emulator.Term.Process
import System.Terminal.Emulator.KeyboardInput
import qualified Data.Sequence as Seq
import Data.Foldable (traverse_, Foldable (toList))
import qualified Data.Vector.Unboxed as VU
import Control.Lens.Operators
import Graphics.Vty (defAttr, Event (..), Key (..), Modifier (..))
import Control.Monad.Trans.Class
import Data.Char
import Control.Applicative
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (hCenterLayer)
import LogClock
import Data.Text (Text)
import qualified Data.Text as T

data Name
  = ExitDialogButtonYes
  | ExitDialogButtonNo
  | GameViewport
  | TerminalViewport
  deriving (Eq, Ord, Show)

data ConsoleFocus = ConsoleInvisible | ConsoleVisibleUnfocused | ConsoleFocused deriving (Eq, Show)

data AppState = AppState
  { _gameExit :: Maybe GameExit
  , _consoleFocus :: ConsoleFocus
  , _consoleDisplaySize :: (Int, Int) -- ^ (w,h)
  , _exitDialog :: Maybe (Dialog () Name)
  , _consoleLines :: Seq TermLine
  , _consoleScroll :: Maybe Int
  }

data GameExit = ExitDesktop | ExitMainMenu | Crash String

makeLenses ''AppState

neverIn
  :: (MonadIO m, Time (In cl) ~ UTCTime, Time cl ~ UTCTime, Clock m cl, Clock m (Out cl), Clock m (In cl), GetClockProxy cl)
  => Rhine m cl () b
  -> Rhine m (Never `SeqClock` cl) a b
neverIn rh = (tagS >>> arr absurd) @@ Never >-- trivialResamplingBuffer --> rh

neverOut
  :: (MonadIO m, Time (Out cl) ~ UTCTime, Time cl ~ UTCTime, Clock m cl, Clock m (Out cl), Clock m (In cl), GetClockProxy cl)
  => Rhine m cl a ()
  -> Rhine m (cl `SeqClock` Never) a b
neverOut rh = rh >-- trivialResamplingBuffer --> (tagS >>> arr absurd) @@ Never

inClSF :: MonadIO m => BrickThread s SimState st -> ClSF m cl SimState ()
-- Despite the `atomically`, presume this to be non-blocking
-- If this takes enough time to not be non-blocking, we have more pressing
-- things to worry about than clock accuracy
inClSF bth = arrMCl (atomically . sendBrickEvent bth)

displayRhine :: forall m s. (MonadIO m, MonadLog (LogMessage Text) m) => BrickThread s SimState AppState -> Rhine m _ SimState UserInput
displayRhine bth = neverOut $ inClSF bth @@ logClockDebug @m "DisplayClock-displayRhine" (DisplayClock bth) (const mempty)

userLineRhine :: forall m t a. (MonadIO m, MonadLog (LogMessage Text) m) => HaskelineThread t -> Rhine m (Never `SeqClock` _) a UserInput
userLineRhine hth = neverIn $ (tagS >>> arr ReplLine) @@ logClockDebug @m "HaskelineThreadLineClock" (HaskelineThreadLineClock hth) id

data HaskelineInEvent
  = KP !KeyPress
  | RS !Int !Int
  | RR !ReplResult

newGameUI
  :: forall s m t. (MonadResource m, MonadSchedule m, MonadUnliftIO m, MonadLog (LogMessage Text) m)
  => AppThread s
  -> HaskelineThread t
  -> m (ReleaseKey, Rhine (ExceptT GameExit m) _ (SimState, Maybe ReplResult) UserInput)
newGameUI th hth = do
  -- Output event channel to pass to the app
  -- Unbounded memory usage, but it's expected: we don't want to lose any
  -- terminal user input
  (appC :: Chan (Either UserInput HaskelineInEvent)) <- UnliftIO.newChan

  (rk1, bth) <- newBrickThread th (theapp appC) s0

  let hClock = SelectClock (EventClock @(Either UserInput HaskelineInEvent)) (^? _Right)
      uClock :: HoistClock m (ExceptT GameExit m) (SelectClock (HoistClock _ m _) _)
      uClock = HoistClock (SelectClock (eventClockOn appC) (^? _Left)) lift
      dc :: forall m'. MonadLog (LogMessage Text) m' => LogClockH m' _
      dc = logClockDebug "DisplayClock-hRh" (DisplayClock bth) (const mempty)

  -- TODO: interact with HaskelineThread inside App directly without a Rhine
  let hRh :: forall m'. (MonadLog (LogMessage Text) m', MonadIO m', MonadSchedule m') => Rhine (EventChanT (Either UserInput HaskelineInEvent) m') _ () ()
      hRh = (tagS >>> arr \case
        Left v -> absurd v
        Right (KP kp) -> (Nothing, Just kp, Nothing)
        Right (RS w h) -> (Nothing, Nothing, Just (w,h))
        Right (RR rr) -> (Just rr, Nothing, Nothing)
        -- TODO: own EventClock type that uses TBChan and is covariant
        ) ^->@ haskelineRhine hth hClock size0 >-- keepLast Seq.empty -->
        -- ensure bounded memory usage for BrickThread's queue by only writing terminal updates on DisplayClock ticks
        arrM (UnliftIO.atomically . sendBrickEvent bth . Left) @@ dc @(EventChanT (Either UserInput HaskelineInEvent) m')

  -- Thread for managing HaskelineThread
  (rk2, _) <- withRunInIO $ \z -> z $ allocate
    (async $ runLoggingT (withChan appC $ flow_ hRh) (liftIO . z . logMessage))
    uninterruptibleCancel

  -- combine rk1 and rk2
  (rk, _) <- allocate
    do
      f1 <- unprotect rk1
      f2 <- unprotect rk2
      pure (f1, f2)
    \(f1, f2) -> do
      sequence_ f2
      sequence_ f1

  let -- cl1 and cl2 make the Rhine throw GameExit when appropriate
      cl1 = HoistClock (BrickExitClock bth) $ withExceptT @m
        (either
          (Crash . displayException @SomeException)
          (fromMaybe (Crash "No reason given for game exit") . _gameExit))
      cl2 = HoistClock (HaskelineThreadExitClock hth) $ withExceptT @m \(ps :: Maybe ProcessStatus) ->
        Crash ("Haskeline process failed with " <> show ps)
      rh1 = (tagS @@ cl1) @>>^ absurd
      rh2 = (tagS @@ cl2) @>>^ absurd
      exitRhine = rh1 |@| rh2
      rh =
        -- consumer of input to the returned Rhine
        (arrM (\(ss, mrr) -> traverse_ (UnliftIO.writeChan appC . Right . RR) mrr >> pure ss) ^->@ displayRhine (lmap Right bth))
              -- line inputs in the console
          |@| (userLineRhine hth
                -- no output, just throws a GameExit once possible
            |@| (exitRhine
                  -- direct user input
              |@| neverIn (tagS @@ logClockDebug @(ExceptT GameExit m) "user input clock" uClock (T.pack . show))))
  pure (rk, rh)
  where
    size0 = (64, 16)
    s0 = AppState
      { _gameExit = Nothing
      , _consoleFocus = ConsoleFocused
      , _exitDialog = Nothing
      , _consoleDisplaySize = size0
      , _consoleLines = mempty
      , _consoleScroll = Nothing
      }


theapp :: Chan (Either UserInput HaskelineInEvent) -> App AppState (Either (Seq TermLine) SimState) Name
theapp c = App {..}
  where
    appDraw s = [ if s ^. consoleFocus == ConsoleInvisible then emptyWidget else consoleWindow s
                , maybe (gameWindow s) (`renderDialog` gameWindow s) (s ^. exitDialog)
                ]

    gameWindow _ = viewport GameViewport Both (vLimit 100 $ hLimit 100 $ fill 'A')
    consoleWindow s
      = hCenterLayer
      . border
      . vLimit (s ^. consoleDisplaySize . _2)
      . hLimit (s ^. consoleDisplaySize . _1 + 1 )
      . withVScrollBars OnRight
      . viewport TerminalViewport Vertical
      . vBox
      . zipWith (\i l ->
          (if i == fromMaybe (length (_consoleLines s)) (_consoleScroll s)
            then visible
            else id) $ drawLine l) [1..]
      . toList
      $ _consoleLines s

    appAttrMap _ = attrMap defAttr []

    appHandleEvent (VtyEvent (EvKey KEsc _)) =
      liftIO . writeChan c $ Left TogglePause

    -- toggle console visibility
    appHandleEvent (VtyEvent (EvKey (KChar 't') [MCtrl])) =
      consoleFocus %= \case
        ConsoleInvisible -> ConsoleFocused
        ConsoleFocused -> ConsoleInvisible
        ConsoleVisibleUnfocused -> ConsoleInvisible

    -- toggle console focus without affecting visibility
    appHandleEvent (VtyEvent (EvKey (KChar 'w') [MCtrl])) =
      consoleFocus %= \case
        ConsoleInvisible -> ConsoleInvisible
        ConsoleFocused -> ConsoleVisibleUnfocused
        ConsoleVisibleUnfocused -> ConsoleFocused

    appHandleEvent (VtyEvent (EvKey k ms)) = do
      s <- Brick.get
      liftIO . traverse_ (writeChan c) $ case s ^. consoleFocus of
        ConsoleFocused ->
          (if isInterrupt k ms then Just (Left Interrupt) else Nothing)
          <|>
          (Right . KP <$> consoleInput k ms)
        ConsoleInvisible -> Left <$> directInput k ms
        ConsoleVisibleUnfocused -> Left <$> directInput k ms

    appHandleEvent _ = pure ()

    appChooseCursor = neverShowCursor

    appStartEvent = pure ()

drawLine :: TermLine -> Widget Name
-- TODO: handle attrs
drawLine = str . map fst . VU.toList

consoleInput :: Key -> [Modifier] -> Maybe KeyPress
consoleInput k m = case k of
  KEsc -> Just $ KeyPress_SpecialKey SpecialKey_Escape m'
  KFun 1 -> Just $ KeyPress_SpecialKey SpecialKey_F1 m'
  KFun 2 -> Just $ KeyPress_SpecialKey SpecialKey_F2 m'
  KFun 3 -> Just $ KeyPress_SpecialKey SpecialKey_F3 m'
  KFun 4 -> Just $ KeyPress_SpecialKey SpecialKey_F4 m'
  KFun 5 -> Just $ KeyPress_SpecialKey SpecialKey_F5 m'
  KFun 6 -> Just $ KeyPress_SpecialKey SpecialKey_F6 m'
  KFun 7 -> Just $ KeyPress_SpecialKey SpecialKey_F7 m'
  KFun 8 -> Just $ KeyPress_SpecialKey SpecialKey_F8 m'
  KFun 9 -> Just $ KeyPress_SpecialKey SpecialKey_F9 m'
  KFun 10 -> Just $ KeyPress_SpecialKey SpecialKey_F10 m'
  KFun 11 -> Just $ KeyPress_SpecialKey SpecialKey_F11 m'
  KFun 12 -> Just $ KeyPress_SpecialKey SpecialKey_F12 m'
  KFun _ -> Nothing
  KUp -> Just $ KeyPress_SpecialKey SpecialKey_ArrowUp m'
  KDown -> Just $ KeyPress_SpecialKey SpecialKey_ArrowDown m'
  KLeft -> Just $ KeyPress_SpecialKey SpecialKey_ArrowLeft m'
  KRight -> Just $ KeyPress_SpecialKey SpecialKey_ArrowRight m'
  KHome -> Just $ KeyPress_SpecialKey SpecialKey_Home m'
  KEnd -> Just $ KeyPress_SpecialKey SpecialKey_End m'
  KBackTab -> Just $ KeyPress_SpecialKey SpecialKey_Tab m' -- Shift assumed to be in modifiers
  KEnter -> Just $ KeyPress_SpecialKey SpecialKey_Enter m'
  KBS -> Just $ KeyPress_SpecialKey SpecialKey_Backspace m'
  KDel -> Just $ KeyPress_SpecialKey SpecialKey_Delete m'
  KIns -> Just $ KeyPress_SpecialKey SpecialKey_Insert m'
  KPageUp -> Just $ KeyPress_SpecialKey SpecialKey_PageUp m'
  KPageDown -> Just $ KeyPress_SpecialKey SpecialKey_PageDown m'
  KPrtScr -> Nothing
  KUpLeft -> Just $ KeyPress_SpecialKey SpecialKey_ArrowUp m'
  KUpRight -> Just $ KeyPress_SpecialKey SpecialKey_ArrowUp m'
  KDownLeft -> Just $ KeyPress_SpecialKey SpecialKey_ArrowDown m'
  KDownRight -> Just $ KeyPress_SpecialKey SpecialKey_ArrowDown m'
  KCenter -> Nothing
  KPause -> Nothing
  KBegin -> Nothing
  KMenu -> Nothing
  KChar c ->
    if isPrint c
      then Just $ KeyPress_Char c m'
      else Nothing
  where
    m' = KeyModifiers
      { shift = MShift `elem` m
      , ctrl = MCtrl `elem` m
      , alt = MAlt `elem` m
      , capsLock = False
      }

isInterrupt :: Key -> [Modifier] -> Bool
isInterrupt k m = MCtrl `elem` m && k == KChar 'c'

directInput :: Key -> [Modifier] -> Maybe UserInput
directInput KUp m = Just $ Move (MShift `elem` m) (fromRadians 0)
directInput KUpRight m = Just $ Move (MShift `elem` m) (fromRadians (pi / 4))
directInput KRight m = Just $ Move (MShift `elem` m) (fromRadians (pi / 2))
directInput KDownRight m = Just $ Move (MShift `elem` m) (fromRadians (3 * pi / 4))
directInput KDown m = Just $ Move (MShift `elem` m) (fromRadians pi)
directInput KDownLeft m = Just $ Move (MShift `elem` m) (fromRadians (5 * pi / 4))
directInput KLeft m = Just $ Move (MShift `elem` m) (fromRadians (3 * pi / 2))
directInput KUpLeft m = Just $ Move (MShift `elem` m) (fromRadians (7 * pi / 4))
directInput (KChar ' ') _ = Just Jump
directInput _ _ = Nothing
