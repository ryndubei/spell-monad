{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module App.UI.GameScreen (withGameScreen) where

import App.Thread.UI
import Input
import Simulation
import App.Thread.Repl

withGameScreen :: ReplThread -> (UIThread () (Either SimState SimEvent) UserInput -> IO r) -> IO r
withGameScreen _ k = do
  showGameScreen
  withUIThread (\_ _ -> pure ()) k

#if defined(wasi_HOST_OS)
foreign import javascript "__GAME_STAGE.showGameScreen()"
  showGameScreen :: IO ()
#else
noJsffi :: a
noJsffi = error "no JSFFI"

showGameScreen :: IO ()
showGameScreen = noJsffi
#endif
