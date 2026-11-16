{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module App.UI.LoadingScreen (withLoadingScreen) where

import App.Thread.UI
import Data.Void

withLoadingScreen :: (UIThread () Void Void -> IO r) -> IO r
withLoadingScreen k = do
  showLoadingScreen
  withUIThread (\_ _ -> pure ()) k

#if defined(wasi_HOST_OS)
foreign import javascript "__GAME_STAGE.showLoadingScreen()"
  showLoadingScreen :: IO ()
#else
showLoadingScreen :: IO ()
showLoadingScreen = error "no JSFFI"
#endif
