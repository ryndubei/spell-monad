module Main (main) where

import Run (run)
import GHC.Stack

main :: HasCallStack => IO ()
main = run
