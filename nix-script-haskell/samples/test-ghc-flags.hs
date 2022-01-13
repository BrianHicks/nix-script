#!/usr/bin/env nix-script-haskell
#!ghcFlags -threaded

import Control.Concurrent
import System.Exit

main :: IO ()
main = do
  if rtsSupportsBoundThreads
    then putStrLn "Success! Bound threads are supported"
    else do
      putStrLn "Failure! Bound threads are not supported"
      exitFailure
