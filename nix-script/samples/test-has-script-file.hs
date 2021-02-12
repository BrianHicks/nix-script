#!/usr/bin/env nix-script
#!buildInputs haskellPackages.ghc
#!build ghc -o $OUT_FILE $SCRIPT_FILE

import System.Environment
import System.Exit

main :: IO ()
main = do
  putStr "Checking that SCRIPT_FILE exists and is set correctly...\t"
  scriptPath <- lookupEnv "SCRIPT_FILE"
  case scriptPath of
    Just "samples/test-has-script-file.hs" ->
      putStrLn "success!"
    Just somethingElse -> do
      putStrLn "failure!"
      putStrLn ("I expected samples/test-has-script-file.hs but got " ++ somethingElse)
      exitFailure
    Nothing -> do
      putStrLn "failure!"
      putStrLn "SCRIPT_FILE was not set at all!"
