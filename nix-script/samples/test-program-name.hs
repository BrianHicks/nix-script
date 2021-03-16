#!/usr/bin/env nix-script
#!buildInputs haskellPackages.ghc
#!build ghc -o $OUT_FILE $SCRIPT_FILE

import System.Environment
import System.Exit

main :: IO ()
main = do
  putStr "Checking that I have the right program name...\t"
  name <- getProgName
  case name of
    "test-program-name.hs" ->
      putStrLn "success!"
    anythingElse -> do
      putStrLn "failure!"
      putStrLn ("I expected `test-program-name.hs` but got " ++ show anythingElse)
      exitFailure
