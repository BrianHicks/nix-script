#!/usr/bin/env nix-script
#!buildInputs haskellPackages.ghc
#!build ghc -o $OUT_FILE $SCRIPT_FILE

import System.Environment
import System.Exit

main :: IO ()
main = do
  putStr "Checking that I'm being passed arguments...\t"
  args <- getArgs
  case args of
    ["a", "b", "c"] ->
      putStrLn "success!"
    anythingElse -> do
      putStrLn "failure!"
      putStrLn ("I expected `[\"a\",\"b\",\"c\"]` but got " ++ show anythingElse)
      exitFailure
