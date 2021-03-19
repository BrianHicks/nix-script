#!/usr/bin/env nix-script
#!buildInputs haskellPackages.ghc
#!build ghc -o $OUT_FILE $SCRIPT_FILE

import System.Environment
import System.Exit

main :: IO ()
main = do
  putStr "Checking that I'm being passed flags...\t"
  args <- getArgs
  case args of
    ["--help"] ->
      putStrLn "success!"
    anythingElse -> do
      putStrLn "failure!"
      putStrLn ("I expected `[\"--help\"]` but got " ++ show anythingElse)
      exitFailure
