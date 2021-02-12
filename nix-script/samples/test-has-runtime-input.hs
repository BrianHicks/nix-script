#!/usr/bin/env nix-script
#!buildInputs haskellPackages.ghc
#!build ghc -o $OUT_FILE $SCRIPT_FILE
#!runtimeInputs jq

import System.Process

main :: IO ()
main = do
  putStr "Checking that I get my runtime inputs...\t"
  _ <- readProcess "jq" ["."] "{}"
  putStrLn "success!"
