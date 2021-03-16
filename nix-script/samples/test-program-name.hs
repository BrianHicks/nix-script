#!/usr/bin/env nix-script
#!buildInputs haskellPackages.ghc
#!build ghc -o $OUT_FILE $SCRIPT_FILE

import System.Environment

main :: IO ()
main = putStrLn =<< getProgName
