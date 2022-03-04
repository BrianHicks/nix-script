#!/usr/bin/env nix-script
#!build $SRC $SRC.hs; ghc -o $OUT $SRC.hs
#!buildInputs (haskellPackages.ghcWithPackages (ps: [ ps.text ]))

{-# LANGUAGE OverloadedStrings #-}

import Data.Text.IO as TextIO

main :: IO ()
main =
  TextIO.putStrLn "Hello, World!"
