#!/usr/bin/env nix-script-haskell
#!haskellPackages relude

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Relude

main :: IO ()
main = putTextLn "Hello, World!"
