#!/usr/bin/env nix-script-haskell
#!ghcFlags -XNumericUnderscores

main :: IO ()
main = do
  putStrLn $ "This is a number: " ++ show 123_456
