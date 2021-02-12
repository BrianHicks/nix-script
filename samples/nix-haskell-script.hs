#!nix-haskell-script
#!haskellInputs text

{-# LANGUAGE OverloadedStrings #-}

import Data.Text.IO

main :: IO ()
main = Data.Text.IO.putStrLn "Hello, World!"
