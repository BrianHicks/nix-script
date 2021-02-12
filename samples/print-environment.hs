#!nix-script
#!buildInputs haskellPackages.ghc
#!build ghc -O -o $OUT_FILE $SCRIPT_FILE

import System.Environment (getArgs, lookupEnv)

main :: IO ()
main = do
  putStrLn "I got these arguments:"
  getArgs >>= print
  putStrLn ""
  putStrLn "And this environment:"
  lookupEnv "SCRIPT_FILE" >>= print
