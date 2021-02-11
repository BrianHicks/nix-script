#!nix-script
#!buildInputs haskellPackages.ghc
#!build ghc -O -o $OUT_FILE $SCRIPT_FILE

import System.Environment (getArgs, getEnvironment)

main :: IO ()
main = do
  putStrLn "I got these arguments:"
  getArgs >>= print
  putStrLn ""
  putStrLn "And this environment:"
  getEnvironment >>= print
