import qualified Data.Text as Text
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.Process as Process

main :: IO ()
main = do
  args <- Environment.getArgs
  case args of
    [] ->
      callNixScript [] []
    "--ghcid" : target : _ -> do
      Environment.setEnv "RUNTIME_INPUTS" "haskellPackages.ghcid"
      Environment.setEnv "SHELL_RUN" ("ghcid " ++ target)
      haskellPackages <- getHaskellPackages target
      callNixScript haskellPackages ["--shell", target]
    "--shell" : target : args -> do
      haskellPackages <- getHaskellPackages target
      callNixScript haskellPackages ("--shell" : target : args)
    target : args -> do
      haskellPackages <- getHaskellPackages target
      callNixScript haskellPackages (target : args)

callNixScript :: [Text] -> [String] -> IO ()
callNixScript haskellPackages args = do
  Environment.setEnv
    "BUILD_INPUTS"
    ( "(haskellPackages.ghcWithPackages (ps: with ps; ["
        ++ toString (Text.intercalate " " haskellPackages)
        ++ "]))"
    )
  -- We have to add a `.hs` extension if it isn't present because otherwise
  -- Haskell thinks we're trying to compile a named module instead of a file
  -- in cases where the script file does not have an extension.
  Environment.setEnv "BUILD_COMMAND" "mv $SCRIPT_FILE $SCRIPT_FILE.hs && ghc -O -o $OUT_FILE $SCRIPT_FILE.hs"
  Process.spawnProcess "nix-script" args
    >>= Process.waitForProcess
    >>= Exit.exitWith

getHaskellPackages :: FilePath -> IO [Text]
getHaskellPackages target = do
  sourceLines <- lines <$> readFileText target
  pure $ mapMaybe (Text.stripPrefix "#!haskellPackages ") sourceLines
