import qualified Data.Text as Text
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.Process as Process

main :: IO ()
main = do
  args <- Environment.getArgs
  case args of
    [] ->
      callNixScript []
    "--shell" : target : args -> do
      setBuildInputsFromHaskellPackages target
      callNixScript ("--shell" : target : args)
    target : args -> do
      setBuildInputsFromHaskellPackages target
      callNixScript (target : args)

callNixScript :: [String] -> IO ()
callNixScript args = do
  Environment.setEnv "BUILD_COMMAND" "ghc -O -o $OUT_FILE $SCRIPT_FILE"
  Process.spawnProcess "nix-script" args
    >>= Process.waitForProcess
    >>= Exit.exitWith

setBuildInputsFromHaskellPackages :: FilePath -> IO ()
setBuildInputsFromHaskellPackages target = do
  sourceLines <- lines <$> readFileText target
  case mapMaybe (Text.stripPrefix "#!haskellPackages ") sourceLines of
    [] -> pure ()
    deps ->
      Environment.setEnv
        "BUILD_INPUTS"
        ( "(haskellPackages.ghcWithPackages (ps: with ps; ["
            ++ (Text.unpack (Text.intercalate " " deps))
            ++ "]))"
        )
