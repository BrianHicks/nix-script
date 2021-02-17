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
        ++ (Text.unpack (Text.intercalate " " haskellPackages))
        ++ "]))"
    )
  Environment.setEnv "BUILD_COMMAND" "ghc -O -o $OUT_FILE $SCRIPT_FILE"
  Process.spawnProcess "nix-script" args
    >>= Process.waitForProcess
    >>= Exit.exitWith

getHaskellPackages :: FilePath -> IO [Text]
getHaskellPackages target = do
  sourceLines <- lines <$> readFileText target
  pure $ mapMaybe (Text.stripPrefix "#!haskellPackages ") sourceLines
