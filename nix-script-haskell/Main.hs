import qualified Data.Text as Text
import qualified Options.Applicative as Options
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.Process as Process

data Options = Options
  { isGhcidMode :: Bool,
    isShellMode :: Bool,
    target :: FilePath,
    args :: [String]
  }

optionsParser :: Options.ParserInfo Options
optionsParser =
  Options.info
    ( Options
        <$> Options.switch
          ( Options.long "ghcid"
              <> Options.help "Launch a ghcid session watching TARGET"
          )
        <*> Options.switch
          ( Options.long "shell"
              <> Options.help "Enter a shell with all script dependencies"
          )
        <*> Options.strArgument
          (Options.metavar "SCRIPT" <> Options.help "Path to the script to run")
        <*> many (Options.strArgument (Options.metavar "ARGS" <> Options.help "Arguments to pass to your script"))
        <**> Options.helper
    )
    ( Options.fullDesc
        <> Options.progDesc "Does the same as nix-script, but specializes some options for scripts written in Haskell."
        <> Options.footer "This program handles flags the same way as nix-script, namely: if you use --help, --ghcid, or --shell it must be before any positional arguments. Otherwise, it will be passed to your script after compilation."
        <> Options.forwardOptions -- so script targets can define their own --flags
        <> Options.noIntersperse -- so script targets can define flags that we also use
    )

main :: IO ()
main = do
  options <- Options.execParser optionsParser
  haskellPackages <- getHaskellPackages (target options)
  ghcFlags <- getGhcFlags (target options)
  if isGhcidMode options
    then do
      Environment.setEnv "RUNTIME_INPUTS" "haskellPackages.ghcid"
      Environment.setEnv "SHELL_RUN" ("ghcid " ++ target options)
      callNixScript haskellPackages ghcFlags ["--shell", target options]
    else
      if isShellMode options
        then do
          callNixScript haskellPackages ghcFlags ("--shell" : target options : args options)
        else do
          callNixScript haskellPackages ghcFlags (target options : args options)

callNixScript :: [Text] -> [Text] -> [String] -> IO ()
callNixScript haskellPackages flags args = do
  Environment.setEnv
    "BUILD_INPUTS"
    ( "(haskellPackages.ghcWithPackages (ps: with ps; ["
        ++ toString (Text.intercalate " " haskellPackages)
        ++ "]))"
    )
  -- We have to add a `.hs` extension if it isn't present because otherwise
  -- Haskell thinks we're trying to compile a named module instead of a file
  -- in cases where the script file does not have an extension.
  Environment.setEnv
    "BUILD_COMMAND"
    ( "mv $SCRIPT_FILE $SCRIPT_FILE.hs && ghc -O "
        ++ toString (Text.intercalate " " flags)
        ++ " -o $OUT_FILE $SCRIPT_FILE.hs"
    )
  Process.spawnProcess "nix-script" args
    >>= Process.waitForProcess
    >>= Exit.exitWith

getHaskellPackages :: FilePath -> IO [Text]
getHaskellPackages target = do
  sourceLines <- lines <$> readFileText target
  pure $ mapMaybe (Text.stripPrefix "#!haskellPackages ") sourceLines

getGhcFlags :: FilePath -> IO [Text]
getGhcFlags target = do
  sourceLines <- lines <$> readFileText target
  pure $ mapMaybe (Text.stripPrefix "#!ghcFlags ") sourceLines
