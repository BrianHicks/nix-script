import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as Base16
import Data.Fix (Fix (Fix))
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Prettyprint.Doc as Doc
import qualified Data.Text.Prettyprint.Doc.Render.Text as RenderText
import NeatInterpolation (text)
import qualified Nix.Expr.Types as NET
import qualified Nix.Parser
import qualified Nix.Pretty
import qualified Options.Applicative as Options
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import System.FilePath.Posix ((</>))
import qualified System.FilePath.Posix as FilePath
import qualified System.Process as Process
import qualified System.IO.Temp as Temp

data Options = Options
  { isShellMode :: Bool,
    target :: FilePath,
    args :: [String]
  }

optionsParser :: Options.ParserInfo Options
optionsParser =
  Options.info
    ( Options
        <$> Options.switch
          ( Options.long "shell"
              <> Options.help "Enter a shell with all script dependencies"
          )
        <*> Options.strArgument
          (Options.metavar "SCRIPT" <> Options.help "Path to the script to run")
        <*> many (Options.strArgument (Options.metavar "ARGS" <> Options.help "Arguments to pass to your script"))
        <**> Options.helper
    )
    ( Options.fullDesc
        <> Options.progDesc "Transparently manage a compilation cache for scripts written in compiled languages. When you use this program as the interpreter in a script by saying `#!/usr/bin/env nix-script` on the first line, it will automatically manage dependencies for you. See the project README for the full directive list."
        <> Options.footer "Since this program is intended to be used as an interpreter, flags like --shell must be passed before the script name. Otherwise, they will be passed to your script as-is! That means an invocation like `nix-script --help test.hs` will show help, but `nix-script test.hs --help` will pass `--help` to the program in `test.hs`."
        <> Options.forwardOptions -- so script targets can define their own --flags
        <> Options.noIntersperse -- so script targets can define flags that we also use
    )

main :: IO ()
main = do
  options <- Options.execParser optionsParser
  if isShellMode options
    then enterShell (target options)
    else buildAndRun (target options) (args options)

enterShell :: FilePath -> IO ()
enterShell target = do
  source <- readFileText target
  let sourceLines = lines source
  buildInputs <- getBuildInputs sourceLines
  runtimeInputs <- getRuntimeInputs sourceLines
  packages <-
    if buildInputs == "" && runtimeInputs == ""
      then do
        let targetForProblem = toText target
        TextIO.hPutStrLn stderr [text|$targetForProblem doesn't have any build-time or runtime dependencies. Nothing for me to do!|]
        exitFailure
      else case deps [buildInputs, runtimeInputs] of
        Left problem -> do
          TextIO.hPutStrLn stderr problem
          exitFailure
        Right success ->
          pure $ map toString success
  maybeRun <- Environment.lookupEnv "SHELL_RUN"

  let args =
        concat $
          catMaybes
            [ fmap (\run -> ["--run", run]) maybeRun,
              Just ("-p" : intersperse "-p" packages)
            ]
  Environment.setEnv "SCRIPT_FILE" target
  Process.spawnProcess "nix-shell" args
    >>= Process.waitForProcess
    >>= Exit.exitWith

buildAndRun :: FilePath -> [String] -> IO ()
buildAndRun target args = do
  canonicalTarget <- Directory.canonicalizePath target
  source <- readFileText canonicalTarget
  derivationTemplate <- getDerivationTemplateFor canonicalTarget source
  -- what's our target?
  cacheDir <- getCacheDir
  Directory.createDirectoryIfMissing True cacheDir
  nixPath <- fromMaybe "" <$> Environment.lookupEnv "NIX_PATH"
  let hash =
        Base16.encode $
          SHA256.finalize $
            SHA256.updates
              SHA256.init
              [ encodeUtf8 source,
                encodeUtf8 derivationTemplate,
                encodeUtf8 nixPath
              ]
  let cacheTarget = cacheDir </> (FilePath.takeFileName canonicalTarget ++ "-" ++ decodeUtf8 hash)

  -- rebuild, if necessary
  state <- symlinkState cacheTarget
  case state of
    TargetExists -> pass
    _ -> do
      builtDerivation <- build derivationTemplate
      link cacheTarget (builtDerivation </> FilePath.takeFileName canonicalTarget)

  -- run the thing
  Environment.setEnv "SCRIPT_FILE" target
  Process.spawnProcess cacheTarget args >>= Process.waitForProcess >>= Exit.exitWith

build :: Text -> IO FilePath
build nixSource = do
  Temp.withSystemTempDirectory "nix-script" $ \dir -> do
    writeFile (dir </> "default.nix") (toString nixSource)
    outLines <- List.lines <$> Process.readProcess "nix-build" ["--no-out-link", dir] []
    case outLines of
      [] -> fail "nix-build did not give me a store path. How weird!"
      first : _ -> pure first

link :: FilePath -> FilePath -> IO ()
link destination built = do
  state <- symlinkState destination
  case state of
    DoesNotExist -> pass
    NotASymlink -> fail ("I expected " ++ destination ++ " to be a symlink, but it wasn't. What's going on?")
    _ -> do
      Directory.removeFile destination
      putStrLn ("removing " ++ destination)

  Directory.createFileLink built destination

data SymlinkState
  = DoesNotExist
  | NotASymlink
  | TargetExists
  | TargetAbsent
  deriving (Show)

symlinkState :: FilePath -> IO SymlinkState
symlinkState target = do
  exists <- Directory.doesPathExist target
  if not exists
    then pure DoesNotExist
    else do
      isSymlink <- Directory.pathIsSymbolicLink target
      if not isSymlink
        then pure NotASymlink
        else do
          targetExists <- Directory.doesFileExist =<< Directory.getSymbolicLinkTarget target
          if targetExists
            then pure TargetExists
            else pure TargetAbsent

getCacheDir :: IO FilePath
getCacheDir =
  fromMaybe ".nix-script-cache" <$> Environment.lookupEnv "NIX_SCRIPT_CACHE_PATH"

getDerivationTemplateFor :: FilePath -> Text -> IO Text
getDerivationTemplateFor canonicalTarget source = do
  let dirName = toText $ FilePath.takeDirectory canonicalTarget
  let fileName = toText $ FilePath.takeFileName canonicalTarget
  let sourceLines = lines source
  buildCommand <- getBuildCommand sourceLines
  buildInputs <- getBuildInputs sourceLines
  runtimeInputs <- getRuntimeInputs sourceLines
  installPhase <- getInstallPhase fileName sourceLines
  pure
    [text|
        { pkgs ? import <nixpkgs> { }, ... }:
        pkgs.stdenv.mkDerivation {
          name = "$fileName";
          src = builtins.filterSource (path: _: path == "$dirName/$fileName") $dirName;

          buildInputs = with pkgs; [ makeWrapper $buildInputs $runtimeInputs ];
          buildPhase = ''
            OUT_FILE=$fileName
            SCRIPT_FILE=$fileName

            # TODO: this should be escaped somehow so two single primes
            # don't mess it up
            $buildCommand
          '';

          installPhase = ''
            mkdir -p $$out
            $installPhase
          '';
        }
      |]

getInstallPhase :: Text -> [Text] -> IO Text
getInstallPhase fileName sourceLines = do
  runtimeInputs <- getRuntimeInputs sourceLines
  interpreter <- getInterpreter sourceLines
  let addPath =
        if runtimeInputs /= ""
          then [text|--prefix PATH : $${with pkgs; pkgs.lib.makeBinPath [ $runtimeInputs ]}|]
          else ""
  pure $ case words interpreter of
    [] ->
      [text|
        mv $fileName $$out/$fileName
        wrapProgram $$out/$fileName --argv0 $fileName $addPath
      |]
    command : args ->
      let flags = unwords args
       in [text|
        mv $fileName $$out/.$fileName
        makeWrapper "$$(command -v $command)" $$out/$fileName --argv0 $fileName --add-flags "$flags $$out/.$fileName" $addPath
       |]

getBuildCommand :: [Text] -> IO Text
getBuildCommand sourceLines = do
  fromEnv <- Environment.lookupEnv "BUILD_COMMAND"
  case fromEnv of
    Just command -> pure $ toText command
    Nothing ->
      case mapMaybe (Text.stripPrefix "#!build ") sourceLines of
        [] -> fail "I couldn't find a build statement. Either set BUILD_COMMAND or add a `#!build` line to your script."
        [only] -> pure only
        many_ -> fail "I found more than one `#!build` statements in the source, but I can only handle one!"

getBuildInputs :: [Text] -> IO Text
getBuildInputs sourceLines = do
  fromEnv <- Environment.lookupEnv "BUILD_INPUTS"
  let fromSource = map (Text.stripPrefix "#!buildInputs ") sourceLines
  pure $ Text.intercalate " " $ catMaybes (fmap toText fromEnv : fromSource)

getRuntimeInputs :: [Text] -> IO Text
getRuntimeInputs sourceLines = do
  fromEnv <- Environment.lookupEnv "RUNTIME_INPUTS"
  let fromSource = map (Text.stripPrefix "#!runtimeInputs ") sourceLines
  pure $ Text.intercalate " " $ catMaybes (fmap toText fromEnv : fromSource)

getInterpreter :: [Text] -> IO Text
getInterpreter sourceLines = do
  fromEnv <- Environment.lookupEnv "INTERPETER"
  case fromEnv of
    Just interpreter -> pure $ toText interpreter
    Nothing ->
      case mapMaybe (Text.stripPrefix "#!interpreter ") sourceLines of
        [] -> pure ""
        [only] -> pure only
        many_ -> fail "I found more than one `#!interpreter` statements in the source, but I can only handle one!"

deps :: [Text] -> Either Text [Text]
deps variousDeps = do
  let source = Text.concat ["[ ", Text.intercalate " " variousDeps, " ]"]
  let result = Nix.Parser.parseNixText source
  case result of
    Right (Fix (NET.NList deps)) -> do
      let docs = map Nix.Pretty.prettyNix deps
      Right (map (RenderText.renderStrict . Doc.layoutCompact) docs)
    Right node ->
      Left
        ( Text.concat
            [ "Internal error: expected a list, but got: ",
              RenderText.renderStrict $ Doc.layoutCompact $ Nix.Pretty.prettyNix node
            ]
        )
    Left problem ->
      Left
        ( Text.concat
            [ "I had a problem parsing the script's dependencies: ",
              RenderText.renderStrict $ Doc.layoutCompact problem
            ]
        )
