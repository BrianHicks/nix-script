import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as Base16
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import NeatInterpolation (text)
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import System.FilePath.Posix ((</>))
import qualified System.FilePath.Posix as FilePath
import qualified System.Process as Process

main :: IO ()
main = do
  args <- Environment.getArgs
  case args of
    [] -> do
      printUsage
      exitFailure
    ["--shell", target] ->
      enterShell target
    target : args ->
      buildAndRun target args

printUsage :: IO ()
printUsage = do
  name <- toText <$> Environment.getProgName
  TextIO.hPutStrLn
    stderr
    [text|
      USAGE: $name path/to/script

      But really, you should just use this in a script's shebang, like so:

          #!/usr/bin/env $name
    |]

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
      else pure $ toString $ Text.intercalate " " [buildInputs, runtimeInputs]
  Environment.setEnv "SCRIPT_FILE" target
  Process.callProcess "nix-shell" ["-p", packages]

buildAndRun :: FilePath -> [String] -> IO ()
buildAndRun target args = do
  source <- readFileText target
  derivationTemplate <- getDerivationTemplateFor target source
  -- what's our target?
  cacheDir <- getCacheDir
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
  let cacheTarget = cacheDir </> (FilePath.takeFileName target ++ "-" ++ decodeUtf8 hash)
  -- rebuild, if necessary
  needToBuild <- not <$> existsAsValidSymlink cacheTarget
  if needToBuild
    then build cacheTarget (FilePath.takeFileName target) derivationTemplate
    else pass
  -- run the thing
  Environment.setEnv "SCRIPT_FILE" target
  Process.spawnProcess cacheTarget args >>= Process.waitForProcess >>= Exit.exitWith

build :: FilePath -> FilePath -> Text -> IO ()
build destination builtFile nixSource = do
  let dir = FilePath.takeDirectory destination
  Directory.createDirectoryIfMissing True dir
  writeFile (dir </> "default.nix") (toString nixSource)
  outLines <- List.lines <$> Process.readProcess "nix-build" ["--no-out-link", dir] []
  built <- case outLines of
    [] -> fail "nix-build did not give me a store path. How weird!"
    first : _ -> pure first
  Directory.createFileLink (built </> builtFile) destination

existsAsValidSymlink :: FilePath -> IO Bool
existsAsValidSymlink target = do
  exists <- Directory.doesFileExist target
  if not exists
    then pure False
    else do
      isSymlink <- Directory.pathIsSymbolicLink target
      if not isSymlink
        then pure False
        else do
          linkTarget <- Directory.getSymbolicLinkTarget target
          Directory.doesFileExist linkTarget

getCacheDir :: IO FilePath
getCacheDir =
  fromMaybe ".nix-script-cache" <$> Environment.lookupEnv "NIX_SCRIPT_CACHE_PATH"

getDerivationTemplateFor :: FilePath -> Text -> IO Text
getDerivationTemplateFor target source = do
  dirName <- toText <$> Directory.canonicalizePath (FilePath.takeDirectory target)
  let fileName = toText $ FilePath.takeFileName target
  let sourceLines = lines source
  buildCommand <- getBuildCommand sourceLines
  buildInputs <- getBuildInputs sourceLines
  runtimeInputs <- getRuntimeInputs sourceLines
  interpreter <- getInterpreter sourceLines
  let swapInterpreter =
        if interpreter /= ""
          then [text|sed -i "1c#!/usr/bin/env $interpreter" $$SCRIPT_FILE|]
          else "true"
  let callWrapProgram =
        if runtimeInputs /= ""
          then [text|wrapProgram $$out/$fileName --prefix PATH : $${with pkgs; pkgs.lib.makeBinPath [ $runtimeInputs ]}|]
          else "true"
  pure
    [text|
        { pkgs ? import <nixpkgs> { }, ... }:
        pkgs.stdenv.mkDerivation {
          name = "$fileName";
          src = builtins.filterSource (path: _: path == "$dirName/$fileName") $dirName;

          buildInputs = with pkgs; [ makeWrapper $buildInputs ];
          buildPhase = ''
            OUT_FILE=$fileName
            SCRIPT_FILE=$fileName

            $swapInterpreter

            # TODO: this should be escaped somehow so two single primes
            # don't mess it up
            $buildCommand
          '';

          installPhase = ''
            mkdir -p $$out
            mv $fileName $$out/$fileName

            $callWrapProgram
          '';
        }
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
