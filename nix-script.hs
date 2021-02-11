#!/usr/bin/env runghc

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.UTF8
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import NeatInterpolation (text)
import Relude
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.FilePath.Posix as FilePath
import System.FilePath.Posix ((</>))
import qualified System.Process as Process

main :: IO ()
main = do
  args <- Environment.getArgs
  case args of
    [] -> do
      printUsage
      exitFailure
    [target] ->
      buildAndRun target
    _many -> do
      printUsage
      exitFailure

printUsage :: IO ()
printUsage = do
  TextIO.putStrLn
    [text|
      USAGE: nix-script path/to/script

      But really, you should just use this in a script's shebang, like so:

          #!/usr/bin/env nix-script
    |]

buildAndRun :: FilePath -> IO ()
buildAndRun target = do
  source <- readFileText target
  derivationTemplate <- getDerivationTemplateFor target source
  -- what's our target?
  cacheDir <- getCacheDir
  let hash = Base16.encode $ SHA256.finalize $ SHA256.updates SHA256.init [encodeUtf8 source, encodeUtf8 derivationTemplate]
  let cacheTarget = cacheDir </> (FilePath.takeFileName target ++ "-" ++ Data.ByteString.UTF8.toString hash)
  -- rebuild, if necessary
  needToBuild <- not <$> existsAsValidSymlink cacheTarget
  if needToBuild
    then build cacheTarget derivationTemplate
    else pure ()
  -- run the thing
  TextIO.putStrLn derivationTemplate

build :: FilePath -> Text -> IO ()
build destination nixSource = do
  let dir = FilePath.takeDirectory destination
  Directory.createDirectoryIfMissing True dir
  TextIO.writeFile (dir </> "default.nix") nixSource
  outLines <- List.lines <$> Process.readProcess "nix-build" ["--no-out-link", dir] []
  out <- case outLines of
    [] -> fail "nix-build did not give me a store path. How weird!"
    first : _ -> pure first
  print out

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
  Maybe.fromMaybe ".nix-script-cache" <$> Environment.lookupEnv "NIX_SCRIPT_CACHE_PATH"

getDerivationTemplateFor :: FilePath -> Text -> IO Text
getDerivationTemplateFor target source = do
  dirName <- Text.pack <$> Directory.makeAbsolute (FilePath.takeDirectory target)
  let fileName = Text.pack $ FilePath.takeFileName target
  let sourceLines = lines source
  buildCommand <- getBuildCommand sourceLines
  buildInputs <- getBuildInputs sourceLines
  runtimeInputs <- getRuntimeInputs sourceLines
  let callWrapProgram =
        if runtimeInputs /= ""
          then [text|wrapProgram $$out/$fileName --prefix PATH : $${with pkgs; pkgs.lib.makeBinPath [ $runtimeInputs ]}|]
          else "true"
  pure
    ( [text|
        { pkgs ? import <nixpkgs> { }, ... }:
        pkgs.stdenv.mkDerivation {
          name = "$fileName";
          src = builtins.filterSource (path: _: path == "$dirName/$fileName") $dirName;

          buildInputs = with pkgs; [ makeWrapper $buildInputs ];
          buildPhase = ''
            OUT_FILE=$fileName
            SCRIPT_FILE=$fileName

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
    )

getBuildCommand :: [Text] -> IO Text
getBuildCommand sourceLines = do
  fromEnv <- Environment.lookupEnv "BUILD_COMMAND"
  case fromEnv of
    Just command -> pure $ Text.pack command
    Nothing ->
      case catMaybes $ map (Text.stripPrefix "#!build ") sourceLines of
        [] -> fail "I couldn't find a build statement. Either set BUILD_COMMAND or add a `#!build` line to your script."
        [only] -> pure only
        many_ -> fail "I found many `#!build` statements in the source, but I can only handle one!"

getBuildInputs :: [Text] -> IO Text
getBuildInputs sourceLines = do
  fromEnv <- Environment.lookupEnv "BUILD_INPUTS"
  let fromSource = map (Text.stripPrefix "#!buildInputs ") sourceLines
  pure $ Text.intercalate " " $ catMaybes $ (fmap Text.pack fromEnv : fromSource)

getRuntimeInputs :: [Text] -> IO Text
getRuntimeInputs sourceLines = do
  fromEnv <- Environment.lookupEnv "RUNTIME_INPUTS"
  let fromSource = map (Text.stripPrefix "#!runtimeInputs ") sourceLines
  pure $ Text.intercalate " " $ catMaybes $ (fmap Text.pack fromEnv : fromSource)
