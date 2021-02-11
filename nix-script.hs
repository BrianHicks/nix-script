#!/usr/bin/env runghc

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Text (Text, pack)
import qualified Data.Text.IO as TextIO
import NeatInterpolation (text)
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import System.Exit (exitFailure)
import qualified System.FilePath.Posix as FilePath
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
  derivationTemplate <- getDerivationTemplateFor target
  TextIO.putStrLn derivationTemplate

getDerivationTemplateFor :: FilePath -> IO Text
getDerivationTemplateFor target = do
  dirName <- pack <$> Directory.makeAbsolute (FilePath.takeDirectory target)
  let fileName = pack $ FilePath.takeFileName target
  source <- readFile target
  let sourceLines = lines source
  buildCommand <- getBuildCommand sourceLines
  buildInputs <- getBuildInputs sourceLines
  pure
    ( [text|
        { pkgs ? import <nixpkgs> { }, ... }:
        pkgs.stdenv.mkDerivation {
          name = "$fileName";
          src = builtins.filterSource (path: _: path == "$dirName/$fileName") $dirName;

          buildInputs = with pkgs; [ $buildInputs ];
          buildPhase = ''
            OUT_FILE=$fileName
            SCRIPT_FILE=$fileName

            # TODO: this should be escaped somehow so double single primes
            # don't mess it up
            $buildCommand
          '';

          installPhase = ''
            mkdir -p $$out
            mv $fileName $$out/$fileName
          '';
        }
      |]
    )

getBuildCommand :: [String] -> IO Text
getBuildCommand sourceLines = do
  fromEnv <- Environment.lookupEnv "BUILD_COMMAND"
  case fromEnv of
    Just command -> pure $ pack command
    Nothing ->
      case Maybe.listToMaybe (filter (List.isPrefixOf "#!build ") sourceLines) of
        Just line -> pure $ pack $ List.drop 8 line
        Nothing -> fail "I couldn't find a build statement. Either set BUILD_COMMAND or add a `#!build` line to your script."

getBuildInputs :: [String] -> IO Text
getBuildInputs sourceLines = do
  fromEnv <- Environment.lookupEnv "BUILD_INPUTS"
  let fromSource = map (List.drop 15) $ filter (List.isPrefixOf "#!build-inputs ") sourceLines
  pure $ pack $ List.intercalate " " $
    case fromEnv of
      Just stuff -> stuff : fromSource
      Nothing -> fromSource
