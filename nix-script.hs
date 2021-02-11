#!/usr/bin/env runghc

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import NeatInterpolation (text)
import Relude
import qualified System.Directory as Directory
import qualified System.Environment as Environment
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
  -- hash the source & inputs to see if anything changed
  -- if the cached version doesn't exist or is a broken symlink:
  --   make a temporary directory
  --   build
  --   make a symlink to the result/bin/thing
  -- run the thing
  TextIO.putStrLn derivationTemplate

getDerivationTemplateFor :: FilePath -> IO Text
getDerivationTemplateFor target = do
  dirName <- Text.pack <$> Directory.makeAbsolute (FilePath.takeDirectory target)
  let fileName = Text.pack $ FilePath.takeFileName target
  source <- readFileText target
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
            # nix-script cache buster: 0
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
