#!/usr/bin/env runghc

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified System.Environment as Environment
import System.Exit (exitFailure)
import System.FilePath.Posix (takeBaseName, takeDirectory, takeFileName)
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
  putStrLn "USAGE: nix-script path/to/script"
  putStrLn ""
  putStrLn "But really, you should just use this in a script's shebang, like so:"
  putStrLn ""
  putStrLn "    #!/usr/bin/env nix-script"

buildAndRun :: FilePath -> IO ()
buildAndRun target = do
  (derivationTemplate, niceName) <- getDerivationTemplateFor target
  putStrLn derivationTemplate

getDerivationTemplateFor :: FilePath -> IO (String, String)
getDerivationTemplateFor target = do
  let niceName = takeBaseName target
  let dirName = takeDirectory target
  let fileName = takeFileName target
  source <- readFile target
  let sourceLines = lines source
  buildCommand <- getBuildCommand sourceLines
  buildInputs <- getBuildInputs sourceLines
  pure
    ( "{ pkgs ? import <nixpkgs> { }, ... }: pkgs.stdenv.mkDerivation { name = \""
        ++ niceName
        ++ "\"; src = builtins.filterSource (path: _: baseNameOf path == \""
        ++ fileName
        ++ "\") ../"
        ++ dirName
        ++ "; buildInputs = with pkgs; ["
        ++ buildInputs
        ++ "]; buildPhase = ''OUT_FILE="
        ++ niceName
        ++ "; SCRIPT_FILE="
        ++ fileName
        ++ "; "
        -- TODO: escape build command so strings don't mess it up
        ++ buildCommand
        ++ "''; installPhase = ''mkdir -p $out; mv "
        ++ niceName
        ++ " $out/"
        ++ niceName
        ++ "''; }",
      niceName
    )

getBuildCommand :: [String] -> IO String
getBuildCommand sourceLines = do
  fromEnv <- Environment.lookupEnv "BUILD_COMMAND"
  case fromEnv of
    Just command -> pure command
    Nothing ->
      case Maybe.listToMaybe (filter (List.isPrefixOf "#!build ") sourceLines) of
        Just line -> pure (List.drop 8 line)
        Nothing -> fail "I couldn't find a build statement. Either set BUILD_COMMAND or add a `#!build` line to your script."

getBuildInputs :: [String] -> IO String
getBuildInputs sourceLines = do
  fromEnv <- Environment.lookupEnv "BUILD_INPUTS"
  let fromSource = map (List.drop 15) $ filter (List.isPrefixOf "#!build-inputs ") sourceLines
  pure $ List.intercalate " " $
    case fromEnv of
      Just stuff -> stuff : fromSource
      Nothing -> fromSource
