{ mkDerivation, base, base16-bytestring, cryptohash-sha256, data-fix, directory
, filepath, hnix, lib, neat-interpolation, optparse-applicative, prettyprinter
, process, relude, temporary, text, utf8-string, xdg-basedir }:
mkDerivation {
  pname = "nix-script";
  version = "1.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
    base16-bytestring
    cryptohash-sha256
    data-fix
    directory
    filepath
    hnix
    neat-interpolation
    optparse-applicative
    prettyprinter
    process
    relude
    temporary
    text
    utf8-string
    xdg-basedir
  ];
  license = lib.licenses.bsd3;
}
