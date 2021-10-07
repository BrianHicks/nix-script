{ mkDerivation, base, lib, optparse-applicative, process, relude
, text
}:
mkDerivation {
  pname = "nix-script-haskell";
  version = "1.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base optparse-applicative process relude text
  ];
  license = lib.licenses.bsd3;
}
