{ ... }:
let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  niv = import sources.niv { };
in with nixpkgs;
stdenv.mkDerivation {
  name = "nix-script";
  buildInputs = [
    niv.niv
    git
    (haskellPackages.ghcWithPackages (ps: [
      ps.base16-bytestring
      ps.cryptohash-sha256
      ps.neat-interpolation
      ps.relude
      ps.text
      ps.utf8-string
    ]))
    haskellPackages.ormolu
  ];
}
