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
    (haskellPackages.ghcWithPackages (ps: [ ps.neat-interpolation ps.text ]))
    haskellPackages.ormolu
  ];
}
