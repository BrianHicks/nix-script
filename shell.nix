{ ... }:
let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  niv = import sources.niv { };
in with nixpkgs;
pkgs.mkShell {
  inputsFrom = [ (haskellPackages.callCabal2nix "nix-script" ./. { }).env ];
  buildInputs = [ niv.niv git cabal-install haskellPackages.ormolu ];
}
