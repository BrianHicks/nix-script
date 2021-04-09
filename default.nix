{ sources ? import ./nix/sources.nix { }, pkgs ? import sources.nixpkgs { }
, pinnedPkgs ? sources.nixpkgs, ... }:
pkgs.symlinkJoin {
  name = "nix-script";
  paths = [
    (pkgs.callPackage ./nix-script { inherit pinnedPkgs; })
    (pkgs.callPackage ./nix-script-bash { inherit pinnedPkgs; })
    (pkgs.callPackage ./nix-script-haskell { inherit pinnedPkgs; })
  ];
}
