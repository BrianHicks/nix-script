{ sources ? import ../nix/sources.nix { }, pkgs ? import sources.nixpkgs { }
, ... }:
let gitignore = pkgs.callPackage sources.gitignore { };
in pkgs.haskellPackages.callCabal2nix "nix-script"
(gitignore.gitignoreSource ./.) { }
