{ sources ? import ./nix/sources.nix { }, pkgs ? import sources.nixpkgs { }, ...
}:
pkgs.symlinkJoin {
  name = "nix-script";
  paths = [
    (pkgs.callPackage ./nix-script { })
    (pkgs.callPackage ./nix-script-bash { })
    (pkgs.callPackage ./nix-script-haskell { })
  ];
}
