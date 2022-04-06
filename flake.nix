{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    naersk.url = "github:nmattia/naersk";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import inputs.nixpkgs { inherit system; };
        naersk-lib = inputs.naersk.lib."${system}";
      in rec {
        packages.nix-script = naersk-lib.buildPackage {
          root = ./.;

          buildInputs = [ pkgs.clippy ];
        };

        defaultPackage = packages.nix-script;
        overlay = final: prev: { nix-script = packages.nix-script; };

        devShell = pkgs.mkShell {
          packages = [
            # rust
            pkgs.rustc
            pkgs.cargo
            pkgs.clippy
            pkgs.rustPackages.rustfmt

            # system
            pkgs.libiconv
          ];
        };
      });
}
