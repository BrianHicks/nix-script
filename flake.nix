{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    naersk.url = "github:nix-community/naersk";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import inputs.nixpkgs { inherit system; };
        naersk-lib = inputs.naersk.lib."${system}";
      in rec {
        packages.nix-script = naersk-lib.buildPackage {
          name = "nix-script";

          root = ./.;

          buildInputs = [ pkgs.clippy ];
          target = [ "nix-script" ];

          doCheck = true;
          checkPhase = ''
            cargo clippy -- --deny warnings
          '';
        };

        packages.nix-script-bash = pkgs.writeShellScriptBin "nix-script-bash" ''
          exec ${packages.nix-script}/bin/nix-script \
            --build-command 'cp $SRC $OUT' \
            --interpreter bash \
            "$@"
        '';

        defaultPackage = packages.nix-script;
        overlay = final: prev: {
          nix-script = packages.nix-script;
          nix-script-bash = packages.nix-script-bash;
        };

        devShell = pkgs.mkShell {
          NIX_PKGS = inputs.nixpkgs;

          packages = [
            # rust
            pkgs.rustc
            pkgs.cargo
            pkgs.cargo-edit
            pkgs.clippy
            pkgs.rustPackages.rustfmt

            # system
            pkgs.libiconv
          ];
        };
      });
}
