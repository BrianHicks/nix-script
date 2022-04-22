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
        packages.nix-script = naersk-lib.buildPackage rec {
          name = "nix-script";
          version = "2.0.0";

          root = ./.;

          buildInputs = [ pkgs.clippy ];
          target = [ name ];

          doCheck = true;
          checkPhase = ''
            cargo clippy -- --deny warnings

            # make sure we've listed the right version here (we can't grab
            # it from the manifest because we use a virtual manifest for the
            # various targets.) If this test fails, fix by making the `version`
            # attribute the same as the one in `nix-script/Cargo.toml`
            grep -q -e 'version = "${version}"' ${name}/Cargo.toml
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
