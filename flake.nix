{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    naersk.url = "github:nix-community/naersk";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import inputs.nixpkgs { inherit system; };
        naersk-lib = inputs.naersk.lib."${system}";

        rustTarget = { name, version, postInstall ? "" }:
          naersk-lib.buildPackage {
            inherit name;
            inherit version;
            inherit postInstall;

            root = ./.;

            buildInputs = [ pkgs.clippy pkgs.makeWrapper ];
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

            copyBinsFilter = ''
              select(.reason == "compiler-artifact" and .executable != null and .profile.test == false and .target.name == "${name}")'';
          };
      in rec {
        packages = {
          nix-script-all = pkgs.symlinkJoin {
            name = "nix-script-all";
            paths = [
              packages.nix-script
              packages.nix-script-haskell
              packages.nix-script-bash
            ];
          };

          nix-script = rustTarget {
            name = "nix-script";
            version = "2.0.0";
          };

          nix-script-bash = pkgs.writeShellScriptBin "nix-script-bash" ''
            exec ${packages.nix-script}/bin/nix-script \
              --build-command 'cp $SRC $OUT' \
              --interpreter bash \
              "$@"
          '';

          nix-script-haskell = rustTarget rec {
            name = "nix-script-haskell";
            version = "2.0.0";

            postInstall = ''
              # avoid trying to package the dependencies step
              if test -f $out/bin/${name}; then
                wrapProgram $out/bin/${name} \
                  --prefix PATH : ${
                    pkgs.lib.makeBinPath [ packages.nix-script ]
                  }
              fi
            '';
          };
        };

        defaultPackage = packages.nix-script-all;

        overlay = final: prev: {
          nix-script-all = packages.nix-script-all;
          nix-script = packages.nix-script;
          nix-script-bash = packages.nix-script-bash;
          nix-script-haskell = packages.nix-script-haskell;
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
