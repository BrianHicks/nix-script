{
  description =
    "write scripts in compiled languages that run in the nix ecosystem, with no separate build step";

  inputs = {
    utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/release-21.05";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, utils, flake-compat }:
    (utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
          config = { allowUnsupportedSystem = true; };
        };

        nix-script-shell = with pkgs; [
          nix-script
          nix-script-haskell
          nix-script-bash
          (haskellPackages.ghcWithPackages (p: with p; [ relude ]))
        ];
      in rec {
        packages = {
          inherit (pkgs) nix-script nix-script-bash nix-script-haskell;
        };

        defaultPackage = pkgs.nix-script;

        devShell = with pkgs;
          mkShell {
            buildInputs = [
              cabal-install
              haskellPackages.ghcid
              haskellPackages.hlint
              haskellPackages.ormolu
              nix-script-shell
              nixfmt
            ];
          };

        apps = {
          tests = utils.lib.mkApp {
            drv = with import nixpkgs { inherit system; };
              pkgs.writeShellScriptBin "nix-script-example-checks" ''
                set -xeuo pipefail
                export NIX_SCRIPT_CACHE_PATH=".nix-script-cache"
                export PATH=${
                  pkgs.lib.strings.makeBinPath
                  ([ pkgs.nixUnstable findutils coreutils ] ++ nix-script-shell)
                }

                (
                  cd nix-script
                  samples/test-has-script-file.hs
                  samples/test-receives-arguments.hs a b c
                  samples/test-receives-flags.hs --help
                  samples/test-has-runtime-input.hs
                  samples/test-program-name.hs

                  # regression test: we should not error out if the underlying
                  # builds get GC'd
                  find "$NIX_SCRIPT_CACHE_PATH" -type l \
                    | xargs --no-run-if-empty -n 1 readlink \
                    | xargs --no-run-if-empty nix-store --delete
                  samples/test-program-name.hs
                )

                (
                  cd nix-script-bash
                  samples/hello-world.sh
                  samples/with-dependencies.sh
                )

                (
                  cd nix-script-haskell
                  samples/hello-world.hs
                  samples/with-dependencies.hs
                  samples/no-extension
                  samples/test-receives-flags.hs --help
                )
              '';
          };

          # nix run .#lint for checking that source files are lint-free
          lint = utils.lib.mkApp {
            drv = with import nixpkgs { inherit system; };
              pkgs.writeShellScriptBin "nix-script-lint" ''
                set -xeuo pipefail
                export PATH=${
                  pkgs.lib.strings.makeBinPath [
                    nixfmt
                    haskellPackages.hlint
                    findutils
                  ]
                }

                nixfmt --check $(find . -name '*.nix')

                ( cd nix-script; hlint . )

                ( cd nix-script-haskell; hlint . )
              '';
          };

          # nix run .#cabal2nix for updating cabal2nix files
          cabal2nix = utils.lib.mkApp {
            drv = with import nixpkgs { inherit system; };
              pkgs.writeShellScriptBin "nix-script-cabal2nix" ''
                set -xeuo pipefail
                export PATH=${pkgs.lib.strings.makeBinPath [ cabal2nix nixfmt ]}

                (
                  cd nix-script
                  cabal2nix . > default.nix
                  nixfmt default.nix
                )

                (
                  cd nix-script-haskell
                  cabal2nix . > default.nix
                  nixfmt default.nix
                )
              '';
          };
        };
      })) // {
        overlay = final: prev: {
          haskellPackages = prev.haskellPackages.override (old: {
            overrides =
              final.lib.composeExtensions (old.overrides or (_: _: { }))
              (hself: hsuper: {
                nix-script = with final.haskell.lib;
                  generateOptparseApplicativeCompletion "nix-script"
                  (overrideCabal (prev.haskellPackages.callPackage ./nix-script
                    { }
                    #prev.haskellPackages.callCabal2nix "nix-script" ./nix-script { }
                  ) (drv: {
                    buildTools = drv.buildTools or [ ] ++ [ final.makeWrapper ];
                    postInstall = with final;
                      drv.postInstall or "" + ''
                        wrapProgram $out/bin/nix-script \
                        --set NIX_PATH "nixpkgs=${final.path}" \
                        --prefix PATH ":" "${lib.makeBinPath [ nixUnstable ]}"
                      '';
                  }));
                nix-script-haskell =
                  prev.haskellPackages.callPackage ./nix-script-haskell { };
              });
          });

          nix-script = with final;
            haskell.lib.justStaticExecutables haskellPackages.nix-script;

          nix-script-haskell = with final;
            haskell.lib.justStaticExecutables
            haskellPackages.nix-script-haskell;

          nix-script-bash = prev.callPackage ./nix-script-bash { };
        };
      };
}
