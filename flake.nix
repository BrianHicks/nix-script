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
    let
      rustTarget = { name, version, pkgs, naersk-lib, postInstall ? "" }:
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

      mkNixScript = pkgs: naersk-lib:
        rustTarget {
          name = "nix-script";
          version = "2.0.0";
          inherit pkgs;
          inherit naersk-lib;
        };

      mkNixScriptBash = pkgs:
        pkgs.writeShellScriptBin "nix-script-bash" ''
          exec ${pkgs.nix-script}/bin/nix-script \
            --build-command 'cp $SRC $OUT' \
            --interpreter bash \
            "$@"
        '';

      mkNixScriptHaskell = pkgs: naersk-lib:
        rustTarget rec {
          name = "nix-script-haskell";
          version = "2.0.0";
          inherit pkgs;
          inherit naersk-lib;

          postInstall = ''
            # avoid trying to package the dependencies step
            if test -f $out/bin/${name}; then
              wrapProgram $out/bin/${name} \
                --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.nix-script ]}
            fi
          '';
        };

      mkNixScriptAll = pkgs:
        pkgs.symlinkJoin {
          name = "nix-script-all";
          paths =
            [ pkgs.nix-script pkgs.nix-script-haskell pkgs.nix-script-bash ];
        };
    in {
      overlay = final: prev:
        let naersk-lib = inputs.naersk.lib."${final.system}";
        in {
          nix-script = mkNixScript prev naersk-lib;
          nix-script-bash = mkNixScriptBash prev;
          nix-script-haskell = mkNixScriptHaskell prev naersk-lib;
          nix-script-all = mkNixScriptAll prev;
        };
    } // inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [ inputs.self.overlay ];
        };
      in {
        packages = {
          nix-script = pkgs.nix-script;
          nix-script-bash = pkgs.nix-script-bash;
          nix-script-haskell = pkgs.nix-script-haskell;
          nix-script-all = pkgs.nix-script-all;
        };

        defaultPackage = pkgs.nix-script-all;

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
