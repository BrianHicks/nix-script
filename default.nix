{ sources ? import ./nix/sources.nix { }, pkgs ? import sources.nixpkgs { }, ...
}:
let gitignore = pkgs.callPackage sources.gitignore { };
in rec {
  nix-script-haskell = pkgs.stdenv.mkDerivation {
    name = "nix-script-haskell";

    src = ./nix-script-haskell;
    buildInputs = [ pkgs.makeWrapper ];
    buildPhase = "true";

    installPhase = ''
      mkdir -p $out
      mv nix-script-haskell.sh $out/nix-script-haskell

      wrapProgram $out/nix-script-haskell \
        --prefix PATH : ${pkgs.lib.makeBinPath [ nix-script ]}
    '';
  };

  nix-script-bash = pkgs.stdenv.mkDerivation {
    name = "nix-script-bash";

    buildInputs = [ pkgs.makeWrapper ];
    unpackPhase = "true";
    buildPhase = "true";

    installPhase = ''
      mkdir -p $out

      makeWrapper ${nix-script}/bin/nix-script $out/bin/nix-script-bash \
        --argv0 nix-script-bash \
        --set BUILD_COMMAND 'chmod +x $SCRIPT_FILE' \
        --set INTERPETER bash
    '';
  };
}
