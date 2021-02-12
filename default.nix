{ sources ? import ./nix/sources.nix { }, pkgs ? import sources.nixpkgs { }, ...
}:
let gitignore = pkgs.callPackage sources.gitignore { };
in rec {
  nix-script = pkgs.haskellPackages.callCabal2nix "nix-script"
    (gitignore.gitignoreSource ./.) { };

  nix-haskell-script = pkgs.stdenv.mkDerivation {
    name = "nix-haskell-script";

    src = ./nix-haskell-script;
    buildInputs = [ pkgs.makeWrapper ];
    buildPhase = "true";

    installPhase = ''
      mkdir -p $out
      mv nix-haskell-script.sh $out/nix-haskell-script

      wrapProgram $out/nix-haskell-script \
        --prefix PATH : ${pkgs.lib.makeBinPath [ nix-script ]}
    '';
  };
}
