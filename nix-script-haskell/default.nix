{ sources ? import ../nix/sources.nix { }, pkgs ? import sources.nixpkgs { }
, ... }:
let gitignore = pkgs.callPackage sources.gitignore { };
in pkgs.stdenv.mkDerivation {
  name = "nix-script-haskell";

  src = gitignore.gitignoreSource ./.;
  buildInputs = [ pkgs.makeWrapper ];
  buildPhase = "true";

  installPhase = ''
    mkdir -p $out/bin
    mv nix-script-haskell.sh $out/bin/nix-script-haskell

    wrapProgram $out/bin/nix-script-haskell \
      --prefix PATH : ${
        pkgs.lib.makeBinPath [ (pkgs.callPackage ../nix-script { }) ]
      }
  '';
}
