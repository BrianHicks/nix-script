{ sources ? import ../nix/sources.nix { }, pkgs ? import sources.nixpkgs { }
, ... }:
let
  gitignore = pkgs.callPackage sources.gitignore { };

  nix-script = pkgs.callPackage ../nix-script { };
  nix-script-haskell = pkgs.haskellPackages.callCabal2nix "nix-script-haskell"
    (gitignore.gitignoreSource ./.) { };
in pkgs.stdenv.mkDerivation {
  name = "nix-script-haskell";

  src = gitignore.gitignoreSource ./.;

  buildInputs = [ pkgs.makeWrapper ];
  buildPhase = "true";

  doCheck = true;
  checkInputs = [ pkgs.haskellPackages.hlint ];
  checkPhase = ''
    hlint .
  '';

  installPhase = ''
    mkdir -p $out/bin

    makeWrapper ${nix-script-haskell}/bin/nix-script-haskell $out/bin/nix-script-haskell \
      --prefix PATH : ${pkgs.lib.makeBinPath [ nix-script ]}
  '';
}
