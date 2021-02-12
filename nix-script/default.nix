{ sources ? import ../nix/sources.nix { }, pkgs ? import sources.nixpkgs { }
, pinnedPkgs ? sources.nixpkgs, ... }:
let
  gitignore = pkgs.callPackage sources.gitignore { };
  nix-script = pkgs.haskellPackages.callCabal2nix "nix-script"
    (gitignore.gitignoreSource ./.) { };
in pkgs.stdenv.mkDerivation {
  name = "nix-script";

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

    makeWrapper ${nix-script}/bin/nix-script $out/bin/nix-script \
      --set NIX_PATH nixpkgs=${pinnedPkgs} \
      --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.nix ]}
  '';
}
