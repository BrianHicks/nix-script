{ stdenv, makeWrapper, haskellPackages, pkgs, nixUnstable }:
stdenv.mkDerivation
{

  name = "nix-script";

  src = ./.;

  buildInputs = [ makeWrapper ];

  buildPhase = "true";

  doCheck = true;

  checkInputs = [ haskellPackages.hlint ];

  checkPhase = ''
    hlint .
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin

    makeWrapper ${haskellPackages.nix-script}/bin/nix-script $out/bin/nix-script \
      --set NIX_PATH nixpkgs=${pkgs} \
      --prefix PATH : ${pkgs.lib.makeBinPath [ nixUnstable ]}

     runHook postInstall
  '';
}
