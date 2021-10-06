{ stdenv, lib, makeWrapper, haskellPackages }:

stdenv.mkDerivation
{
  name = "nix-script-haskell";

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

    makeWrapper ${haskellPackages.nix-script-haskell}/bin/nix-script-haskell $out/bin/nix-script-haskell \
      --prefix PATH : ${lib.makeBinPath [ haskellPackages.nix-script ]}

    runHook postInstall
  '';
}
