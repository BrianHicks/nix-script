{ stdenv, makeWrapper, haskellPackages }:

stdenv.mkDerivation {
  name = "nix-script-bash";

  buildInputs = [ makeWrapper ];

  unpackPhase = "true";

  buildPhase = "true";

  installPhase = ''
    runHook preInstall

    mkdir -p $out

    makeWrapper ${haskellPackages.nix-script}/bin/nix-script $out/bin/nix-script-bash \
      --argv0 nix-script-bash \
      --set BUILD_COMMAND 'chmod +x $SCRIPT_FILE' \
      --set INTERPETER bash

     runHook postInstall
  '';
}
