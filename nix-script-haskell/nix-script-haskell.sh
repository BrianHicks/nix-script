#!/usr/bin/env bash
set -euo pipefail

SOURCE="${1:-}"
if test -z "$SOURCE"; then
  # defer to nix-script to print the error here
  exec -a "nix-script-haskell" nix-script
fi

# make it easier to pass args later
shift

BUILD_INPUTS="(haskellPackages.ghcWithPackages (ps: with ps; [ $( (grep '#!haskellInputs ' "$SOURCE" || true) | cut -d ' ' -f 2-) ]))"

exec -a "nix-script-haskell" env BUILD_COMMAND="ghc -O -o \$OUT_FILE \$SCRIPT_FILE" BUILD_INPUTS="$BUILD_INPUTS" nix-script "$SOURCE" "${@}"
