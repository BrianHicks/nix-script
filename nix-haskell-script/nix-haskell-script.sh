#!/usr/bin/env bash
set -euo pipefail

SOURCE="${1:-}"
if test -z "$SOURCE"; then
  echo "USAGE: nix-haskell-script path/to/script.hs"
  exit 1
fi

# make it easier to pass args later
shift

BUILD_INPUTS="(haskellPackages.ghcWithPackages (ps: with ps; [ $( (grep '#!haskellInputs ' "$SOURCE" || true) | cut -d ' ' -f 2-) ]))"

exec env BUILD_COMMAND="ghc -O -o \$OUT_FILE \$SCRIPT_FILE" BUILD_INPUTS="$BUILD_INPUTS" nix-script "$SOURCE" "${@}"
