#!/usr/bin/env nix-script
#!build cp $SRC $OUT
#!interpreter bash
#!runtimeInputs bash
set -euo pipefail

echo "$SCRIPT_FILE"
