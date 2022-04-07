#!/usr/bin/env nix-script
#!buildRoot .
#!build cp $SRC $OUT
#!interpreter bash
#!runtimeFiles message
set -euo pipefail

cat "$RUNTIME_FILES_ROOT/message"
