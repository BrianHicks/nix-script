#!/usr/bin/env nix-script
#!build cp $SRC $OUT; chmod +x $OUT
#!interpreter bash
#!runtimeInputs bash jq
# shellcheck shell=bash
set -euo pipefail

echo '{"message": "Hello, World!"}' | jq .
