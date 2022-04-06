#!/usr/bin/env nix-script
#!build cp $SRC $OUT
#!interpreter bash
#!runtimeInputs bash jq
# shellcheck shell=bash
set -euo pipefail

echo '{"message": "Hello, World!"}' | jq -r .message
