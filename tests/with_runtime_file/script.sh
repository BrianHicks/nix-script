#!/usr/bin/env nix-script
#!buildRoot .
#!build cp $SRC $OUT
#!interpreter bash
#!runtimeInputs coreutils
#!runtimeFiles message
set -exuo pipefail

HERE="$(dirname "$(realpath $0)")"
cat $HERE/message
