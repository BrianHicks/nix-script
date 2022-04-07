#!/usr/bin/env nix-script
#!build cp $SRC $OUT
#!interpreter bash
#!runtimeInputs coreutils
#!runtimeFiles message
#!root .
set -exuo pipefail

HERE="$(dirname "$(realpath $0)")"
cat $HERE/message
