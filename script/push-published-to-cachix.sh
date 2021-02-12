#!/usr/bin/env nix-shell
#!nix-shell -i bash -p cachix
set -euo pipefail

cachix watch-exec nix-script nix-build https://github.com/BrianHicks/nix-script/archive/main.zip
