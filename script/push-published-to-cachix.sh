#!/usr/bin/env nix-shell
#!nix-shell -i bash -p cachix
set -euo pipefail

cachix push nix-script "$(nix-build https://github.com/BrianHicks/nix-script/archive/main.tar.gz)"
