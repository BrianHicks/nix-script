#!/usr/bin/env nix-script-bash
#!runtimeInputs jq

echo "Time to call jq!"
echo '{ "foo": "bar" }' | jq .
