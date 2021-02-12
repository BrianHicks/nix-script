#!nix-script-bash
set -euo pipefail

cat <<EOF
I got these arguments:
${@}

And this environment:
SCRIPT_FILE: ${SCRIPT_FILE}
EOF
