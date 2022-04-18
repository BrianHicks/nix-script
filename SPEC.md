# Specification

`nix-script` is a program that brings the power and utility of Nix to scripting tasks.
It transparently compiles and caches both binaries and script dependencies to keep invocation fast.

## Reading this Document

This document describes nix-script version 2, a complete rewrite of nix-script version 1.

Each section is tagged with done-ness and determined-ness, with one of these values:

- *implemented*: this feature is implemented as specified in the section below
- *partially implemented*: this feature is not completely done.
  Details will be given.
- *defined*: this feature is ready to implemented.
  Details may change as we discover the system.
- *partially defined*: this feature still needs to be thought through completely.
  It may be partially implemented as a spike.
  This will be noted if so.
- *speculative*: this would be nice to have, but we need to think through it a lot more first.

## Transformation to Derivations

*status: implemented*

`nix-script` parses extra shebang (`#!`) lines into arguments to `mkDerivation`.
This set of shebangs, when placed in `cool-script`:

```haskell
#!/usr/bin/env nix-script
#!buildInputs (haskellPackages.ghcWithPackages (ps: [ ps.aeson ps.text ]))
#!runtimeInputs jq
#!buildPhase mv $SRC $SRC.hs; ghc -o $OUT $SRC.hs
```

... would produce a derivation that looked approximately like this:

```nix
{ pkgs ? import <nixpkgs> { }, jq ? pkgs.jq }:
pkgs.stdenv.mkDerivation {
  name = "cool-script";

  src = ./.; # actual implementation filters for `cool-script.hs`

  buildInputs = [ (pkgs.haskellPackages.ghcWithPackages (ps: [ ps.aeson ps.text ])) ];
  buildPhase = ''
    SRC=cool-script

    mkdir bin
    OUT=bin/cool-script

    # specified buildPhase below
    mv $SRC $SRC.hs; ghc -o $OUT $SRC.hs
  '';

  installPhase = ''
    mkdir -p $out
    mv bin $out/bin

    makeWrapper cool-script $out/bin/cool-script \
        --argv0 cool-script \
        --prefix PATH : ${pkgs.lib.makeBinPath [ jq ]}
  '';
}
```

You can also control these settings with flags (e.g. `nix-script --build-command ...`) or add to the directives specified in the source (e.g. `nix-script --runtime-input ...`.)
Command-line arguments always take precedence, then shebangs.

### Keys Accepted

*status: implemented*

| `#!` line         | Meaning                                       | Notes                                                                                                                |
|-------------------|-----------------------------------------------|----------------------------------------------------------------------------------------------------------------------|
| `#!build`         | build command for script                      | should read from `$INPUT` and write to `$OUTPUT`. Will be run in the source directory.                               |
| `#!buildRoot`     | build step will include all source here       | must be a parent directory of the script                                                                             |
| `#!buildInputs`   | build inputs, as a Nix list                   | e.g. `buildInputs = [ the-thing-you-specify ];`.                                                                     |
| `#!runtimeInputs` | runtime inputs, as a Nix list                 | see note on `buildInputs`.                                                                                           |
| `#!interpreter`   | interpret "built" binary with this script     | Must be a binary which accepts at least one argument (the build source). Binary must be provided by `runtimeInputs`. |
| `#!runtimeFiles`  | files or directories to include at build time | multiple calls will be merged.                                                                                       |

### What about environment variables as inputs?

*status: defined*

In `nix-script` version 1, we also accepted environment variables like `BUILD_COMMAND` and `RUNTIME_INPUTS`.
These were mostly useful for writing wrapper scripts, but in `nix-script` version 2, we do that differently.
However, if this ends up being something that breaks your workflow please open an issue and we'll see what we can do here.

### Lifting inputs

*status: implemented*

In the example above, `buildInputs` is not lifted to the top-level function arguments, but `runtimeInputs` is.
To do this, we parse these shebangs as a list.
Items that are expressions are left alone and items that appear to be references are lifted to the inputs.

### Exporting

*status: implemented*

Running `nix-script --export --build-root path/to path/to/script.sh` will print the derivation to stdout instead of building it.
We intend here to provide a mechanism for things like import-from-derivation.

## Caching

*status: implemented*

`nix-script` manages a directory of symlinks for caching.
The names of these links are script hashes and the targets are locations under `/nix/store`.

When `nix-script` is invoked, the basic operation is to:

1. create a hash of the script, based on parameters
1. build, if necessary
2. run the built derivation

"if necessary" here can mean a couple of different things:

1. there was not a match in the nix-script cache for the given hash
2. there was a match, but the binary is missing (e.g. because `nix-collect-garbage` removed it)

### Hash Calculation

*status: implemented*

The hash includes:

- the bytes of the script source
- the directives calculated between script source and command-line flags
- bytes of any files in the file specified by `--build-root`

## Shell mode

*status: implemented*

You can get into a bash shell with all your script's build-time dependencies by calling `nix-script --shell path-to-your-script`.

Shell mode implements many of the same command-line flags that `nix-shell` does.
For example:

- `--packages / -p PACKAGES...` to include the specified packages in the shel
- `--run CMD` to run a command in the shell
- `--pure` to do the equivalent of `nix-shell --pure` (that is: ignore your `PATH` and some other things)

Run `nix-script --help` to see the full set of commands.

## Wrapper Scripts

*status: speculative*

If `nix-script` is equivalent to `mkDerivation`, wrapper scripts are equivalent to [the language and framework support in nixpkgs](https://nixos.org/manual/nixpkgs/stable/#chap-language-support).

Wrapper scripts are expected to accept parsed shebang lines as environment variables and invoke `nix-script`.

For example, here's how you'd define `nix-script-bash`:

```sh
#!/usr/bin/env bash
set -euo pipefail

nix-script --build 'cp $SRC $OUT; chmod +x $OUT' --runtime-inputs bash "$@"
```

Wrapper scripts may also use `nix-script` to manage their own dependencies.

### Parsing shebangs

*status: partially implemented* (schema not finalized; breaking schema changes will not trigger a major version bump)

To help writing wrapper scripts, `nix-script` also provides a way to extract the shebang lines from a source file.
For example: `nix-script --parse $1` in the script above, assuming no other arguments existed.

For example:

```json
{
  "build_command": "mv $SRC $SRC.hs; ghc -o $OUT $SRC.hs",
  "build_root": null,
  "build_inputs": [
    {
      "raw": "haskellPackages.ghcWithPackages (ps: [ ps.text ])"
    }
  ],
  "interpreter": null,
  "runtime_inputs": [],
  "runtime_files": [],
  "raw": {
    "buildInputs": [
      "(haskellPackages.ghcWithPackages (ps: [ ps.text ]))"
    ],
    "/usr/bin/env": [
      "nix-script"
    ],
    "build": [
      "mv $SRC $SRC.hs; ghc -o $OUT $SRC.hs"
    ]
  }
}
```

## Runtime Variables

*status: implemented* (but this list may grow)

We set some environment variables that the script can access at runtime:

| Variable             | Meaning                                                                                                            |
|----------------------|--------------------------------------------------------------------------------------------------------------------|
| `RUNTIME_FILES_ROOT` | If `#!runtimeFiles` or `--runtime-files` was specified, this is set to where we put them.                          |
| `SCRIPT_FILE`        | the name of the script as originally invoked (name is awkward but remains for compatibility with nix-script 1.0.0) |
