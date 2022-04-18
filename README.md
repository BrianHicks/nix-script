# nix-script

`nix-script` lets you write quick scripts in compiled languages, transparently compile and cache them, and pull in whatever dependencies you need from the Nix ecosystem.

This README is intended more as a reference, but I also wrote [a blog post to explain what this is and why it exists](https://bytes.zone/posts/nix-script/).

## Installing

You might have guessed this already, but you can install this package with `nix`:

```
nix-env -if https://github.com/BrianHicks/nix-script/archive/main.tar.gz
```

NB: This will only install `nix-script` itself. To install e.g. `nix-script-haskell`, use:

```
nix-env -f https://github.com/BrianHicks/nix-script/archive/main.tar.gz -iA packages.x86_64-linux.nix-script-haskell
```

This project's CI also pushes Linux and macOS builds to [`nix-script.cachix.org`](https://app.cachix.org/cache/nix-script) automatically.

## Commands

### `nix-script`

The normal `nix-script` invocation is controlled using shebang lines.
Starting your file with `#!/usr/bin/env nix-script` makes these options available:

| What?                                | Shebang line      | Notes                                                                             |
|--------------------------------------|-------------------|-----------------------------------------------------------------------------------|
| Compile the script to a binary       | `#!build`         | The command specified here must read from `SCRIPT_FILE` and write to `OUT_FILE`   |
| Use all files in the given directory | `#!buildRoot`     | Must be a parent directory of the script                                          |
| Specify build-time dependencies      | `#!buildInputs`   | A space-separated list of Nix expressions                                         |
| Use an alternative interpreter       | `#!interpreter`   | Run this script with the given binary (must be in `runtimeInputs`)                |
| Specify runtime dependencies         | `#!runtimeInputs` | This should be a space-separated list of Nix expressions.                         |
| Access auxillary files at runtime    | `#!runtimeFiles`  | Make these files available at runtime (at the path given in `RUNTIME_FILES_ROOT`) |

you can also control these options with equivalent command-line flags to `nix-script` (see the `--help` output for exact names.)

`nix-script` also lets your compiled script know where it came from by setting the `SCRIPT_FILE` environment variable to what you would have gotten in `$0` if it was a shell script.

#### Shell Mode

Building a new version for every change can get a little tiresome while developing.
If you want a quicker feedback loop, you can include `--shell` in your `nix-script` invocation (e.g. `nix-script --shell path/to/script`) to drop into a development shell with your build-time and runtime dependencies.
This won't run your build command, but it will let you run it yourself, play around in REPLs, etc.

If you are making a wrapper script, you may find the `--run` flag useful: it allows you to specify what command to run in the shell.
If your language ecosystem has some common watcher script, it might be nice to add a special mode to your wrapper for it!
(For example, `nix-script-haskell` has a `--ghcid` flag for this purpose.
See the source for how it's set up!)

### `nix-script-bash`

`nix-script-bash` exists to let you specify exact versions of your dependencies via Nix.
For example:

```bash
#!/usr/bin/env nix-script-bash
#!runtimeInputs jq

jq --help
```

This is quicker than using `nix-shell` shebangs because the runtime environment calculation will be cached.

### `nix-script-haskell`

`nix-script-haskell` is a convenience wrapper for Haskell scripts.
In addition to the regular `nix-script` options, `nix-script-haskell` lets you specify some Haskell-specific options:

| Shebang line        | Notes                                                                                                                      | Example                        |
|---------------------|----------------------------------------------------------------------------------------------------------------------------|--------------------------------|
| `#!haskellPackages` | Haskell packages to build with (you can get a list of available names by running `nix-env -qaPA nixpkgs.haskellPackages`.) | `#!haskellPackages text aeson` |
| `#!ghcFlags`        | Additional flags to pass to the compiler.                                                                                  | `#!ghcFlags -threaded`         |

Unlike other shebang options, `#!haskellPackages` does not have an equivalent setting in the environment.

You can get quick compilation feedback with [`ghcid`](https://github.com/ndmitchell/ghcid) by running `nix-script-haskell --ghcid path/to/your/script.hs`.

## Controlling `nixpkgs` version

By default, `nix-script` will use the version of [nixpkgs](https://github.com/nixos/nixpkgs) we use to build scripts it's installed with.
If you want to change this, specify `pinnedPkgs` when installing `nix-script`.
For example, if you use `niv` that might look like:

```nix
let
  sources = import ./nix/sources.nix { };
  pkgs = import sources.nixpkgs { };
in pkgs.mkShell {
  buildInputs =
    [ (pkgs.callPackage sources.nix-script { pinnedPkgs = sources.nixpkgs; }) ];
}
```

The package set is included in the cache key calculations, so if you change your package set your scripts will automatically be rebuilt the next time you run them.

You can check which package set the runner will build from by examining the source of the `nix-script` wrapper installed on your `PATH`.

## Climate Action

I want my open-source work to support projects addressing the climate crisis (for example, projects in clean energy, public transit, reforestation, or sustainable agriculture.)
If you are working on such a project, and find a bug or missing feature in any of my libraries, **please let me know and I will treat your issue as high priority.**
I'd also be happy to support such projects in other ways, just ask!

## License

`nix-script` is licensed under the BSD 3-Clause license, located at `LICENSE`.
