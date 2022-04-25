# nix-script

`nix-script` lets you write quick scripts in compiled languages, transparently compile and cache them, and pull in whatever dependencies you need from the Nix ecosystem.

This README is intended more as a reference, but I also wrote [a blog post to explain what this is and why it exists](https://bytes.zone/posts/nix-script/).

## Installing

You might have guessed this already, but you can install this package with `nix`!

### Installing to Your Profile

```
nix-env -if https://github.com/BrianHicks/nix-script/archive/main.tar.gz
```

This project's CI also pushes Linux and macOS builds to [`nix-script.cachix.org`](https://app.cachix.org/cache/nix-script) automatically, meaning `cachix add nix-script` should set you up to compile fewer things.

### Installing with Flakes

Once added as a flake, we provide these attributes in `package`:

- `nix-script-all` (the default package): contains everything below
- `nix-script`: only `nix-script`
- `nix-script-bash`: only `nix-script-bash`, but referencing the correct `nix-script`
- `nix-script-haskell`: only `nix-script-haskell`, but referencing the correct `nix-script`

We also provide an `overlay`, which has all of these.

### Installing with Niv

Once added to Niv (`niv add BrianHicks/nix-script`), you should be able to `import sources.nix-script { };` and have the same things described in the flakes section above, except you'll have to explicitly reference things like `overlay."${builtins.currentSystem}"`.

## Commands

### `nix-script`

The normal `nix-script` invocation is controlled using shebang lines (lines starting with `#!` by default, although you can change it to whatever you like with the `--indicator` flag.)
Starting your file with `#!/usr/bin/env nix-script` makes these options available:

| What?                                 | Shebang line      | Notes                                                                             |
|---------------------------------------|-------------------|-----------------------------------------------------------------------------------|
| How to compile the script to a binary | `#!build`         | The command specified here must read from `SCRIPT_FILE` and write to `OUT_FILE`   |
| Use all files in the given directory  | `#!buildRoot`     | Must be a parent directory of the script                                          |
| Specify build-time dependencies       | `#!buildInputs`   | A space-separated list of Nix expressions                                         |
| Use an alternative interpreter        | `#!interpreter`   | Run this script with the given binary (must be in `runtimeInputs`)                |
| Specify runtime dependencies          | `#!runtimeInputs` | This should be a space-separated list of Nix expressions.                         |
| Access auxillary files at runtime     | `#!runtimeFiles`  | Make these files available at runtime (at the path given in `RUNTIME_FILES_ROOT`) |

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

#### Exporting, or How To Grow a Script

In nix-script version 1, it was common to run up against the limits of a single file, whether that meant having namespace issues or simply a single file becoming unwieldy.
Getting aroung this commonly meant giving up on all the nice things that nix-script provided (like faster feedback loops and transparent compilation caching) so it was a tough tradeoff.

Nix-script version 2 has two new flags to help with this: `--build-root` and `--export`.
Once you get to the point in your program's life cycle where you need multiple files, tell nix-script where the project root is with `#!buildRoot` (or `--build-root`) and we'll include all the files in that directory during builds.
This lets you do things like splitting out your source into multiple files, all of which will be checked when we try to determine whether or not we have a cache hit.

Once even that is not enough, you can include `--export` in your nix-script invocation to print out the `default.nix` that we would have used to build your script.
If you put that (or any `default.nix`) inside the directory specified in `#!buildRoot`, we'll use that instead of generating our own.

Once you get to the point of having a fully-realized directory with a `default.nix` inside, you've arrived at a "real" derivation, and you can then use any Nix tooling you like to further modify your project.

#### Parsing Directives

If you are making a wrapper script for a new language, you can also use `--build-root` to hold package manager files and extremely custom `build.nix` files.
We also provide a `--parse` flag which will ask `nix-script` to parse any directives in the script and give them to you as JSON on stdout.

**Caution:** be aware that the format here is not stable yet and may change in backwards-incompatible ways without a corresponding major version bump in nix-script.
If you have any feedback on the data returned by `--parse`, please open an issue!

### `nix-script-bash`

`nix-script-bash` exists to let you specify exact versions of your dependencies via Nix.
For example:

```bash
#!/usr/bin/env nix-script-bash
#!runtimeInputs jq

jq --help
```

### `nix-script-haskell`

`nix-script-haskell` is a convenience wrapper for Haskell scripts.
In addition to the regular `nix-script` options, `nix-script-haskell` lets you specify some Haskell-specific options:

| Shebang line        | Notes                                                                                                                      | Example                        |
|---------------------|----------------------------------------------------------------------------------------------------------------------------|--------------------------------|
| `#!haskellPackages` | Haskell packages to build with (you can get a list of available names by running `nix-env -qaPA nixpkgs.haskellPackages`.) | `#!haskellPackages text aeson` |
| `#!ghcFlags`        | Additional flags to pass to the compiler.                                                                                  | `#!ghcFlags -threaded`         |

You can get quick compilation feedback with [`ghcid`](https://github.com/ndmitchell/ghcid) by running `nix-script-haskell --ghcid path/to/your/script.hs`.

## Controlling `nixpkgs` version

`nix-script` will generate derivations that `import <nixpkgs> {}` by default.
That means all you need to do to control which `nixpkgs` your scripts are built with is to set `NIX_PATH`, for example to `NIX_PATH=nixpkgs=/nix/store/HASHHASHHASH-source`.
For projects, this is reasonably easy to do in a `mkShell` (for example by setting `NIX_PATH = "nixpkgs=${pkgs.path}"`,) or by using `makeWrapper` on `nix-script` in a custom derivation.

`NIX_PATH` is included in cache key calculations, so if you change your package set your scripts will automatically be rebuilt the next time you run them.

## Climate Action

I want my open-source work to support projects addressing the climate crisis (for example, projects in clean energy, public transit, reforestation, or sustainable agriculture.)
If you are working on such a project, and find a bug or missing feature in any of my libraries, **please let me know and I will treat your issue as high priority.**
I'd also be happy to support such projects in other ways, just ask!

## License

`nix-script` is licensed under the BSD 3-Clause license, located at `LICENSE`.
