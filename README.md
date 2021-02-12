# nix-script

`nix-script` lets you write quick scripts in compiled languages, tranparently compile and cache them, and pull in whatever dependencies you need from the Nix ecosystem.

## Installing

You might have guessed this already, but you can install this package with `nix`:

```
nix-env -if https://github.com/BrianHicks/nix-script/archive/main.tar.gz
```

You probably should use [`niv`](https://github.com/nmattia/niv) or similar, though!
Once you do, you can control the version of `nixpkgs` you use (see "[Controlling `nixpkgs` version](#controlling-nixpkgs-version)" below.)

This project's CI also pushes Linux builds to [`nix-script.cachix.org`](https://app.cachix.org/cache/nix-script) automatically.
I push macOS builds by hand when I remember.
(But there is not a lot of code in this project; a full rebuild is not too painful!)

## What? Why?

I like writing quick little scripts to avoid having to remember how to do things.
Most of the time I start in bash, thinking the task won't be too complicated... but then before I know it I'm having to reread the bash man pages to figure out how arrays work for the thousandth time.

So really, I'd rather write scripts in a language that offers some more safety and programmer ergonomics.
We're trying to learn Haskell at work, so maybe that could work?

Let's see a hello world:

```haskell
#!/usr/bin/env runghc

main :: IO ()
main = putStrLn "Hello, World!"
```

That seems pretty reasonable, and only takes like 300ms to compile and run.
But that's not as fast as a bash script, plus I have to use Haskell's standard prelude instead of something safer like [relude](https://kowainik.github.io/projects/relude) or [nri-prelude](http://hackage.haskell.org/package/nri-prelude).

It's reasonable to solve this, though: I can just use `nix-shell` to get a `ghc` with packages.

```haskell
#!/usr/bin/env nix-shell
#!nix-shell -i runghc -p "(pkgs.haskellPackages.ghcWithPackages (ps: [ ps.text ]))"

{-# LANGUAGE OverloadedStrings #-}

import Data.Text.IO

main :: IO ()
main = Data.Text.IO.putStrLn "Hello, World!"
```

Well, that works, but now run time has ballooned to over 2 seconds!
Eep!

Enter `nix-script`!
It transparently manages a compilation and nix-shell cache for scripts, and lets you specify dependencies and build commands inline using more shebangs!

That means the example above can be rewritten like so:

```haskell
#!/usr/bin/env nix-script
#!buildInputs (pkgs.haskellPackages.ghcWithPackages (ps: [ ps.text ]))
#!build ghc -O -o $OUT_FILE $SCRIPT_FILE

{-# LANGUAGE OverloadedStrings #-}

import Data.Text.IO

main :: IO ()
main = Data.Text.IO.putStrLn "Hello, World!"
```

The first time you run that, it'll compile the script to a binary and run it.
That takes about two seconds on my machine.

The second time you run it, we'll just use the compiled binary.
That takes 30ms or so for me!
Big improvement!

But it's really not that fun to have to figure out that `#!build` line every time, and I always forget how to call `pkgs.haskellPackages.ghcWithPackages` correctly... so there's also a wrapper script called `nix-script-haskell` that makes this nicer:

```haskell
#!/usr/bin/env nix-script-haskell
#!haskellPackages text

{-# LANGUAGE OverloadedStrings #-}

import Data.Text.IO

main :: IO ()
main = Data.Text.IO.putStrLn "Hello, World!"
```

And, in addition to the speed boost, we can depend on any package in the nix ecosystem!
For example, here's how you'd add and call `jq`:

```haskell
#!/usr/bin/env nix-script-haskell
#!runtimeInputs jq

import System.Process

main :: IO ()
main = do
  formatted <-
    readProcess
      "jq"
      ["--color-output", "."]
      "{\"name\": \"Atlas\", \"species\": \"kitty cat\"}"
  putStr formatted
```

It's also pretty easy to create more wrapping interpreters, so we also ship one for bash (even though it's not compiled, we can cache the nix environment with your exact dependencies!)

## Commands

### `nix-script`

The normal `nix-script` invocation is controlled using shebang lines.
Starting your file with `#!/usr/bin/env nix-script` makes these options available:

| What?                           | Shebang line      | Notes                                                                                                                                                                                                                           |
|---------------------------------|-------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Compile the script to a binary  | `#!build`         | The command specified here must read from `SCRIPT_FILE` and write to `OUT_FILE`                                                                                                                                                 |
| Specify build-time dependencies | `#!buildInputs`   | This should be a space-separated list of Nix expressions. For example, you can get `ghc` by specifying `haskellPackages.ghc`                                                                                                    |
| Specify runtime dependencies    | `#!runtimeInputs` | This should be a space-separated list of Nix expressions.                                                                                                                                                                       |
| Use an alternative interpreter  | `#!interpreter`   | Useful for running non-compiled languages. The interpreter specified will be used in a new shebang line on the first line of the compiled program. (For example, if you specify `bash` the line will be `#!/usr/bin/env bash`.) |

You can also control these options via environment variables in wrapper scripts (see the source of `nix-script-haskell/nix-script-haskell.sh` for an example.)

| Shebang line      | Environment variable |
|-------------------|----------------------|
| `#!build`         | `BUILD_COMMAND`      |
| `#!buildInputs`   | `BUILD_INPUTS`       |
| `#!runtimeInputs` | `RUNTIME_INPUTS`     |
| `#!interpreter`   | `INTERPRETER`        |

`nix-script` also lets your compiled script know where it came from by setting the `SCRIPT_FILE` environment variable to what you would have gotten in `$0` if it was a shell script.

#### Shell Mode

Building a new version for every change can get a little tiresome while developing.
If you want a quicker feedback loop, you can invoke `nix-script` and friends like `nix-script --shell path/to/script` to drop into a development shell with your build- and runtime dependencies.
This won't run your build command, but it will let you run it yourself, play around in repls, etc.

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
In addition to the regular `nix-script` options, this lets you specify `#!haskellPackages`, which should be a space-separated list of Haskell packages (you can get a list of available names by running `nix-env -qaPA nixpkgs.haskellPackages`.)
For example:

```haskell
#!/usr/bin/env nix-script-haskell
#!haskellPackages text

{-# LANGUAGE OverloadedStrings #-}

import Data.Text.IO

main :: IO ()
main = Data.Text.IO.putStrLn "Hello, World!"
```

Unlike other options, `#!haskellPackages` is not loaded from the environment.

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
