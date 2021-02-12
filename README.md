# nix-script

`nix-script` lets you write quick scripts in compiled languages, tranparently compile and cache them, and pull in whatever dependencies you need from the Nix ecosystem.

## Installing

You might have guessed this already, but you can install this package with `nix`:

```
nix-env -if https://github.com/BrianHicks/nix-script/archive/main.zip
```

If you're using this in a project, it would probably be better to add it to your project with [`niv`](https://github.com/nmattia/niv) or similar:

```
niv add BrianHicks/nix-script
```

Once you have that, `sources.nix-script` is a derivation that will install `nix-script` and all language helpers.

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
It transparently manages a compilation and nix-shell catch for scripts, and lets you specify dependencies and build commands inline using more shebangs!

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

And, in addition to the speed boost, we can depend on any package in the nix ecosystem!
For example, here's how you'd add `jq`:

```haskell
#!/usr/bin/env nix-script
#!buildInputs pkgs.haskellPackages.ghc
#!build ghc -O -o $OUT_FILE $SCRIPT_FILE
#!runtimeInputs jq

import System.Process

main :: IO ()
main = callProcess "jq" ["--help"]
```

But it's really not that ergonomic to have to write `#!build` lines, and I always forget how to call `pkgs.haskellPackages.ghcWithPackages`, so there's also a wrapper script called `nix-script-haskell` that makes this more ergonomic:

```haskell
#!/usr/bin/env nix-script-haskell
#!haskellPackages text

{-# LANGUAGE OverloadedStrings #-}

import Data.Text.IO

main :: IO ()
main = Data.Text.IO.putStrLn "Hello, World!"
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
| Specify run-time dependencies   | `#!runtimeInputs` | This should be a space-separated list of Nix expressions.                                                                                                                                                                       |
| Use an alternative interpreter  | `#!interpreter`   | Useful for running non-compiled languages. The interpreter specified will be used in a new shebang line on the first line of the compiled program. (For example, if you specify `bash` the line will be `#!/usr/bin/env bash`.) |

You can also control these options via environment variables in wrapper scripts (see the source of `nix-script-haskell/nix-script-haskell.sh` for an example.)

| Shebang line      | Environment variable |
|-------------------|----------------------|
| `#!build`         | `BUILD_COMMAND`      |
| `#!buildInputs`   | `BUILD_INPUTS`       |
| `#!runtimeInputs` | `RUNTIME_INPUTS`     |
| `#!interpreter`   | `INTERPRETER`        |

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
