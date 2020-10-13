---
title: Shebang nix-shell
updated: "2020-10-13"
---


Using `nix-shell`{.bash} this way allows us to create scripts that describe
their own dependencies.


# Usage

Basic usage:

``` {.usage data-lang=usage}
#!/usr/bin/env nix-shell
#!nix-shell [OPTIONS] -i INTERPRETER [--packages PACKAGES|-p PACKAGES|PATH]
[#!nix-shell OPTIONS]
```

As shown above line `#!nix-shell`{.bash} can be repeated.  These lines are
parsed by `nix-shell`{.bash} itself, which differs slightly from how shell would parse
them.  For example **single quotes do not work**, and double quotes must be
used when e.g.  specifying Nix expression.

Somewhat useful documentation can be found in [`nix-shell(1)`
](https://nixos.org/nix/manual/#sec-nix-shell) manual page.


# Bash Script

``` {.bash data-lang=bash}
#!/usr/bin/env nix-shell
#!nix-shell -i bash -p curl

# shellcheck shell=bash

set -euo pipefail

function main() {
    : TODO
}

main "$@"
```


# Haskell (Shakefile)

``` {.haskell data-lang=haskell}
#!/usr/bin/env nix-shell
#!nix-shell -i runghc
#!nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ directory executable-path shake ])"

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wall -Wcompat #-}

-- |
-- Description: An example of Shake-based build system using nix-shell
-- Copyright:   (c) 2019 Peter Trško
-- License:     BSD3
--
-- An example of Shake-based build system using @nix-shell@ to provide all
-- dependencies needed by the buildsystem itself. This generalises to any
-- Haskell binary.
module Main (main)
  where

import Development.Shake


main :: IO ()
main = do
    opts@Options{..} <- getOptions
    shakeMain opts shakeOptions
        { shakeColor = True
--      , shakeFiles = ...
--      , shakeVersion = ...
--      , shakeChange = ChangeDigest
--      , shakeExtra = ...
        }

data Options = Options
    { shakeOptions :: ShakeOptions
--  , ...
    }

-- | Figure out some things that don't need to be tracked by Shake.
getOptions :: IO Options
getOptions = undefined  -- TODO

shakeMain :: ShakeOptions -> IO ()
shakeMain = shareArgs opts do
    ...

    want
        [ ...
        ]

    ...
```
