---
title: Life With XDG Base Directory Specification
updated: "2020-10-13"
---

[XDG Base Directory Specification
](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
provides a lot of interesting directories that can be used to avoid clutter in
one's `$HOME`.  Unfortunately, these directories can be overridden using
environment variables. This is annoying to work with, unless one knows how they
should be properly resolved.  In here we will describe how they should be
resolved in various languages.


# Bash

``` {.bash data-lang=bash}
readonly config_dir="${XDG_CONFIG_HOME:-${HOME}/.config}"
readonly data_dir="${XDG_DATA_HOME:-${HOME}/.local/share}"
readonly cache_dir="${XDG_CACHE_HOME:-${HOME}/.cache}"
readonly runtime_dir="${XDG_RUNTIME_DIR:-${TMPDIR:-/tmp}}"

readonly data_dirs="${XDG_DATA_DIRS:-/usr/local/share/:/usr/share/}"
readonly config_dirs="${XDG_CONFIG_DIRS:-/etc/xdg}"
```


# Dhall

``` {.dhall data-lang=dhall}
let home = env:HOME as Text
let temp-dir = env:TMPDIR as Text ? "/tmp"

in  { config-dir =
        env:XDG_CONFIG_HOME as Text ? "${home}/.config"

    , data-dir =
        env:XDG_DATA_HOME as Text ? "${home}/.local/share"

    , cache-dir =
        env:XDG_CACHE_HOME as Text ? "${home}/.cache"

    , runtime-dir =
        env:XDG_RUNTIME_DIR as Text ? temp-dir

    , data-dirs =
        env:XDG_DATA_DIRS as Text ? "/usr/local/share/:/usr/share/"

    , config-dirs =
        env:XDG_CONFIG_DIRS as Text ? "/etc/xdg"
    }
```


# Haskell

Dependencies:

- [`base`](http://hackage.haskell.org/package/base)
- [`directory`](http://hackage.haskell.org/package/directory) ≥ 1.3.2.0

``` {.haskell data-lang=haskell}
-- |
-- Description: An example of accessing XDG directories in Haskell
-- Copyright:   (c) 2019 Peter Trško
-- License:     BSD3
--
-- An example of accessing XDG directories in Haskell using minimum amount of
-- dependencies while still not requiring us to manually write the low-level
-- code. The only exception is 'getXdgDirectory', which requires sensible
-- defaults and may always need some hand-holding.
module XdgDirs
  where

import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import System.Directory
    ( XdgDirectory(..)
    , XdgDirectoryList(..)
    , getXdgDirectory
    , getXdgDirectoryList
    )


data XdgDirs = XdgDirs
    { configDir :: FilePath
    , dataDir :: FilePath
    , cacheDir :: FilePath
    , runtimeDir :: FilePath
    , dataDirs :: [FilePath]
    , configDirs :: [FilePath]
    }
  deriving (Show)

xdgDirs :: IO XdgDirs
xdgDirs = XdgDirs
    <$> getXdgDirectory XdgConfig ""
    <*> getXdgDirectory XdgData ""
    <*> getXdgDirectory XdgCache ""
    <*> getXdgRuntimeDirectory
    <*> getXdgDirectoryList XdgDataDirs
    <*> getXdgDirectoryList XdgConfigDirs
  where
    getXdgRuntimeDirectory = fromMaybe "/tmp"
        <$> ( lookupEnv "XDG_RUNTIME_DIR"
            >>= maybe (lookupEnv "TMPDIR") (pure . Just)
            )
```


# Related Articles

* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
* [ArchWiki: XDG Base Directory
  ](https://wiki.archlinux.org/index.php/XDG_Base_Directory)
* [Good default for XDG\_RUNTIME\_DIR?
  ](https://serverfault.com/questions/388840/good-default-for-xdg-runtime-dir)
