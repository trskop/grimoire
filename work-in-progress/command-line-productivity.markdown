---
title: Command Line Productivity Incantations
---


# Use ~/.config and Store it in Git

* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
* [ArchWiki: XDG Base Directory
  ](https://wiki.archlinux.org/index.php/XDG_Base_Directory)


# Fuzzy Text Selectors

*   [`fzf`](https://github.com/junegunn/fzf)

*   [Skim (`sk`)](https://github.com/lotabout/skim) -- basically `fzf`
    reimplementation in Rust.

*   [`fzy`](https://github.com/jhawthorn/fzy)


# Find and Grep Alternatives

*   [ripgrep `rg`](https://github.com/BurntSushi/ripgrep) is a fast alternative
    to `grep -r` (recursive Grep).

*   [`bfs`](https://github.com/tavianator/bfs) is a variant of the UNIX find
    command that operates breadth-first rather than depth-first.

*   [`fd`](https://github.com/sharkdp/fd) is a simple, fast and user-friendly
    alternative to [GNU `find`](https://www.gnu.org/software/findutils/).


# Processors and Interpreters for Various File Formats

## jq

Using `jq` as a templating language for JSON:

```
jq  --argjson name '"John Snow"' \
    --argjson email '"knows.nothing@winterfell.westeros"' \
    --null-input '{name: $name, email: $email}'
```


## yq

`jq` for YAML.  [mikefarah.github.io/yq](https://mikefarah.github.io/yq/)


## rq

`rq` (record query) is a tool that's used for performing queries on streams of
records in various formats.  [github.com/dflemstr/rq
](https://github.com/dflemstr/rq)


## Dhall

Officially "Dhall is a programmable configuration language optimized for
maintainability."  [dhall-lang.org](https://dhall-lang.org/)

*   [`dhall`](https://github.com/dhall-lang/dhall-haskell), including
    `dhall-to-bash`, `dhall-to-{json,yaml}`, and `{json,yaml}-to-dhall`


# Network-related

*   Traditional `nc`
*   OpenBSD `nc`
*   `socat`
*   `corkscrew`
*   `ssh`


# Other

*   [`bat`](https://github.com/sharkdp/bat)

*   `direnv`

*   `kitty`

*   [ShellCheck](https://github.com/koalaman/shellcheck)

*   [`yank`](https://github.com/mptre/yank)

*   [`dust`](https://github.com/bootandy/dust)


# Reading Material

*   <https://www.wezm.net/technical/2019/10/useful-command-line-tools/>
