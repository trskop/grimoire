---
title: Bash Scripting
---


# Portability

Writing portable Bash scripts is hard.  Not only it is impossible to guarantee
that executables we depend on are installed, with a correct version, and
compiled with correct compilation flags, but also Bash itself is prone to these
issues.

There are few possible solutions to this problem:

1.  Keep Bash scripts very simple and rely only on universally available
    commands.

    This is really, really, really, and I mean really hard, unless it's a
    few-liner of pure Bash (no external commands, just builtins) and we
    restrict ourselves to just a small subset of Bash features.

    Anything more complicated will eventually have to handle differences
    between systems, or use sub-par solutions to be portable.

2.  Use Nix to handle dependencies of our scripts, including Bash itself.

    One of the most common reasons Bash scripts are used is that Bash is so
    common.  It's not everywhere, but even if it's not immediately available,
    it is usually easy to install it.  Nix, not so much.  Nix also has a steep
    learning curve, and it itself becomes part of the script.

3.  Just don't.  Stop trying to write portable scripts, and focus on what is
    really important, the script itself.

    This is not always possible.  Sometimes we are writing a system script, or
    a script that bootstraps installation on multiple platform.  Reasons for
    needing portable script are endless.

In this article we will focus mostly on the last option, non-portable scripts.
This doesn't mean that we will abandon all good practices, quite the opposite.
It will free us from trying to make the script portable, which will allow us to
write Bash scripts that:

*   Follow good programming practices.  Yes, Bash scripting is programming.
    This is easily forgotten under the weight of portability and focus on
    administrative tasks.

*   Focus on solving the problem, not the endless list of quirks introduced by
    always increasing number of Linux distributions.

*   Provide better UX (user experience) to people using our scripts.


# Subtlety of Locale Hell

**TODO: What is locale and how it can introduce bugs.**


# Shebang

Also known as *hashbang* is a feature of UNIX-like operating systems.  It allows
us to pretend that script is an executable.  The best way how to make our Bash
script into an executable is to start it with following line:

```Bash
#!/usr/bin/env bash

# Here goes the script.
```

After that we just need to make it executable (`chmod +x FILE` works on many
systems).

What we should try to avoid is this:

```Bash
#!/bin/bash

# Here goes the script.
```

Using `env` to invoke Bash is commonly advised for portability, but it makes
sense even if we don't want to be portable at all.  Lets say that our project
uses a lot of Bash scripts that depend on a specific version of Bash, but our
system uses a different version.  This can be solved by modifying `PATH` when
working on our project:

```Bash
PATH="/path/to/our/specific/bin/directory:$PATH"
```

All we need now is to have required Bash version in our
`/path/to/our/specific/bin/directory`, `env` will do the rest.  There are
tricks and tools that can make this easier on us if we need to maintain
multiple environments like these.  To see how that can be done read through
[*Environment Management*](#environment-management) section.

Some reading material:

*   [nixCraft: Make Linux/Unix Script Portable With `#!/usr/bin/env` As a Shebang](https://www.cyberciti.biz/tips/finding-bash-perl-python-portably-using-env.html)


## Generic Form of Shebang

The most generic and most widely supported shebang has following syntax:

```Bash
#!/absolute/path/to/interpreter [OPTIONAL_ARGUMENT]
```

**TODO: Talk about it more. Mention that there can be at most one
`optional-arg` and why. Magic.**

Knowing above may come handy if we start mixing our Bash scripts with other
scripts that we want to call from it.

**TODO: Surprisingly useful: <https://en.wikipedia.org/wiki/Shebang_(Unix)>**


# Strict Mode

Bash strict mode is... Oh wait, which strict mode?  What actually is strict
mode?  As you probably guessed it's not that easy, but if you want simple
answer then this is the minimum every Bash script should start with:

```Bash
#!/usr/bin/env bash

set -e

# Here lies the script.
```

There are other, even stricter modes, but they all have issues.  The more
control we want to have over what our script does, and the more we want to do
actual programming, the more stricter we usually end up being.


## Issues With Unbound Variables

**TODO: Introduce `set -u` and its limitations, especially how it doesn't
really work on old versions of Bash**


## Last One, I Swear

```Bash
#!/usr/bin/env bash

# TODO: Describe individual options.
set -eo pipefail

if (( BASH_VERSINFO[0] > 4 || ( BASH_VERSINFO[0] == 4 && BASH_VERSINFO[1] >= 4 ) )); then
    # Treat unset variables and parameters as an error when expanding.  This
    # wasn't very reliable in older Bash versions, hence the version check.
    set -u

    # Command substitutions will inherits the value of the `set -e`, i.e.
    # `set -o errexit`.
    #
    # Available since Bash 4.4, this is actually something Bash does in POSIX
    # mode without it needed to be told.
    shopt -s inherit_errexit
fi

if [[ -n "${DEBUGGING_MODE:-}"  ]]; then
    # This will cause Bash to print commands before executing them.
    set -x
fi

# Here lies rest of the script.
```

If we really know that our Bash is of the proper version that we can simplify
it to:

```Bash
#!/usr/bin/env bash

# TODO: Describe individual options.
set -euo pipefail

# Command substitutions will inherits the value of the `set -e`, i.e.
# `set -o errexit`.
shopt -s inherit_errexit

if [[ -n "${DEBUGGING_MODE:-}"  ]]; then
    # This will cause Bash to print commands before executing them.
    set -x
fi

# Here lies rest of the script.
```

# Shellcheck

**TODO: Why? How? Why always `shell=bash`. How to disable checks.**

```Bash
#!/usr/bin/env bash

# shellcheck shell=bash

set -eo pipefail

if (( BASH_VERSINFO[0] > 4 || ( BASH_VERSINFO[0] == 4 && BASH_VERSINFO[1] >= 4 ) )); then
    # Treat unset variables and parameters as an error when expanding.  This
    # wasn't very reliable in older Bash versions, hence the version check.
    set -u

    # Command substitutions will inherits the value of the `set -e`, i.e.
    # `set -o errexit`.
    #
    # Available since Bash 4.4, this is actually something Bash does in POSIX
    # mode without it needed to be told.
    shopt -s inherit_errexit
fi

if [[ -n "${DEBUGGING_MODE:-}"  ]]; then
    # This will cause Bash to print commands before executing them.
    set -x
fi

# Here lies rest of the script.
```


# Sourcing vs. Executing

**TODO: What's the difference, describe examples.**

```Bash
# Source/include/load 'the-script' as a library:
source /path/to/the-script
```

```
# Calling 'the-script' as a normal executable using full path:
/path/to/the-script
```

```
# Calling 'the-script' as a normal executable using relative path:
./relative/path/to/the-script
```

```
# Handing over control to 'the-script', i.e. executing it:
exec /path/to/the-script
```

**TODO: Bash libraries are hard: Paths and figuring up where they are.**


## Why Not Both?

**TODO: Why this is not a good idea and why it is a good idea.**

```
#!/usr/bin/env bash

# shellcheck shell=bash

if [[ "${BASH_SOURCE[0]}" = "$0" ]]; then
    # This script is being executed, not sourced.  We can do proper setup here
    # which we would want to avoid doing it if sourced.  The script sourcing us
    # may need to do a different setup.

    # Command substitutions will inherits the value of the `set -e`, i.e.
    # `set -o errexit`.
    shopt -s inherit_errexit

    if [[ -n "${DEBUGGING_MODE:-}"  ]]; then
        # This will cause Bash to print commands before executing them.
        set -x
    fi
fi

# Here lies our library, er... script, no library... what?
```

# Variables are not Environment Variables

**TODO: What's the difference and why should I care?**

**TODO: Convention SOME_ENVIRONMENT\_VARIABLE, vs. someVariable and
some_variable.**


# Builtins

**TODO: What are builtins, why should I be interested in them, and where are
they documented?**

**TODO: Describe `builtin` builtin too.**


# When Bash Commands

**TODO: What is a command and how does Bash execute it.  Why is it important to
know?**


# Functions

**TODO: Start with few examples.**


## How To Name a Function

**TODO: What are the valid characters in a function; how is it resolved
compared to commands; how can I find what functions are defined.**


## Local Variables And Functions

**TODO: Introduce tricks with local variables, like emulating call-stack, maybe
not that, but similar.**


## `shift`-ing Through Arguments

**TODO**


## Function All The Things

Functions allow us to avoid global variables.  To leverage that to its maximum
we need `main` function.

```Bash
# ...

# Usage:
#
#   main [OPTIONS]
function main() {
    local -r someLocalVariable='has some read-only value'

    # The corpse^wbody of the function.
}

main "$@"
```

```Bash
# ...

# Usage:
#
#   main [OPTIONS]
function main() {
    local -r someLocalVariable='has some read-only value'

    # The corpse^wbody of the function.
}

# This allows us to use interactive Bash to test parts of our script.
if [[ "${BASH_SOURCE[0]}" = "$0" ]]; then
    main "$@"
fi
```

# User Experience


## Colors or NO_COLOR

**TODO: When colours, <https://no-color.org/>**


## Help Message

**TODO: How to 'printHelp'.  When stderr and when stdout.**

```Bash
# Print short help message to standard output.
#
# Usage:
#
#   printHelp COMMAND_NAME
#   printHelp COMMAND_NAME 1>&2
#
function printHelp() {
    # Using 'command' as a name of this variable can be tricky, since it's a
    # builtin Bash command.
    local -r cmd="$1"; shift

    cat <<EOF
Hereby I promise to describe this script one day.

Usage:

  ${cmd} [OPTIONS] [ARGUMENTS]

  ${cmd} {--help|-h}

Options:

  ...

  --help, -h
      Print this help message and exit.

Some other important things we should know.  Possibly few examples.
EOF
}
```


## Bash Completion

**TODO: This will be huge.**


# Environment Management

**TODO: What does it solve and why should I care about it?**


## Direnv

**TODO**


# Nix

**TODO**


# Direnv+Nix

**TODO**


# Bash Tips and Tricks, Mostly Tricks


## Parsing Options

### Using `getopts` Built-in

**TODO**

### Using `getopt`

**TODO**

### Manually Using `case`-statement

**TODO**


## Read NUL ('\0') Terminated Input Into an Array

Since Bash 4.4 `mapfile` provided `-d DELIMITER` option.  This is an example of
using it for `\0` delimited entries:

```Bash
mapfile -t -d '' < <(find . -print0)
```

## Delimiter, Separator, and Terminator

<https://stackoverflow.com/questions/9118769/when-to-use-the-terms-delimiter-terminator-and-separator>
