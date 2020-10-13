---
title: Notes on Command Line Interface (CLI) Design
---

# Problem

There is a lot of command line tools out there, and not all of them have good
ergonomics.  That makes using them hard, or at least annoying when one needs to
use them.  People use various ways how to deal with that, shell aliases,
wrapper scripts, etc.  Wouldn't it be better to design CLI with better UX from
the start?

These are very opinionated guidelines for creating command line tools with a
good UX.  Be warned, this is still a work in progress.


# Intuitive vs. Easy vs. Simple vs. Consistent

TODO:
*   Intuitive — resembles what we already know.
*   Easy — does what we want without too much investment, doesn't care about
    scalability, maintainability, cleanness.
*   Simple — conceptually simple, understandable, doesn't imply easy or no
    learning curve.  Simple may even have steep learning curve, like maths.
*   Consistent — modularity, clean semantics, reliable semantics.


# Documentation

Three types of documentation should be provided:

1.  Project documentation, which should answer following:

    * What does this thing actually do?
    * Why should I care?
    * How do I install it and what dependencies does it have?
    * How can I interact with the project?

2.  Embedded help message that is printed to terminal.

3.  On-line documentation, manual page, or both.


## Project Documentation

TODO: What? Why? Where?  Is `README.md` on GitHub enough?


## Embedded Help

Embedded help message should be printed when `--help` or `-h` are invoked.
While the later is sometimes used for example as `-h HOSTNAME` it is not the
common case and a lot of people get confused when `-h` is not short for
`--help`.  Some commands do not support GNU `--long-option` style, which makes
supporting `-h` even more important.

If subcommands are supported by the command line tool i.e. something like:

```
some-tool [OPTIONS] SUBCOMMAND [SUBCOMMAND_ARGS]
```

Then `help` subcommand should be available as well.  Invoking `some-tool help`
should give the same result as `some-tool --help` or `some-tool -h`.

When help messages is requested by the user, i.e. it's not part of an error
message, then it should be printed to standard output.  When it's printed as
part of error message then the error message and help message should be printed
to standard error output.  Reason for this is if you don't support pagination
of `--help` output users can easily do:

```bash
some-tool --help | less
```

If help message is printed to standard error output this becomes trial and
error process where users will try the above first and only if they know enough
shell hackery they'll redirect `stderr` with `stdout`.  For example in Bash
that would look like:

```bash
some-tool --help 2>&1 | less
```


## Manual Page

This may be quite controversial idea, nevertheless, here we go.

TODO:

*   Describe manual a page, and why it is a good idea to have one.
*   Generating manual pages from e.g. Markdown.


# Colours

Output should use these colours:

*   Red for errors
*   Green for success
*   Yellow for warnings

TODO:

*   Describe `--colo[u]r={always|auto|never}` and how it should affect the
    output.

*   Does the whole message has to be highlighted or just the important bits?


# Signal-to-Noise Ratio

TODO: How much output should we print? Verbosity options?


# Unicode and Emoji

TODO: When is it a good idea to use Unicode/emojis?


# Machine Readable Output

TODO: Why would I want this? Emphasise the contrast with previous (Unicode and
Emoji).


# Scriptability

TODO: What does it mean?


# Configuration File

TODO: Describe how configuration files should be applied, where they should be
stored, and what formats are good and which should be avoided.


# Command Line Options

TODO:

* What other command line options should be supported, why, and how they should
  be applied.
* When to use subcommands and when to use options.


# Shell Completion

TODO: Talk about why shell completion is necessary part of good CLI, how to
implement it, and list some good resources.


# Environment Variables

TODO: Describe what standard environment variables should be respected, and how
they should be applied.


# Follow Existing Patterns

TODO: How not to deviate from user expectations.  Possible pitfalls.


# Localisation and Internationalisation

TODO: When is it a good idea to localise.  What the heck is the difference
between these two terms?  <https://www.w3.org/International/questions/qa-i18n>


# Related Articles

* [UX for Command Line Tools by Amodu Temitope on 11 December 2017
  ](https://blog.prototypr.io/ux-for-command-line-tools-4630eb0b3c9b)


<!--
  vim: ft=markdown spell
-->
