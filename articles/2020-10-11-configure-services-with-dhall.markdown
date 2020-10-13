---
title: Configure services with Dhall
updated: "2020-10-13"
---

For services, running inside a docker container, parsing command line options
may be an overkill. Adding an option every time we need a new configuration
value does feel like a lost effort. In addition, the contract between the
service and outside world is getting more and more complicated, increasing the
likelihood of misconfiguration.

What we can do about it? We can use a proper configuration language instead. In
many languages we can actually derive parsing and serialisation code using some
form of meta programming or generics making it the overhead even smaller.

A lot of services, these days, use YAML, JSON, or some INI-style configuration,
but we won't be talking about those. There's a lot of articles on the internet
on this topic and we don't need to duplicate it. Here we'll focus on services
written in Haskell that are using [Dhall configuration language
](https://dhall-lang.org/). Here are some of the things that are special when
it comes to this pairing:

* We can easily derive parsing of Dhall configuration even for complex types.
  This includes even functions.

* Dhall itself is typed and allows us to use more specific types. Giving us
  more precise parsing and error messages when attempting to start the service
  with an incorrect configuration.

  One of the biggest advantage of this approach, as opposed to the multiple
  options approach, is that we get all the errors at once instead of for the
  first option that is missing.

* Typed configuration language allows us to type check the configuration file
  before we even attempt to run the application in kind of a dry-run mode.

  Dhall is very good at this and will be able to tell us not only that types
  are mismatched, but also how. For example, it is able to tell as that we are
  missing or have extra fields in a record. This is particularly useful for
  spotting typos.

* Dhall's powerful import system allows us to be flexible and adapt to needs of
  our infrastructure much more easily. Most of the time it doesn't even require
  changing our service.

Some related articles:

* ["The Dhall configuration language" homepage](https://dhall-lang.org)

* [Dhall - References - Built-in types, functions, and operators: Imports
  ](https://docs.dhall-lang.org/references/Built-in-types.html#imports)


# Minimalistic Command-line Contract

While there are myriad of way this can be defined, the simplest of them that
can be easily used with containers is just providing `CONFIG` environment
variable:

``` {data-lang=usage}
CONFIG=EXPR EXECUTABLE
```

While this is certainly useful, it doesn't allow us to use the full power of
Haskell and Dhall pairing. Instead, we'll use the following command-line
contract for our `EXECUTABLE`s:

``` {data-lang=usage}
[CONFIG=EXPR] EXECUTABLE [--config=EXPR] [--typecheck]
EXECUTABLE --print-config-type
```

We have two ways of how we can pass the configuration value:

*   `CONFIG=EXPR`{.bash} environment variable where the value is a Dhall expression.
    For example simple server may need this:

    ``` {.bash data-lang=bash}
    export CONFIG='
      { api =
        { port = 8080
        , host = None Text
        }
      , healthApi =
        { port = 8090
        , host = None Text
        }
      }'
    ```

*   `--config=EXPR`{.bash} command line option. This is the same as passing it the
    configuration through environment variable, but it should be noted that if
    we allow server to support Dhall imports then following will work out of
    the box:

    -   `--config=FILE`{.bash} where `FILE` is an absolute file path or one
        starting with `./` or `~/`.  This allows us to handle passing files out
        of the box without needing to handle it explicitly or to provide
        another option like `--config-file=FILE`{.bash}

    -   `--config=env:ENV_VAR`{.bash} where `ENV_VAR` is a name of an
        environment variable. This actually allows us to use different
        environment variable than `CONFIG` if the situation requires it.

In addition to that we have two additional configuration options:

*   `--typecheck`{.bash} that allows us to run the `EXECUTABLE` in dry-run mode
    where we only parse and type-check the configuration.

    We can make this part of our build or deployment pipeline to verify that we
    hadn't forgotten to update the configuration before we attempt to start the
    service/container.

*   `--print-config-type`{.bash} is a very useful way of querying the
    `EXECUTABLE` it self to tell us what it's configuration looks like. There's
    nothing easier than running `EXECUTABLE --print-config-type`{.bash}, saving
    it into a file, and then writing the config file based on it.

Something that should be noted about the following calling convention:

``` {data-lang=usage}
[CONFIG=EXPR] EXECUTABLE [--config=EXPR] [--typecheck]
```

Is that at least either `CONFIG=EXPR`{.bash} or `--config=EXPR`{.bash} must be
supplied when running the service. This is to avoid having defaults in our
services. While it can be useful to start the service with test configuration,
it can easily lead to production misconfiguration.

Some related articles:

* [Dhall - References - Built-in types, functions, and operators: Imports
  ](https://docs.dhall-lang.org/references/Built-in-types.html#imports)

* [Command-line reference - docker run: Set environment variables (-e, \--env,
  \--env-file)
  ](https://docs.docker.com/engine/reference/commandline/run/#set-environment-variables--e---env---env-file)

* [Command-line reference - docker run: Mount volume (-v, --read-only)
  ](https://docs.docker.com/engine/reference/commandline/run/#mount-volume--v---read-only)

# Keep Your Secrets

The way secrets are exposed to a service running in a container can be
classified into following methods:

1.  Environment variables — secrets are exported as a set of environment
    variables available inside the container.

2.  Configuration files — secrets are written into configuration files that are
    then volume mounted into the container for the service to access.

3.  Secrets API — service has to retrieve it itself through an API that's
    usually some kind of key-value store with security of secrets in mind.

    This approach is not covered in this article, but it's easy to change our
    configuration definition to use something like (example is hypothetical and
    is written in Dhall):

    ``` {.dhall data-lang=dhall}
    let Secret = < Value : Text | Reference : Text >

    in  { database =
          { host = "database.example.com"
          , port = 5432
          , user = Secret.Reference "db-user"
          , password = Secret.Reference "db-password"
          }
        , api = { port = 8080, host = None Text }
        , healthApi = { port = 8090, host = None Text }
        }
    ```

    When the application loads the configuration file it knows that it needs to
    fetch the secrets through the secrets API using provided reference,
    probably a key to lookup in the key-value store. Other approaches are
    possible as well.

We can handle methods 1 and 2 in pure Dhall allowing us to decouple a lot of
infrastructure specific decisions from how services are built. In other words,
we don't need to change the service if we decide to rename an environment
variable or a configuration file. We can even switch between environment
variables and files without needing to touch the code. The only exception is
the third method, as this requires the application to understand the API for
retrieving secrets.

Some related articles:

* [Kubernetes Documentation - Concepts - Configuration - Secrets
  ](https://kubernetes.io/docs/concepts/configuration/secret/) — describes all
  the possible ways how secrets can be exposed to a pod (and container) when
  using Kubernetes.

* [Google Cloud - Solutions - Architectures - Best practices for operating
  containers: Ensure that your containers are stateless and immutable
  ](https://cloud.google.com/solutions/best-practices-for-operating-containers#ensure_that_your_containers_are_stateless_and_immutable)
  — describes best practices for accessing configuration in a container.

* [Command-line reference - docker run: Set environment variables (-e, \--env,
  \--env-file)
  ](https://docs.docker.com/engine/reference/commandline/run/#set-environment-variables--e---env---env-file)

* [Command-line reference - docker run: Mount volume (-v, --read-only)
  ](https://docs.docker.com/engine/reference/commandline/run/#mount-volume--v---read-only)


## Environment Variables

Dhall supports imports in the form `env:ENV_VAR` where `ENV_VAR` is a name of
an environment variable. What it allows us to do is:

``` {.dhall data-lang=dhall}
{ database =
  { host = "database.example.com"
  , port = 5432
  , user = env:DB_USER as Text
  , password = env:DB_PASSWORD as Text
  }
, api = { port = 8080, host = None Text }
, healthApi = { port = 8090, host = None Text }
}
```

Where `DB_USER` and `DB_PASSWORD` are environment variables containing secrets.

The `env:ENV_VAR as Text` means that the value of `ENV_VAR` will be treated as
is, instead of Dhall expression.

Some related articles:

* [Dhall - References - Built-in types, functions, and operators: Imports
  ](https://docs.dhall-lang.org/references/Built-in-types.html#imports)

* [Command-line reference - docker run: Set environment variables (-e, \--env,
  \--env-file)
  ](https://docs.docker.com/engine/reference/commandline/run/#set-environment-variables--e---env---env-file)


## Configuration Files

File paths are imports in Dhall the same way as `env:ENV_VAR` or an URL is.
This allows us to easily use the same syntax to access files:

``` {.dhall data-lang=dhall}
{ database =
  { host = "database.example.com"
  , port = 5432
  , user = /config/db-user as Text
  , password = /config/db-password as Text
  }
, api = { port = 8080, host = None Text }
, healthApi = { port = 8090, host = None Text }
}
```

Where `/config/db-user` and `/config/db-passowrd` are files volume mounted into
the container and they contain the secrets.

The `/file/path as Text` means that the contents of `/file/path` will be
imported as is, instead of Dhall expression.

Some related articles:

* [Dhall - References - Built-in types, functions, and operators: Imports
  ](https://docs.dhall-lang.org/references/Built-in-types.html#imports)

* [Command-line reference - docker run: Mount volume (-v, --read-only)
  ](https://docs.docker.com/engine/reference/commandline/run/#mount-volume--v---read-only)


# Example Implementation

Here we will present an example implementation that uses the well known
[`optparse-applicative`
](https://hackage.haskell.org/package/optparse-applicative) library. However,
this library doesn't handle environment variables on it's own and we'll have to
handle that part on ourselves.

In the presented example we are being very explicit, which includes
`NoImplicitPrelude` and usage of explicit import lists. This makes the imports
section more verbose, but better for readability in an article. When reading
the example it may be best to skip the module header and imports sections, at
first, and go back to them later.

The implementation is split into two modules `Main` and `Configuration`.  We'll
start with the `Main` (Haskell entry point) to set the stage:

``` {.haskell data-lang=haskell}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Description: An example of a service configured using Dhall
-- Copyright:   (c) 2020 Peter Trško
-- License:     BSD3
--
-- An example of a service executable configured using Dhall passed using
-- command-line options or an environment variable.
module Main
    ( main
    )
  where

import Data.Maybe (Maybe)
import Data.Semigroup ((<>))
import Data.String (String)
import Data.Word (Word16)
import GHC.Generics (Generic)
import System.IO (IO, print)
import Text.Show (Show)

import qualified Dhall
import qualified Options.Applicative as Options

import Configuration (getConfiguration)


data Listen = Listen
    { host :: Maybe String
    , port :: Word16
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.FromDhall)

data Database = Database
    { host :: String
    , port :: Word16
    , user :: String
    , password :: String
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.FromDhall)

data Config = Config
    { api :: Listen
    , healthApi :: Listen
    , database :: Database
    }
  deriving stock (Generic, Show)
  deriving anyclass (Dhall.FromDhall)

-- | Main entry point. Responsible for parsing command-line options and reading
-- environment variables to get configuration and then run the 'service'
-- itself.
main :: IO ()
main = do
    config <- getConfiguration Dhall.auto
        ( Options.fullDesc
        <> Options.header "Very useful info those that call the '--help'."
        )

    service config

-- | The serice code itself.
service :: Config -> IO ()
service = {- ... -}
```

Now the implementation of `getConfiguration` that allows us to be so brief in
the `Main` module:

``` {.haskell data-lang=haskell}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Description: An example of Dhall-centric access to configuration
-- Copyright:   (c) 2020 Peter Trško
-- License:     BSD3
--
-- An example of Dhall-centric access to configuration which is passed using
-- command-line options or an environment variable. In addition to that we
-- leverage Dhall-specific features like typechecking to provide the ability to
-- test configuration before running the service.
module Configuration
    ( getConfiguration
    )
  where

import Control.Applicative ((<**>), (<*>), (<|>), optional, pure)
import Control.Exception (throwIO)
import Control.Monad ((>>=))
import Data.Function ((.))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Semigroup ((<>))
import Data.String (fromString)
import System.Environment (lookupEnv)
import System.Exit (die, exitSuccess)
import System.IO (IO)

import qualified Data.Either.Validation as Validation
    ( Validation(Failure, Success)
    )
import Data.Text (Text)
import qualified Dhall
    ( Decoder(Decoder, expected)
    , input
    )
import qualified Options.Applicative as Options
    ( InfoMod
    , Parser
    , execParser
    , flag
    , flag'
    , help
    , helper
    , info
    , long
    , metavar
    , strOption
    )
import qualified Prettyprinter (line, pretty)
import qualified Prettyprinter.Render.Text as Prettyprinter (putDoc)


data Mode
    = Execute (Maybe Text)
    -- ^ We want to execute the application with the given configuration. If
    -- the configuration is 'Nothing' we need to read the environment variable.
    | Typecheck (Maybe Text)
    -- ^ We want to do a dry-run during which we only typecheck the
    -- configuration. If the configuration is 'Nothing' we need to read the
    -- environment variable.
    | PrintType
    -- ^ Instead of doing anything just print the type of the configuration the
    -- application expects to be given.

getConfiguration
    :: forall config
    .  Dhall.Decoder config
    -- ^ Dhall 'Dhall.Decoder' consists of parser and expected type. Dhall
    -- library provides one special 'Dhall.Decoder':
    --
    -- @
    -- 'Dhall.auto' :: 'Dhall.FromDhall' a => 'Dhall.Decoder' a
    -- @
    --
    -- Which allows us to use type class mechanism for deriving and combining
    -- parsers and is a good default in many cases.
    -> (forall a. Options.InfoMod a)
    -- ^ Allows options parser configuration including help message information
    -- and general parser behaviour. Reason for rank-2 type is that we don't
    -- want to expose the unnecessary internals of the parser we are using.
    -> IO config
getConfiguration decoder@Dhall.Decoder{expected} infoMod =
    parseOptions >>= \case
        Execute config ->
            parseConfig config

        Typecheck config -> do
            _ <- parseConfig config
            exitSuccess

        PrintType -> do
            printType
            exitSuccess
  where
    parseConfig :: Maybe Text -> IO config
    parseConfig possiblyExpr = do
        expr <- case possiblyExpr of
            Just expr ->
                pure expr

            Nothing ->
                lookupEnv "CONFIG" >>= maybe dieNoConfig (pure . fromString)

        Dhall.input decoder expr

    printType :: IO ()
    printType = case expected of
        Validation.Success expr ->
            Prettyprinter.putDoc
                ( Prettyprinter.pretty expr
                <> Prettyprinter.line
                )

        Validation.Failure err ->
            -- This indicates a bug in the Decoder.
            throwIO err

    dieNoConfig :: IO a
    dieNoConfig =
        die "Either `CONFIG=EXPR' environment variable or `--config=EXPR'\
            \ command-line option must be specified"

    parseOptions :: IO Mode
    parseOptions =
        Options.execParser (Options.info (options <**> Options.helper) infoMod)

    options :: Options.Parser Mode
    options = printTypeFlag <|> (typecheckFlag <*> optional configOption)

    configOption :: Options.Parser Text
    configOption = Options.strOption
        ( Options.long "config"
        <> Options.metavar "EXPR"
        <> Options.help "Set configration to EXPR, where EXPR is a Dhall\
            \ expression; if application fails to parse or typecheck the EXPR\
            \ it terminates with exit code 1"
        )

    typecheckFlag :: Options.Parser (Maybe Text -> Mode)
    typecheckFlag = Options.flag Execute Typecheck
        ( Options.long "typecheck"
        <> Options.help "Typecheck the configuration and exit; exit code 0 is\
            \ used on success and exit code 1 on failure to typecheck"
        )

    printTypeFlag :: Options.Parser Mode
    printTypeFlag = Options.flag' PrintType
        ( Options.long "print-config-type"
        <> Options.help "Print Dhall type of configuration accepted by the\
            \ application"
        )
```

The above code was tested and built with:

*   GHC 8.8.4

*   [Stackage LTS 16.17](https://www.stackage.org/lts-16.17) with following
    exceptions:

    - [prettyprinter-1.7.0](https://hackage.haskell.org/package/prettyprinter-1.7.0)
    - [dhall-1.35.0](https://hackage.haskell.org/package/dhall-1.35.0)
    - [repline-0.4.0.0](https://hackage.haskell.org/package/repline-0.4.0.0)
    - [haskeline-0.8.1.0](https://hackage.haskell.org/package/haskeline-0.8.1.0)

If we compile the above code (with some sensible definition for `service`) and
name it `example-service` then we get following help message:

``` {data-lang=console}
user@machine:~ $ ./example-service --help
Very useful info those that call the '--help'.

Usage: example-service [--print-config-type | [--typecheck] [--config EXPR]]

Available options:
  --print-config-type      Print Dhall type of configuration accepted by the
                           application
  --typecheck              Typecheck the configuration and exit; exit code 0 is
                           used on success and exit code 1 on failure to
                           typecheck
  --config EXPR            Set configration to EXPR, where EXPR is a Dhall
                           expression; if application fails to parse or
                           typecheck the EXPR it terminates with exit code 1
  -h,--help                Show this help text
```

If we don't supply a configuration we get:

``` {data-lang=console}
user@machine:~ $ ./example-service
Either `CONFIG=EXPR' environment variable or `--config=EXPR' command-line
option must be specified
```

And when we supply an incorrect configuration we get a pretty good idea what's
going on:

``` {data-lang=console}
user@machine:~ $ ./example-service --config='{=}'
example-service:
Error: Expression doesn't match annotation

{ - api : …
, - database : …
, - healthApi : …
}

1│ {=} : { api : { host : Optional Text, port : Natural }
2│ , healthApi : { host : Optional Text, port : Natural }
3│ , database : { host : Text, port : Natural, user : Text, password : Text }
4│ }

(input):1:1
```
