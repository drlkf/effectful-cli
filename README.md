# effectful-cli

Small helpers for writing CLIs with [effectful](https://hackage.haskell.org/package/effectful):
effect constraints, concurrency, prompts, sleep/time, optparse-applicative helpers and a
[monad-logger](https://hackage.haskell.org/package/monad-logger) bridge (stderr or OpenTelemetry).

## Quick start

Add the dependencies to `package.yaml`:

```yaml
dependencies:
  - effectful
  - effectful-cli
  - http-client-effectful
  - monad-logger
  - optparse-applicative
  - text
```

Minimal `Main.hs`, one `hello` subcommand logging to stderr:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Logger (logInfoN)
import Data.CLI.Class
import Data.CLI.Impl (execCLIParser, mkCommand, usage)
import Data.Error.CLI (CLIError, displayCLIError)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Effectful
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.HttpClient (runHttpClientTls)
import Effectful.Logger (runStderrLogger)
import Effectful.Reader.Static (runReader)
import Options.Applicative (hsubparser, strArgument)

data Config = Config

instance CLIConfig Config where
  autoAccept _ = False
  autoDecline _ = False

newtype Hello = Hello String

instance Command Config CLIError Hello where
  cmdname _ = "hello"
  doc _ = "greet someone"
  parser _ = Hello <$> strArgument mempty
  runner (Hello name) = logInfoN ("hello " <> T.pack name)

main :: IO ()
main = do
  cmd <- execCLIParser
         (usage "hello" "0.1.0" "greeter" (hsubparser (mkCommand (Hello ""))))
  res <-
    runEff .
    runHttpClientTls .
    runStderrLogger .
    runReader Config .
    runErrorNoCallStack @CLIError .
    runErrorNoCallStack @CLIError $
    runner cmd
  either (T.putStrLn . displayCLIError) pure (res >>= id)
```

```sh
stack run -- hello world
```

See the `Effectful.CLI` module docs for the walkthrough.

## Build

```sh
stack build --test
```

## Release

Requires [just](https://just.systems) and a Hackage token in `HACKAGE_KEY`.

```sh
just docs       # build Haddock tarball
just candidate  # upload package + docs as Hackage candidate
just release    # publish package + docs to Hackage
```
