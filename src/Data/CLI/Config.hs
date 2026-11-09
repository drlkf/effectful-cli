{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Ready-made @show@/@get@/@set@ commands over the application's TOML config file.
module Data.CLI.Config (
  ConfigFile (..),
  ConfigCommand (..),
) where

import Data.CLI.Class (Command (..))
import Data.CLI.Impl (noArgs, unboundEnumError)
import Data.Error.CLI (CLIError (InputError))
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful.CLI.Config (getConfigKey, setConfigKey, showConfig)
import Effectful.CLI.Exception (rethrowIO)
import Effectful.CLI.IO (printOutputLn)
import Effectful.Reader.Static (asks)
import GHC.Enum (boundedEnumFrom, boundedEnumFromThen)
import Options.Applicative (Parser, completer, listCompleter, metavar, strArgument)
import Toml.Schema (FromValue, ToTable)

-- | Application config @c@ that knows its config file and the type it decodes to.
--
-- 'ToTable' is needed by 'ConfigShow'; defaults come from the 'FromValue'
-- instance filling missing keys.
--
-- @since 0.2.6.0
class (FromValue (ConfigOf c), ToTable (ConfigOf c)) => ConfigFile c where
  -- | Type the config file decodes to; 'ConfigSet' refuses to write anything else.
  --
  -- @since 0.2.8.0
  type ConfigOf c :: Type

  -- | Path of the config file.
  configFilePath :: c -> FilePath

  -- | Known dotted keys, used for shell completion and for hinting when a
  -- section key is set to a scalar.
  configKeys :: [Text]
  configKeys = []

-- | Config file commands for config @c@ and application error @e@. IO failures
-- are raised as 'InputError'.
--
-- @since 0.2.6.0
data ConfigCommand c e
  = -- | Print the effective config: file values over the defaults.
    ConfigShow
  | -- | Print a dotted key's value.
    ConfigGet Text
  | -- | Set a dotted key to a TOML literal, or a string if not one.
    ConfigSet Text Text
  deriving (Eq, Show)

instance Bounded (ConfigCommand c e) where
  minBound = ConfigShow
  maxBound = ConfigSet mempty mempty

instance Enum (ConfigCommand c e) where
  fromEnum ConfigShow = 0
  fromEnum (ConfigGet _) = 1
  fromEnum (ConfigSet _ _) = 2

  toEnum 0 = minBound
  toEnum 1 = ConfigGet mempty
  toEnum 2 = maxBound
  toEnum _ = unboundEnumError "ConfigCommand"

  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance ConfigFile c => Command c e (ConfigCommand c e) where
  cmdname ConfigShow = "show"
  cmdname (ConfigGet _) = "get"
  cmdname (ConfigSet _ _) = "set"

  doc ConfigShow = "Print the effective config (file merged over defaults)"
  doc (ConfigGet _) = "Print a dotted key's value (e.g. section.key)"
  doc (ConfigSet _ _) = "Set a dotted key to a TOML literal, or a string if not one"

  parser ConfigShow = noArgs ConfigShow
  parser (ConfigGet _) = ConfigGet <$> keyArg (configKeys @c)
  parser (ConfigSet _ _) = ConfigSet <$> keyArg (configKeys @c) <*> strArgument (metavar "VALUE")

  runner cmd = do
    path <- asks @c configFilePath
    rethrowIO (InputError . ("config file: " <>) . T.show) $ case cmd of
      ConfigShow -> printOutputLn =<< showConfig @(ConfigOf c) path
      ConfigGet k -> printOutputLn =<< getConfigKey path k
      ConfigSet k v -> setConfigKey @(ConfigOf c) (configKeys @c) path k v

keyArg
  :: [Text]
  -> Parser Text
keyArg keys =
  strArgument (metavar "KEY" <> completer (listCompleter (fmap T.unpack keys)))
