{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Read and edit a TOML config file by dotted key (@github.project.team@).
module Effectful.CLI.Config (
  readConfigText,
  getConfigKey,
  setConfigKey,
  lookupKey,
  setKey,
  parseValue,
) where

import Control.Monad ((<=<))
import Data.Error.CLI (CLIError (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Error.Static (Error, throwError)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import Toml (
  Result (..),
  Table,
  Table' (..),
  Value,
  Value' (..),
  forgetTableAnns,
  prettyToml,
 )
import Toml qualified
import Toml.Pretty (prettyValue)
import Toml.Schema (FromValue)

-- | The config file's contents, empty if it does not exist.
--
-- @since 0.2.6.0
readConfigText
  :: IOE :> es
  => FilePath
  -> Eff es Text
readConfigText path = liftIO $ do
  exists <- doesFileExist path
  if exists then T.readFile path else pure ""

-- | Render a dotted key's value: strings unquoted, tables as TOML sections.
--
-- @since 0.2.6.0
getConfigKey
  :: IOE :> es
  => Error CLIError :> es
  => FilePath
  -> Text
  -> Eff es Text
getConfigKey path key =
  maybe (throwError (NotFoundError key)) (pure . render) . lookupKey (T.splitOn "." key)
    =<< readTable path
 where
  render (Text t) = t
  render (Table t) = T.stripEnd (T.show (prettyToml t))
  render v = T.show (prettyValue v)

-- | Set a dotted key to 'parseValue' of the raw input, creating intermediate
-- tables; the file is only written if the result still decodes as @a@.
-- Re-renders the whole file, dropping comments and formatting.
--
-- When decoding fails and @key@ is a section of one of the known keys, the
-- error says which keys to set instead.
--
-- @since 0.2.6.0
--
-- The known-keys argument was added in 0.2.7.0.
setConfigKey
  :: forall a es
   . FromValue a
  => IOE :> es
  => Error CLIError :> es
  => [Text]
  -> FilePath
  -> Text
  -> Text
  -> Eff es ()
setConfigKey known path key raw = do
  table <-
    either (throwError . InputError) pure . setKey (T.splitOn "." key) (parseValue raw)
      =<< readTable path
  let rendered = T.show (prettyToml table)
  case Toml.decode @a rendered of
    Failure es ->
      throwError
        (InputError ("invalid config: " <> T.stripEnd (T.pack (unlines es)) <> sectionHint))
    Success _ _ -> liftIO $ do
      createDirectoryIfMissing True (takeDirectory path)
      T.writeFile path rendered
 where
  sectionHint = case filter (T.isPrefixOf (key <> ".")) known of
    [] -> ""
    ks -> "; " <> key <> " is a section, set one of: " <> T.intercalate ", " ks

readTable
  :: IOE :> es
  => Error CLIError :> es
  => FilePath
  -> Eff es Table
readTable =
  either (throwError . InputError . T.pack) (pure . forgetTableAnns) . Toml.parse
    <=< readConfigText

-- | Value at a dotted key path, the empty path being the whole table.
--
-- @since 0.2.6.0
lookupKey
  :: [Text]
  -> Table
  -> Maybe Value
lookupKey [] t = Just (Table t)
lookupKey (k : ks) (MkTable m) = case (ks, snd <$> Map.lookup k m) of
  ([], v) -> v
  (_, Just (Table t)) -> lookupKey ks t
  _ -> Nothing

-- | Set a value at a dotted key path, creating missing tables; fails when a
-- path segment is not a table.
--
-- @since 0.2.6.0
setKey
  :: [Text]
  -> Value
  -> Table
  -> Either Text Table
setKey [] _ _ = Left "empty key"
setKey [k] v (MkTable m) = Right (MkTable (Map.insert k ((), v) m))
setKey (k : ks) v (MkTable m) = case snd <$> Map.lookup k m of
  Nothing -> descend (MkTable mempty)
  Just (Table t) -> descend t
  Just _ -> Left (k <> " is not a table")
 where
  descend t = (\t' -> MkTable (Map.insert k ((), Table t') m)) <$> setKey ks v t

-- | A TOML literal (@true@, @42@, @["a"]@, @"x"@), else the raw text as a string.
--
-- @since 0.2.6.0
parseValue :: Text -> Value
parseValue raw =
  either
    (const (Text raw))
    (fromMaybe (Text raw) . lookupKey ["v"] . forgetTableAnns)
    (Toml.parse ("v = " <> raw))
