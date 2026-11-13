{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Terminal output, prompts and confirmations.
module Effectful.CLI.IO (
  printOutput,
  printOutputLn,
  printError,
  printLines,
  prompt,
  prompt',
  askForConfirm,
  withConfirm,
  autoOpen,
  chooseItem,
  resolveIndex,
) where

import Control.Monad (unless, void, when)
import Data.CLI.Class (CLIConfig (..))
import Data.Error.CLI (CLIError (..))
import Data.Foldable (toList)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Void (Void)
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.Process (Process, callProcess)
import Effectful.Reader.Static (Reader, ask)
import System.IO (hFlush, stderr)
import System.Info (os)
import Text.Layout.Table (columnHeaderTableS, def, rowsG, tableString, titlesH, unicodeS)
import Text.Megaparsec (Parsec, eof, errorBundlePretty, runParser, (<|>))
import Text.Megaparsec.Char (string')
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Text.Printf (printf)

-- | Print to stdout without a trailing newline.
--
-- @since 0.1.0.0
printOutput
  :: IOE :> es
  => Text
  -- ^ text to print
  -> Eff es ()
printOutput = liftIO . T.putStr

-- | Print to stdout with a trailing newline.
--
-- @since 0.1.0.0
printOutputLn
  :: IOE :> es
  => Text
  -- ^ text to print
  -> Eff es ()
printOutputLn = liftIO . T.putStrLn

-- | Print to stderr without a trailing newline.
--
-- @since 0.1.0.0
printError
  :: IOE :> es
  => Text
  -- ^ text to print
  -> Eff es ()
printError = liftIO . T.hPutStr stderr

-- | Print lines to stdout, each followed by a newline.
--
-- @since 0.1.0.0
printLines
  :: IOE :> es
  => Foldable f
  => f Text
  -- ^ lines to print
  -> Eff es ()
printLines = printOutput . T.unlines . toList

-- | Read a line from stdin after printing an optional prompt to stderr.
--
-- @since 0.1.0.0
prompt
  :: IOE :> es
  => Maybe Text
  -- ^ prompt text, followed by @> @
  -> Eff es Text
prompt p = liftIO $ do
  T.hPutStr stderr (fromMaybe "" p <> "> ")
  hFlush stderr
  T.getLine

-- | Prompt and parse the answer, throwing 'InputError' on parse failure.
--
-- @since 0.1.0.0
prompt'
  :: Error CLIError :> es
  => IOE :> es
  => Maybe Text
  -- ^ prompt text
  -> Parsec Void Text a
  -- ^ answer parser
  -> Eff es a
prompt' p parser =
  prompt p
    >>= either (throwError . InputError . T.pack . errorBundlePretty) pure
      . runParser parser "prompt"

-- | Ask for confirmation; @y@, @yes@ or an empty answer mean yes.
--
-- @since 0.1.0.0
askForConfirm :: Error CLIError :> es => IOE :> es => Eff es Bool
askForConfirm = prompt' (Just "proceed ?") yesOrNo
 where
  yes = void (string' "y" <|> string' "yes") <|> eof
  yesOrNo = (yes $> True) <|> pure False

-- | Run the action after user confirmation, honoring the config's auto-accept and
-- auto-decline flags; throws 'UserCanceled' when declined.
--
-- Ambiguous in @c@: call as @withConfirm @MyConfig@.
--
-- @since 0.1.0.0
withConfirm
  :: forall c es a
   . CLIConfig c
  => Error CLIError :> es
  => IOE :> es
  => Reader c :> es
  => Eff es a
  -- ^ action to confirm
  -> Eff es a
withConfirm action = do
  c <- ask @c
  when (autoDecline c) $ throwError (UserCanceled (Just "auto-declined"))
  unless (autoAccept c) $ do
    ok <- askForConfirm
    unless ok $ throwError (UserCanceled Nothing)
  action

-- | Open a file or URL with @open@ on macOS, @xdg-open@ elsewhere.
--
-- @since 0.1.0.0
autoOpen
  :: Process :> es
  => String
  -- ^ file path or URL
  -> Eff es ()
autoOpen arg = callProcess (if os == "darwin" then "open" else "xdg-open") [arg]

-- | Let the user pick one item from a table; a single item is returned without prompting, none throws 'NotFoundError'.
--
-- @since 0.1.0.0
chooseItem
  :: Error CLIError :> es
  => IOE :> es
  => Foldable f
  => Text
  -- ^ item kind, used as prompt and in the not-found error
  -> [Text]
  -- ^ table headers
  -> (a -> [Text])
  -- ^ table row of an item
  -> f a
  -- ^ items to choose from
  -> Eff es a
chooseItem name headers row = go . toList
 where
  go [] = throwError (NotFoundError ("no item found for " <> name))
  go [x] = pure x
  go xs = pick xs
  pick xs = do
    let hs = "#" : fmap T.unpack headers
        rows = zipWith (\i x -> show i : fmap T.unpack (row x)) [1 :: Int ..] xs
    printOutputLn . T.pack $
      tableString (columnHeaderTableS (fmap (const def) hs) unicodeS (titlesH hs) [rowsG rows])
    i <- prompt' (Just name) (signed (pure ()) decimal)
    either throwError (pure . (xs !!)) (resolveIndex (length xs) i)

-- | Turn a 1-based (or negative, from the end) user index into a 0-based one.
--
-- @since 0.1.0.0
resolveIndex
  :: Int
  -- ^ number of items
  -> Int
  -- ^ user index
  -> Either CLIError Int
resolveIndex len i
  | i >= 1 && i <= len = Right (i - 1)
  | i <= -1 && i >= -len = Right (len + i)
  | otherwise = Left (InputError (T.pack $ printf "index %d out of bounds" i))
