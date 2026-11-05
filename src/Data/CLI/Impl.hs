{-# LANGUAGE OverloadedStrings #-}

-- | Helpers for building optparse-applicative parsers and subcommands.
module Data.CLI.Impl (
  unboundEnumError,
  fromParsec,
  argOrOpt,
  noArgs,
  forceSwitch,
  noopSwitch,
  reversibleSwitch,
  uuidArg,
  mkCommand,
  mkCommands,
  usage,
  usageWith,
  usageTreeWith,
  commandTree,
  HelpStyle (..),
  defaultHelpStyle,
  plainHelpStyle,
  styled,
  colorEnabled,
  execCLIParser,
) where

import Data.Bifunctor (first)
import Data.CLI.Class (Command (..))
import Data.Char (toUpper)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.Void (Void)
import Options.Applicative
import Options.Applicative.Common (mapParser)
import Options.Applicative.Help.Chunk (unChunk)
import Options.Applicative.Help.Pretty (
  AnsiStyle,
  Color (..),
  Doc,
  align,
  annotate,
  bold,
  color,
  fill,
  indent,
  line,
  pretty,
  unAnnotate,
  underlined,
  vsep,
  (<+>),
 )
import Options.Applicative.Types (OptReader (..), Option (..))
import System.Environment (getArgs, lookupEnv)
import System.IO (hIsTerminalDevice, stderr, stdout)
import Text.Megaparsec (Parsec, eof, errorBundlePretty, runParser)
import Text.Printf (printf)

-- | ANSI styles applied to help texts.
--
-- @since 0.2.2.0
data HelpStyle = HelpStyle
  { _styleName :: AnsiStyle
  -- ^ program name in the header
  , _styleVersion :: AnsiStyle
  -- ^ version in the header
  , _styleValue :: AnsiStyle
  -- ^ accepted values and option references
  , _styleWarning :: AnsiStyle
  -- ^ dangerous behavior
  }

-- | Bold underlined name, green version, cyan values, bold red warnings.
--
-- @since 0.2.2.0
defaultHelpStyle :: HelpStyle
defaultHelpStyle = HelpStyle (bold <> underlined) (color Green) (color Cyan) (bold <> color Red)

-- | No styling.
--
-- @since 0.2.2.0
plainHelpStyle :: HelpStyle
plainHelpStyle = HelpStyle mempty mempty mempty mempty

-- | Render a string with the selected style of a t'HelpStyle'.
--
-- @since 0.2.2.0
styled
  :: (HelpStyle -> AnsiStyle)
  -- ^ style selector, e.g. '_styleValue'
  -> HelpStyle
  -- ^ style set to select from
  -> String
  -- ^ text to render
  -> Doc
styled f s
  | f s == mempty = pretty
  | otherwise = annotate (f s) . pretty

-- | Whether colors should be emitted: stdout and stderr are terminals and @NO_COLOR@ is unset or empty.
--
-- @since 0.2.2.0
colorEnabled :: IO Bool
colorEnabled = do
  noColor <- maybe False (not . null) <$> lookupEnv "NO_COLOR"
  ttys <- and <$> traverse hIsTerminalDevice [stdout, stderr]
  pure (ttys && not noColor)

-- | Like 'execParser', printing help when run without arguments and stripping help styles when
-- 'colorEnabled' is false.
--
-- @since 0.2.2.0
execCLIParser
  :: ParserInfo a
  -- ^ top-level parser, e.g. from 'usage'
  -> IO a
execCLIParser pinfo = do
  c <- colorEnabled
  res <- execParserPure (prefs showHelpOnEmpty) pinfo <$> getArgs
  handleParseResult (if c then res else strip res)
 where
  strip (Failure (ParserFailure f)) = Failure (ParserFailure (\p -> let (h, e, w) = f p in (plain h, e, w)))
  strip r = r
  plain (ParserHelp a b c d e f g h) = ParserHelp (u a) (u b) (u c) (u d) (u e) (u f) (u g) (u h)
  u = fmap unAnnotate

-- | Error for 'succ' called on 'maxBound' of the named enum.
--
-- @since 0.1.0.0
unboundEnumError
  :: String
  -- ^ enum type name
  -> a
unboundEnumError name = error (printf "%s.Enum: succ invoked on maxBound" name)

-- | Turn a megaparsec parser into a 'ReadM', requiring the whole input to be consumed.
--
-- @since 0.1.0.0
fromParsec
  :: String
  -- ^ input name used in parse error messages
  -> Parsec Void String a
  -- ^ parser for the whole value
  -> ReadM a
fromParsec name p = eitherReader (first errorBundlePretty . runParser (p <* eof) name)

-- | Accept a value either as a positional argument or as a @-c@/@--name@ option.
--
-- @since 0.1.0.0
argOrOpt
  :: String
  -- ^ long option name, also the uppercased metavar
  -> Char
  -- ^ short option
  -> String
  -- ^ help text
  -> Either (Parsec Void String a) (ReadM a)
  -- ^ value reader, as a megaparsec parser or a 'ReadM'
  -> [String]
  -- ^ shell completion candidates
  -> Parser a
argOrOpt name c h p cs =
  argument r (metavar var <> completeWith cs)
    <|> option r (short c <> long name <> help h <> metavar var <> completeWith cs)
 where
  var = fmap toUpper name
  r = either (fromParsec name) id p

-- | Parser for a command that takes no arguments.
--
-- @since 0.1.0.0
noArgs
  :: a
  -- ^ command value
  -> Parser a
noArgs = pure

-- | @-f@/@--force@ switch.
--
-- @since 0.1.0.0
forceSwitch :: Parser Bool
forceSwitch =
  switch
    ( short 'f'
        <> long "force"
        <> helpDoc (Just ("force" <+> styled _styleWarning defaultHelpStyle "unsafe" <+> "behavior"))
    )

-- | @-n@/@--noop@ switch for dry runs.
--
-- @since 0.1.0.0
noopSwitch :: Parser Bool
noopSwitch = switch (short 'n' <> long "noop" <> help "do not perform any real action")

-- | @--name@/@--no-name@ switch pair; the last occurrence wins, otherwise the default.
--
-- @since 0.2.2.0
reversibleSwitch
  :: Bool
  -- ^ default when neither switch is given
  -> String
  -- ^ long switch name, @no-@ prefixed for the negation
  -> String
  -- ^ help text
  -> Parser Bool
reversibleSwitch def name h =
  last
    <$> some (flag' True (long name <> help h) <|> flag' False (long ("no-" <> name) <> hidden))
      <|> pure def

-- | UUID given as a positional argument or @-u@/@--uuid@.
--
-- @since 0.1.0.0
uuidArg :: Parser UUID
uuidArg = argOrOpt "uuid" 'u' "target uuid" (Right (maybeReader UUID.fromString)) []

-- | Build a subcommand from a 'Command' instance.
--
-- @since 0.1.0.0
mkCommand
  :: Command c e a
  => a
  -- ^ command value
  -> Mod CommandFields a
mkCommand a = command (cmdname a) (info (parser a <**> helper) (progDesc (doc a)))

-- | Build subcommands for every value of an enum, sorted by name and wrapped with the given constructor.
--
-- @since 0.1.0.0
mkCommands
  :: Bounded a
  => Command c e a
  => Command c e b
  => Enum a
  => (a -> b)
  -- ^ wrapping constructor
  -> Mod CommandFields b
mkCommands f = foldMap (mkCommand . f) (sortOn cmdname [minBound ..])

-- | Top-level t'ParserInfo' with @--help@ and a header showing name, description and version.
--
-- @since 0.1.0.0
usage
  :: String
  -- ^ program name
  -> String
  -- ^ version
  -> String
  -- ^ description
  -> Parser a
  -- ^ arguments parser
  -> ParserInfo a
usage = usageWith defaultHelpStyle

-- | 'usage' with a custom t'HelpStyle'.
--
-- @since 0.2.2.0
usageWith
  :: HelpStyle
  -- ^ help style
  -> String
  -- ^ program name
  -> String
  -- ^ version
  -> String
  -- ^ description
  -> Parser a
  -- ^ arguments parser
  -> ParserInfo a
usageWith s name version hdr p =
  info
    (p <**> helper)
    ( fullDesc
        <> progDesc hdr
        <> headerDoc
          ( Just
              ( styled _styleName s name
                  <+> "-"
                  <+> pretty hdr
                  <+> "- version"
                  <+> styled _styleVersion s version
              )
          )
    )

-- | 'usageWith' with the full 'commandTree' as footer.
usageTreeWith
  :: HelpStyle
  -- ^ help style
  -> String
  -- ^ program name
  -> String
  -- ^ version
  -> String
  -- ^ description
  -> Parser a
  -- ^ arguments parser
  -> ParserInfo a
usageTreeWith s name version hdr p =
  (usageWith s name version hdr p)
    { infoFooter = pure (vsep ["Command tree:", indent 2 (commandTree p)])
    }

-- | Every command of a parser, hidden ones included, with nested subcommands indented under their
-- parent.
commandTree :: Parser a -> Doc
commandTree = vsep . commands
 where
  commands :: Parser x -> [Doc]
  commands = concat . mapParser (const entries)
  entries :: Option x -> [Doc]
  entries o = case optMain o of
    CmdReader _ cs -> uncurry entry <$> cs
    _ -> []
  entry :: String -> ParserInfo x -> Doc
  entry n i =
    fill 12 (pretty n) <+> align (fromMaybe mempty (unChunk (infoProgDesc i))) <> case commands (infoParser i) of
      [] -> mempty
      sub -> line <> indent 2 (vsep sub)
