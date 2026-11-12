{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.CLI.Impl (
  commandTree,
  fromParsec,
  plainHelpStyle,
  reversibleSwitch,
  usage,
  usageWith,
 )
import Data.Error.CLI (CLIError (..), displayCLIError)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Effectful (Eff, IOE, liftIO, runEff)
import Effectful.CLI.Conc (batchConcurrentlyWithIndex)
import Effectful.CLI.Config (
  getConfigKey,
  lookupKey,
  parseValue,
  setConfigKey,
  setKey,
  showConfig,
 )
import Effectful.CLI.Exception (rethrowIO)
import Effectful.CLI.IO (resolveIndex)
import Effectful.Concurrent (runConcurrent)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Options.Applicative
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import Test.Hspec
import Text.Megaparsec.Char (string)
import Toml (Table, Table' (..), Value' (..))
import Toml.Schema (
  FromValue (..),
  ToTable (..),
  ToValue (..),
  defaultTableToValue,
  optKey,
  parseTableFromValue,
  reqKey,
  reqKeyOf,
  table,
  (.=),
 )

isInputError :: Either CLIError a -> Bool
isInputError (Left (InputError _)) = True
isInputError _ = False

main :: IO ()
main = hspec $ do
  describe "batchConcurrentlyWithIndex" $ do
    let xs = "abcde"
        expected = zip [1 ..] xs
        run n = runEff . runConcurrent $ batchConcurrentlyWithIndex n (curry pure) xs

    it "pairs 1-based indices with elements" $
      run 10 `shouldReturn` expected

    it "works with a batch smaller than the list" $
      run 2 `shouldReturn` expected

  describe "rethrowIO"
    $ it "rethrows IOException as app error"
    $ runEff
      ( runErrorNoCallStack
          (rethrowIO show (liftIO (ioError (userError "boom")) :: Eff '[Error String, IOE] ()))
      )
      `shouldReturn` Left "user error (boom)"

  describe "resolveIndex" $ do
    let len = 4
    it "resolves positive bounds" $ do
      resolveIndex len 1 `shouldBe` Right 0
      resolveIndex len len `shouldBe` Right (len - 1)

    it "resolves negative bounds" $ do
      resolveIndex len (-1) `shouldBe` Right (len - 1)
      resolveIndex len (-len) `shouldBe` Right 0

    it "rejects out of bounds" $
      fmap (isInputError . resolveIndex len) [0, len + 1, -(len + 1)]
        `shouldBe` [True, True, True]

  describe "displayCLIError"
    $ it "renders every constructor"
    $ fmap
      displayCLIError
      [ InputError "a"
      , NotFoundError "b"
      , NotImplemented "c"
      , UserCanceled Nothing
      , UserCanceled (Just "d")
      ]
      `shouldBe` [ "input error: a"
                 , "not found: b"
                 , "not implemented: c"
                 , "user canceled"
                 , "user canceled: d"
                 ]

  describe "fromParsec" $ do
    let p = info (argument (fromParsec "word" (string "ok")) mempty) mempty
        parse = getParseResult . execParserPure defaultPrefs p

    it "accepts a valid input" $
      parse ["ok"] `shouldBe` Just "ok"

    it "rejects an invalid input" $
      parse ["ko"] `shouldBe` Nothing

  describe "reversibleSwitch"
    $ it "uses the default or the last occurrence"
    $ let p = info (reversibleSwitch True "labels" "show labels") mempty
          parse = getParseResult . execParserPure defaultPrefs p
       in fmap parse [[], ["--no-labels"], ["--no-labels", "--labels"], ["--labels", "--no-labels"]]
            `shouldBe` fmap Just [True, False, True, False]

  describe "usage"
    $ it "styles the header unless plain"
    $ let render st = case execParserPure defaultPrefs (st "prog" "1.0" "desc" (pure ())) ["--help"] of
            Failure f -> fst (renderFailure f "prog")
            _ -> ""
       in ( "\ESC[" `isInfixOf` render usage
          , "\ESC[" `isInfixOf` render (usageWith plainHelpStyle)
          , "prog - desc - version 1.0" `isInfixOf` render (usageWith plainHelpStyle)
          )
            `shouldBe` (True, False, True)

  describe "commandTree"
    $ it "renders nested and hidden commands"
    $ let leaf n = command n (info (pure ()) (progDesc (n <> " desc")))
          p =
            subparser (leaf "a")
              <|> subparser (hidden <> command "b" (info (subparser (leaf "c")) (progDesc "b desc")))
       in show (commandTree p)
            `shouldBe` "a a desc\nb b desc\n  c c desc\n"

  describe "config keys" $ do
    it "parses TOML literals, falling back to strings" $
      fmap parseValue ["true", "\"x\"", "plain"]
        `shouldBe` [Bool True, Text "x", Text "plain"]

    it "sets nested keys and reads them back" $
      ( lookupKey ["a", "b", "c"]
          =<< either (const Nothing) Just (setKey ["a", "b", "c"] (Text "t") (MkTable mempty))
      )
        `shouldBe` Just (Text "t")

    it "refuses to descend into a scalar" $
      (setKey ["a", "b"] (Bool True) =<< setKey ["a"] (Bool False) (MkTable mempty))
        `shouldBe` Left "a is not a table"

    it "writes then reads a key through the file" $ do
      path <- (</> "effectful-cli-config-test.toml") <$> getTemporaryDirectory
      r <- runEff . runErrorNoCallStack @CLIError $ do
        setConfigKey @Table [] path "a.b" "1"
        getConfigKey path "a.b"
      removeFile path
      r `shouldBe` Right "1"

    it "hints at known keys when a section is set to a scalar" $ do
      path <- (</> "effectful-cli-config-hint-test.toml") <$> getTemporaryDirectory
      r <-
        runEff . runErrorNoCallStack @CLIError $
          setConfigKey @Section ["sec.x", "sec.y", "other.z"] path "sec" "plain"
      r `shouldSatisfy` \case
        Left (InputError m) -> "sec is a section, set one of: sec.x, sec.y" `isInfixOf` show m
        _ -> False

    it "shows defaults for a missing file and file values over them" $ do
      dir <- getTemporaryDirectory
      let missing = dir </> "effectful-cli-config-missing.toml"
          present = dir </> "effectful-cli-config-show.toml"
      writeFile present "n = 3\n"
      r <-
        runEff . runErrorNoCallStack @CLIError $
          (,) <$> showConfig @Defaulted missing <*> showConfig @Defaulted present
      removeFile present
      r `shouldBe` Right ("n = 7", "n = 3")

newtype Defaulted = Defaulted Int

instance FromValue Defaulted where
  fromValue = parseTableFromValue (Defaulted . fromMaybe 7 <$> optKey "n")

instance ToValue Defaulted where
  toValue = defaultTableToValue

instance ToTable Defaulted where
  toTable (Defaulted n) = table ["n" .= n]

newtype Section = Section Int

instance FromValue Section where
  fromValue = parseTableFromValue (Section <$> reqKeyOf "sec" (parseTableFromValue (reqKey "x")))
