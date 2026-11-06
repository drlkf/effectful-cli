-- | Effect stack available to command runners.
--
-- = Tutorial
--
-- Define a config implementing 'Data.CLI.Class.CLIConfig' and a command implementing
-- 'Data.CLI.Class.Command', then run its 'Data.CLI.Class.runner' with every effect of
-- 'CLIEffects' discharged:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Data.CLI.Class
-- > import Data.CLI.Impl (execCLIParser, mkCommand, usage)
-- > import Data.Error.CLI (CLIError, displayCLIError)
-- > import Control.Monad.Logger (logInfoN)
-- > import Data.Text qualified as T
-- > import Data.Text.IO qualified as T
-- > import Effectful
-- > import Effectful.Error.Static (runErrorNoCallStack)
-- > import Effectful.HttpClient (runHttpClientTls)
-- > import Effectful.Logger (runStderrLogger)
-- > import Effectful.Reader.Static (runReader)
-- > import Options.Applicative (hsubparser, strArgument)
-- >
-- > data Config = Config
-- >
-- > instance CLIConfig Config where
-- >   autoAccept _ = False
-- >   autoDecline _ = False
-- >
-- > newtype Hello = Hello String
-- >
-- > instance Command Config CLIError Hello where
-- >   cmdname _ = "hello"
-- >   doc _ = "greet someone"
-- >   parser _ = Hello <$> strArgument mempty
-- >   runner (Hello name) = logInfoN ("hello " <> T.pack name)
-- >
-- > main :: IO ()
-- > main = do
-- >   cmd <- execCLIParser (usage "hello" "0.1.0" "greeter" (hsubparser (mkCommand (Hello ""))))
-- >   res <-
-- >     runEff . runHttpClientTls . runStderrLogger . runReader Config
-- >       . runErrorNoCallStack @CLIError . runErrorNoCallStack @CLIError
-- >       $ runner cmd
-- >   either (T.putStrLn . displayCLIError) pure (res >>= id)
--
-- Here the application error @e@ is 'CLIError' itself; use your own type and handle it
-- in the second 'Effectful.Error.Static.runErrorNoCallStack'. See "Data.CLI.Impl" for
-- more parser helpers and "Effectful.CLI.IO" for confirmation prompts.
module Effectful.CLI (
  CLIEffects,
) where

import Data.Error.CLI (CLIError)
import Effectful (IOE, (:>))
import Effectful.Error.Static (Error)
import Effectful.HttpClient (HttpClient)
import Effectful.Logger (Logger)
import Effectful.Reader.Static (Reader)

-- | Effects available to a command runner, with config @c@ and application error @e@.
--
-- @since 0.1.0.0
type CLIEffects c e es =
  ( IOE :> es
  , Reader c :> es
  , Error CLIError :> es
  , Error e :> es
  , Logger :> es
  , HttpClient :> es
  )
