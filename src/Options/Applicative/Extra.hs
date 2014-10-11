{-# LANGUAGE RankNTypes #-}
module Options.Applicative.Extra (
  -- * Extra parser utilities
  --
  -- | This module contains high-level functions to run parsers.
  helper,
  execParser,
  execParserPure,
  getParseResult,
  handleParseResult,
  parserFailure,
  renderFailure,
  ParserFailure(..),
  overFailure,
  ParserResult(..),
  ParserPrefs(..),
  CompletionResult(..),
  ) where

import Control.Applicative
import Data.Monoid (mempty, mconcat)
import System.Environment (getArgs, getProgName)
import System.Exit
import System.IO (hPutStrLn, stderr)

import Options.Applicative.Basic
import Options.Applicative.Builder
import Options.Applicative.Classes
import Options.Applicative.Help

import Options.Applicative.Types

-- | Run a program description.
--
-- Parse command line arguments. Display help text and exit if any parse error
-- occurs.
execParser :: (HasMetadata f, HasUsage f, OptParser f) => f a -> IO a
execParser p = (execParserPure p <$> getArgs) >>= handleParseResult

-- | Handle `ParserResult`.
handleParseResult :: Either (ParserFailure ParserHelp) a -> IO a
handleParseResult (Right a) = return a
handleParseResult (Left failure) = do
      progn <- getProgName
      let (msg, exit) = renderFailure failure progn
      case exit of
        ExitSuccess -> putStrLn msg
        _           -> hPutStrLn stderr msg
      exitWith exit

-- | Extract the actual result from a `ParserResult` value.
--
-- This function returns 'Nothing' in case of errors.  Possible error messages
-- or completion actions are simply discarded.
--
-- If you want to display error messages and invoke completion actions
-- appropriately, use 'handleParseResult' instead.
getParseResult :: ParserResult a -> Maybe a
getParseResult (Success a) = Just a
getParseResult _ = Nothing

-- | The most general way to run a parser in pure code.
execParserPure :: (HasMetadata f, HasUsage f, OptParser f)
               => f a               -- ^ Parser to run
               -> [String]          -- ^ Program arguments
               -> Either (ParserFailure ParserHelp) a
execParserPure p args = either (Left . parserFailure p) pure
                      . (`evalArgParser` args) . runParser $ p

-- | Generate a `ParserFailure` from a `ParseError` in a given `Context`.
--
-- This function can be used, for example, to show the help text for a parser:
--
-- @handleParseResult . Failure $ parserFailure pprefs pinfo ShowHelpText mempty@

parserFailure :: (HasMetadata f, HasUsage f) => f a
              -> ParseError -> ParserFailure ParserHelp
parserFailure p err = ParserFailure $ \progn ->
  ( mconcat [ base_help
            , usage_help progn []
            , error_help ]
  , exit_code
  , 80 )

-- parserFailure pprefs pinfo msg ctx = ParserFailure $ \progn ->
--   let h = with_context ctx pinfo $ \names pinfo' -> mconcat
--             [ base_help pinfo'
--             , usage_help progn names pinfo'
--             , error_help ]
--   in (h, exit_code, prefColumns pprefs)
  where
    md = getMetadata p

    exit_code = case err of
      ErrorMsg _   -> ExitFailure (mdFailureCode md)
      UnknownError -> ExitFailure (mdFailureCode md)
      _            -> ExitSuccess

    usage_help progn names = case err of
      InfoMsg _ -> mempty
      _         -> usageHelp $ vcatChunks
        [ pure . parserUsage p . unwords $ progn : names
        , fmap (indent 2) . mdProgDesc $ md ]

    error_help = errorHelp $ case err of
      ShowHelpText -> mempty
      ErrorMsg m   -> stringChunk m
      InfoMsg  m   -> stringChunk m
      UnknownError -> mempty

    base_help :: ParserHelp
    base_help
      | show_full_help
      = mconcat [h, f, parserHelp p]
      | otherwise
      = mempty
      where
        h = headerHelp (mdHeader md)
        f = footerHelp (mdFooter md)

    show_full_help = case err of
      ShowHelpText -> True
      _            -> False


renderFailure :: ParserFailure ParserHelp -> String -> (String, ExitCode)
renderFailure failure progn =
  let (h, exit, cols) = execFailure failure progn
  in (renderHelp cols h, exit)
