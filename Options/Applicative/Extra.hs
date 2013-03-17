{-# LANGUAGE RankNTypes #-}
module Options.Applicative.Extra (
  -- * Extra parser utilities
  --
  -- | This module contains high-level functions to run parsers.
  helper,
  execParser,
  execParserPure,
  customExecParser,
  usage,
  ParserFailure(..),
  ) where

import Control.Applicative ((<$>), (<|>))
import Data.Monoid (mconcat)
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStr, stderr)

import Options.Applicative.BashCompletion
import Options.Applicative.Common
import Options.Applicative.Builder hiding (briefDesc)
import Options.Applicative.Help
import Options.Applicative.Internal
import Options.Applicative.Utils
import Options.Applicative.Types

-- | A hidden \"helper\" option which always fails.
helper :: Parser (a -> a)
helper = nullOption $ mconcat
       [ long "help"
       , reader (const (ReadM (Left ShowHelpText)))
       , noArgError ShowHelpText
       , short 'h'
       , help "Show this help text"
       , value id
       , metavar ""
       , hidden ]

-- | Run a program description.
--
-- Parse command line arguments. Display help text and exit if any parse error
-- occurs.
execParser :: ParserInfo a -> IO a
execParser = customExecParser (prefs idm)

-- | Run a program description with custom preferences.
customExecParser :: ParserPrefs -> ParserInfo a -> IO a
customExecParser pprefs pinfo = do
  args <- getArgs
  case execParserPure pprefs pinfo args of
    Right a -> return a
    Left failure -> do
      progn <- getProgName
      let c = errExitCode failure
      msg <- errMessage failure progn
      case c of
        ExitSuccess -> putStr msg
        _           -> hPutStr stderr msg
      exitWith c

data Result a = Result a
              | Extra ParserFailure

-- | A pure version 'execParser'.
execParserPure :: ParserPrefs       -- ^ Global preferences for this parser
               -> ParserInfo a      -- ^ Description of the program to run
               -> [String]          -- ^ Program arguments
               -> Either ParserFailure a
execParserPure pprefs pinfo args =
  case runP p pprefs of
    (Right r, _) -> case r of
      Result a -> Right a
      Extra failure -> Left failure
    (Left msg, ctx) -> Left $
      parserFailure pprefs pinfo msg ctx
  where
    parser = infoParser pinfo
    parser' = (Extra <$> bashCompletionParser parser pprefs)
          <|> (Result <$> parser)
    p = runParserFully parser' args

parserFailure :: ParserPrefs -> ParserInfo a
              -> ParseError -> Context
              -> ParserFailure
parserFailure pprefs pinfo msg ctx = ParserFailure
  { errMessage = \progn
      -> with_context ctx pinfo $ \names ->
             return
           . show_help
           . add_error
           . add_usage names progn
  , errExitCode = ExitFailure (infoFailureCode pinfo) }
  where
    add_usage names progn i = i
      { infoHeader = vcat
          ( header_line i ++
            [ usage pprefs (infoParser i) ename ] ) }
      where
        ename = unwords (progn : names)
    add_error i = i
      { infoHeader = vcat (error_msg ++ [infoHeader i]) }
    error_msg = case msg of
      ShowHelpText -> []
      ErrorMsg m   -> [m]
    show_full_help = case msg of
      ShowHelpText -> True
      _            -> prefShowHelpOnError pprefs
    show_help i
      | show_full_help
      = parserHelpText pprefs i
      | otherwise
      = unlines $ filter (not . null) [ infoHeader i ]
    header_line i
      | show_full_help
      = [ infoHeader i ]
      | otherwise
      = []

    with_context :: Context
                 -> ParserInfo a
                 -> (forall b . [String] -> ParserInfo b -> c)
                 -> c
    with_context NullContext i f = f [] i
    with_context (Context n i) _ f = f n i

-- | Generate option summary.
usage :: ParserPrefs -> Parser a -> String -> String
usage pprefs p progn = foldr (<+>) ""
  [ "Usage:"
  , progn
  , briefDesc pprefs p ]
