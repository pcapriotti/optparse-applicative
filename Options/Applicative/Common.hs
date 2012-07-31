{-# LANGUAGE Rank2Types, PatternGuards, ScopedTypeVariables #-}
module Options.Applicative.Common (
  -- * Option parsers
  --
  -- | A 'Parser' is composed of a list of options. Several kinds of options
  -- are supported:
  --
  --  * Flags: simple no-argument options. When a flag is encountered on the
  --  command line, its value is returned.
  --
  --  * Options: options with an argument. An option can define a /reader/,
  --  which converts its argument from String to the desired value, or throws a
  --  parse error if the argument does not validate correctly.
  --
  --  * Arguments: positional arguments, validated in the same way as option
  --  arguments.
  --
  --  * Commands. A command defines a completely independent sub-parser. When a
  --  command is encountered, the whole command line is passed to the
  --  corresponding parser.
  --
  Parser,
  liftOpt,

  -- * Program descriptions
  --
  -- A 'ParserInfo' describes a command line program, used to generate a help
  -- screen. Two help modes are supported: brief and full. In brief mode, only
  -- an option and argument summary is displayed, while in full mode each
  -- available option and command, including hidden ones, is described.
  --
  -- A basic 'ParserInfo' with default values for fields can be created using
  -- the 'info' function.
  ParserInfo(..),

  -- * Running parsers
  runParser,
  runParserFully,
  evalParser,

  -- * Low-level utilities
  runP,
  setContext,
  mapParser,
  optionNames
  ) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid

import Options.Applicative.Internal
import Options.Applicative.Types

optionNames :: OptReader a -> [OptName]
optionNames (OptReader names _) = names
optionNames (FlagReader names _) = names
optionNames _ = []

-- | Create a parser composed of a single option.
liftOpt :: Option a -> Parser a
liftOpt = OptP

data MatchResult
  = NoMatch
  | Match (Maybe String)

instance Monoid MatchResult where
  mempty = NoMatch
  mappend m@(Match _) _ = m
  mappend _ m = m

optMatches :: MonadP m => OptReader a -> String -> Maybe (m a)
optMatches rdr arg = case rdr of
  OptReader names f
    | Just (arg1, val) <- parsed
    , arg1 `elem` names
    -> Just $ do
         arg' <- nextArg val
         liftMaybe $ f arg'
    | otherwise -> Nothing
  FlagReader names x
    | Just (arg1, Nothing) <- parsed
    , arg1 `elem` names
    -> Just $ return x
  ArgReader f
    | Just result <- f arg
    -> Just $ return result
  CmdReader _ f
    | Just subp <- f arg
    -> Just $ do
         setContext (Just arg) subp
         runParser (infoParser subp)
  _ -> Nothing
  where
    parsed
      | '-' : '-' : arg1 <- arg
      = case span (/= '=') arg1 of
          (_, "") -> Just (OptLong arg1, Nothing)
          (arg1', _ : rest) -> Just (OptLong arg1', Just rest)
      | '-' : arg1 <- arg
      = case arg1 of
          [] -> Nothing
          [a] -> Just (OptShort a, Nothing)
          (a : rest) -> Just (OptShort a, Just rest)
      | otherwise = Nothing

stepParser :: MonadP m => Parser a -> String -> m (Parser a)
stepParser (NilP _) _ = mzero
stepParser (OptP opt) arg
  | Just matcher <- optMatches (optMain opt) arg
  = pure <$> matcher
  | otherwise = empty
stepParser (MultP p1 p2) arg = msum
  [ do p1' <- stepParser p1 arg
       return (p1' <*> p2)
  , do p2' <- stepParser p2 arg
       return (p1 <*> p2') ]
stepParser (AltP p1 p2) arg = msum
  [ do p1' <- stepParser p1 arg
       return (p1' <|> p2)
  , do p2' <- stepParser p2 arg
       return (p1 <|> p2') ]
stepParser (BindP p k) arg = do
  p' <- stepParser p arg
  x <- liftMaybe $ evalParser p'
  return $ k x

-- | Apply a 'Parser' to a command line, and return a result and leftover
-- arguments.  This function returns an error if any parsing error occurs, or
-- if any options are missing and don't have a default value.
runParser :: forall m a . MonadP m => Parser a -> m a
runParser p = do
  let result = liftMaybe (evalParser p)
  tryP result $ do
    arg <- nextArg Nothing
    p' <- stepParser p arg
    runParser p'

runParserFully :: Parser a -> P a
runParserFully p = do
  r <- runParser p
  args <- getArgs
  guard $ null args
  return r

-- | The default value of a 'Parser'.  This function returns an error if any of
-- the options don't have a default value.
evalParser :: Parser a -> Maybe a
evalParser (NilP r) = r
evalParser (OptP _) = Nothing
evalParser (MultP p1 p2) = evalParser p1 <*> evalParser p2
evalParser (AltP p1 p2) = evalParser p1 <|> evalParser p2
evalParser (BindP p k) = evalParser p >>= evalParser . k

-- | Map a polymorphic function over all the options of a parser, and collect
-- the results.
mapParser :: (forall x . OptHelpInfo -> Option x -> b)
          -> Parser a
          -> [b]
mapParser = go False False
  where
    has_default :: Parser a -> Bool
    has_default p = isJust (evalParser p)

    go :: Bool -> Bool
       -> (forall x . OptHelpInfo -> Option x -> b)
       -> Parser a -> [b]
    go _ _ _ (NilP _) = []
    go m d f (OptP opt) = [f (OptHelpInfo m d) opt]
    go m d f (MultP p1 p2) = go m d f p1 ++ go m d f p2
    go m d f (AltP p1 p2) = go m d' f p1 ++ go m d' f p2
      where d' = d || has_default p1 || has_default p2
    go _ d f (BindP p _) = go True d f p
