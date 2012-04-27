{-# LANGUAGE GADTs, DeriveFunctor #-}

module Options.Applicative where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Monoid

data OptName = OptLong String | OptShort Char

optNameStr :: OptName -> String
optNameStr (OptLong name) = name
optNameStr (OptShort n) = [n]

isLong, isShort :: OptName -> Bool
isLong (OptLong _ ) = True
isLong _ = False
isShort (OptShort _ ) = True
isShort _ = False

data Option a = Option
  { optName :: OptName
  , optReader :: OptReader a
  } deriving Functor

data OptionGroup a = OptionGroup
  { optAliases :: [Option a]
  , optDefault :: Maybe a }
  deriving Functor

data OptReader a
  = OptReader (String -> Maybe (Parser a))
  | FlagParser !a
  | ArgParser ([String] -> Maybe a)
  | SubParser !Bool (Parser a)
  deriving Functor

liftOpt :: OptionGroup a -> Parser a
liftOpt opts = ConsP (fmap const opts) (pure ())

option :: String
       -> Char
       -> Maybe a
       -> (String -> Maybe a)
       -> Parser a
option lname sname def p = liftOpt OptionGroup
  { optAliases = [ Option (OptLong lname) reader
                 , Option (OptShort sname) reader ]
  , optDefault = def }
  where
    reader = OptReader (fmap pure . p)

optionR :: Read a
        => String
        -> Char
        -> Maybe a
        -> Parser a
optionR lname sname def = option lname sname def p
  where
    p arg = case reads arg of
      [(r, "")] -> Just r
      _         -> Nothing

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x : xs) = Just (x, xs)

rdrApply :: OptReader a -> Maybe String -> [String] -> Maybe (Parser a, [String])
rdrApply rdr value args = case rdr of
  (OptReader f) -> do
    (arg, rest) <- uncons $ maybeToList value ++ args
    parser <- f arg
    return (parser, rest)
  (FlagParser r) -> do
    guard $ isNothing value
    return (pure r, args)
  (ArgParser f) -> do
    guard $ isNothing value
    r <- f args
    return (pure r, [])
  (SubParser strict parser) -> do
    guard $ isNothing value
    (r, rest) <- runParser parser args
    when strict . guard $ null rest
    return (pure r, rest)

data MatchResult
  = NoMatch
  | Match (Maybe String)

instance Monoid MatchResult where
  mempty = NoMatch
  mappend m@(Match _) _ = m
  mappend _ m = m

optMatches :: Option a -> String -> MatchResult
optMatches opt arg = foldMap matches ((arg, Nothing) : maybeToList arg1)
  where
    name = optName opt
    expected
      | isLong name
      = '-' : '-' : optNameStr name
      | otherwise
      = '-' : optNameStr name
    matches (s, rest)
      | s == expected = Match rest
      | otherwise     = NoMatch
    arg1
      | isLong name
      = case span (/= '=') arg of
          (_, "")   -> Nothing
          (s, _ : rest) -> Just (s, Just rest)
      | otherwise
      = case splitAt 2 arg of
          (_, "") -> Nothing
          (s, rest) -> Just (s, Just rest)

data Parser a where
  NilP :: a -> Parser a
  ConsP :: OptionGroup (a -> b)
        -> Parser a -> Parser b

instance Functor Parser where
  fmap f (NilP x) = NilP (f x)
  fmap f (ConsP opt rest) = ConsP (fmap (f.) opt) rest

instance Applicative Parser where
  pure = NilP
  NilP f <*> p = fmap f p
  ConsP opt rest <*> p =
    ConsP (fmap uncurry opt) (fmap (,) rest <*> p)

stepParser :: Parser a -> String -> [String] -> Maybe (Parser a, [String])
stepParser (NilP _) _ _ = Nothing
stepParser (ConsP opts rest) arg args
  | (opt, value) : _ <- all_matches
  = do let reader = optReader opt
       (parser', args') <- rdrApply reader value args
       return (parser' <*> rest, args')
  | otherwise
  = do (parser', args') <- stepParser rest arg args
       return (ConsP opts parser', args')
  where
    all_matches = catMaybes $ fmap match (optAliases opts)
    match opt = case optMatches opt arg of
      NoMatch -> Nothing
      Match value -> Just (opt, value)

runParser :: Parser a -> [String] -> Maybe (a, [String])
runParser parser args = case args of
  [] -> result
  (arg : argt) -> case stepParser parser arg argt of
    Nothing -> result
    Just (parser', args') -> runParser parser' args'
  where
    result = do
      r <- evalParser parser
      return (r, args)

evalParser :: Parser a -> Maybe a
evalParser (NilP r) = pure r
evalParser (ConsP opt rest) = optDefault opt <*> evalParser rest
