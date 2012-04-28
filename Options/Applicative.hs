{-# LANGUAGE GADTs, DeriveFunctor #-}

module Options.Applicative where

import Control.Applicative
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

data OptionGroup r a = OptionGroup
  { optMain :: Option r
  , optAliases :: [Option r]
  , optDefault :: Maybe a
  , optCont :: r -> Maybe (Parser a) }
  deriving Functor

optOptions :: OptionGroup r a -> [Option r]
optOptions opts = optMain opts : optAliases opts

data OptReader a
  = OptReader (String -> Maybe a)
  | FlagReader !a
  | ArgReader ([String] -> Maybe a)
  | SubReader (Parser a)
  deriving Functor

liftOpt :: OptionGroup r a -> Parser a
liftOpt opts = ConsP (fmap const opts) (pure ())

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x : xs) = Just (x, xs)

rdrApply :: OptReader a -> Maybe String -> [String] -> P (a, [String])
rdrApply rdr value args = case rdr of
  OptReader f -> do
    (arg, args') <- tryP . uncons $ maybeToList value ++ args
    r <- tryP $ f arg
    return (r, args')
  FlagReader r -> return (r, args)
  ArgReader f -> do
    r <- tryP $ f args
    return (r, [])
  SubReader p -> tryP $ runParser p args

data MatchResult
  = NoMatch
  | Match (Maybe String)

instance Monoid MatchResult where
  mempty = NoMatch
  mappend m@(Match _) _ = m
  mappend _ m = m

optMatches :: Option a -> String -> MatchResult
optMatches opt arg = case optReader opt of
  OptReader _  -> foldMap matches ((arg, Nothing) : maybeToList arg1)
  FlagReader _ -> matches (arg, Nothing)
  ArgReader _  -> Match Nothing
  SubReader _
    | arg == expected -> Match Nothing
    | otherwise       -> NoMatch
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
  ConsP :: OptionGroup r (a -> b)
        -> Parser a
        -> Parser b

instance Functor Parser where
  fmap f (NilP x) = NilP (f x)
  fmap f (ConsP opts p) = ConsP (fmap (f.) opts) p

instance Applicative Parser where
  pure = NilP
  NilP f <*> p = fmap f p
  ConsP opts p1 <*> p2 =
    ConsP (fmap uncurry opts) $ (,) <$> p1 <*> p2

data P a
  = ParseError
  | NoParse
  | ParseResult a
  deriving Functor

instance Monad P where
  return = ParseResult
  ParseError >>= _ = ParseError
  NoParse >>= _ = NoParse
  ParseResult a >>= f = f a
  fail _ = ParseError

tryP :: Maybe a -> P a
tryP = maybe ParseError return

stepParser :: Parser a -> String -> [String] -> P (Parser a, [String])
stepParser (NilP _) _ _ = NoParse
stepParser (ConsP opts p) arg args
  | (opt, value) : _ <- all_matches
  = do let reader = optReader opt
       (r, args') <- rdrApply reader value args
       liftOpt' <- tryP $ optCont opts r
       return (liftOpt' <*> p, args')
  | otherwise
  = do (p', args') <- stepParser p arg args
       return (ConsP opts p', args')
  where
    all_matches = catMaybes $ fmap match (optOptions opts)
    match opt = case optMatches opt arg of
      NoMatch -> Nothing
      Match value -> Just (opt, value)

runParser :: Parser a -> [String] -> Maybe (a, [String])
runParser p args = case args of
  [] -> result
  (arg : argt) -> case stepParser p arg argt of
    ParseError -> Nothing
    NoParse -> result
    ParseResult (p', args') -> runParser p' args'
  where
    result = (,) <$> evalParser p <*> pure args

evalParser :: Parser a -> Maybe a
evalParser (NilP r) = pure r
evalParser (ConsP opts p) = optDefault opts <*> evalParser p
