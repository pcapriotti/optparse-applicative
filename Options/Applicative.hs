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
  { optOptions :: [Option r]
  , optDefault :: Maybe a
  , optCont :: r -> Parser a }
  deriving Functor

data OptReader a
  = OptReader (String -> Maybe a)
  | FlagReader !a
  | ArgReader ([String] -> Maybe a)
  | SubReader (Parser a)
  deriving Functor

liftOpt :: OptionGroup r a -> Parser a
liftOpt opts = ConsP (fmap const opts) (pure ())

option :: String
       -> Char
       -> Maybe a
       -> (String -> Maybe a)
       -> Parser a
option lname sname def p = liftOpt OptionGroup
  { optOptions = [ Option (OptLong lname) reader
                 , Option (OptShort sname) reader ]
  , optDefault = def
  , optCont = pure }
  where
    reader = OptReader p

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

rdrApply :: OptReader a -> Maybe String -> [String] -> Maybe (a, [String])
rdrApply rdr value args = case rdr of
  OptReader f -> do
    (arg, args') <- uncons $ maybeToList value ++ args
    r <- f arg
    return (r, args')
  FlagReader r -> return (r, args)
  ArgReader f -> do
    r <- f args
    return (r, [])
  SubReader parser -> runParser parser args

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

stepParser :: Parser a -> String -> [String] -> Maybe (Parser a, [String])
stepParser (NilP _) _ _ = Nothing
stepParser (ConsP opts p) arg args
  | (opt, value) : _ <- all_matches
  = do let reader = optReader opt
       (r, args') <- rdrApply reader value args
       let parser' = optCont opts r
       return (parser' <*> p, args')
  | otherwise
  = do (p', args') <- stepParser p arg args
       return (ConsP opts p', args')
  where
    all_matches = catMaybes $ fmap match (optOptions opts)
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
evalParser (ConsP opts p) = optDefault opts <*> evalParser p
