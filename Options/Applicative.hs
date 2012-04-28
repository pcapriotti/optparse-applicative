{-# LANGUAGE GADTs, DeriveFunctor #-}

module Options.Applicative where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Monoid

data OptName = OptLong !String
             | OptShort !Char

optNameStr :: OptName -> String
optNameStr (OptLong name) = name
optNameStr (OptShort n) = [n]

isLong, isShort :: OptName -> Bool
isLong (OptLong _ ) = True
isLong _ = False
isShort (OptShort _ ) = True
isShort _ = False

data OptionGroup r a = OptionGroup
  { optMain :: Option r
  , optAliases :: [Option r]
  , optDefault :: Maybe a
  , optCont :: r -> Maybe (Parser a) }
  deriving Functor

optOptions :: OptionGroup r a -> [Option r]
optOptions opts = optMain opts : optAliases opts

data Option a
  = Option !OptName (String -> Maybe a)
  | Flag !OptName !a
  | Argument (String -> Maybe a)
  | Command (String -> Maybe (Parser a))
  deriving Functor

liftOpt :: OptionGroup r a -> Parser a
liftOpt opts = ConsP (fmap const opts) (pure ())

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x : xs) = Just (x, xs)

data MatchResult
  = NoMatch
  | Match (Maybe String)

instance Monoid MatchResult where
  mempty = NoMatch
  mappend m@(Match _) _ = m
  mappend _ m = m

type Matcher a = [String] -> P (a, [String])

optMatches :: Option a -> String -> Maybe (Matcher a)
optMatches opt arg = case opt of
  Option name f
    | Match value <- foldMap matches ((arg, Nothing) : maybeToList arg1)
    -> Just $ \args -> do
        (arg', args') <- tryP . uncons $ maybeToList value ++ args
        r <- tryP $ f arg'
        return (r, args')
      where
        matches (s, rest)
          | s == expected name = Match rest
          | otherwise          = NoMatch
        arg1
          | isLong name
          = case span (/= '=') arg of
              (_, "")   -> Nothing
              (s, _ : rest) -> Just (s, Just rest)
          | otherwise
          = case splitAt 2 arg of
              (_, "") -> Nothing
              (s, rest) -> Just (s, Just rest)
  Flag name x
    | arg == expected name
    -> Just $ \args -> return (x, args)
  Argument f
    | Just result <- f arg
    -> Just $ \args -> return (result, args)
  Command f
    | Just p <- f arg
    -> Just $ \args -> tryP $ runParser p args
  _ -> Nothing
  where
    expected name
      | isLong name
      = '-' : '-' : optNameStr name
      | otherwise
      = '-' : optNameStr name

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

instance Applicative P where
  pure = return
  (<*>) = ap

tryP :: Maybe a -> P a
tryP = maybe ParseError return

stepParser :: Parser a -> String -> [String] -> P (Parser a, [String])
stepParser (NilP _) _ _ = NoParse
stepParser (ConsP opts p) arg args
  -- take first matcher
  | matcher : _ <- all_matchers
  = do (r, args') <- matcher args
       liftOpt' <- tryP $ optCont opts r
       return (liftOpt' <*> p, args')
  | otherwise
  = do (p', args') <- stepParser p arg args
       return (ConsP opts p', args')
  where
    all_matchers = catMaybes $ fmap match (optOptions opts)
    match opt = optMatches opt arg

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
