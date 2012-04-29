{-# LANGUAGE GADTs, Rank2Types, DeriveFunctor #-}

module Options.Applicative where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Data.Monoid

data OptName = OptLong !String
             | OptShort !Char
  deriving Eq

optNameStr :: OptName -> String
optNameStr (OptLong name) = name
optNameStr (OptShort n) = [n]

isLong, isShort :: OptName -> Bool
isLong (OptLong _ ) = True
isLong _ = False
isShort (OptShort _ ) = True
isShort _ = False

data Option r a = Option
  { optMain :: OptReader r
  , optDefault :: Maybe a
  , optHelp :: String
  , optCont :: r -> Maybe (Parser a) }
  deriving Functor

data OptReader a
  = OptReader [OptName] (String -> Maybe a)
  | FlagReader [OptName] !a
  | ArgReader (String -> Maybe a)
  | CmdReader (String -> Maybe (Parser a))
  deriving Functor

optNames :: OptReader a -> [OptName]
optNames (OptReader names _) = names
optNames (FlagReader names _) = names
optNames _ = []

liftOpt :: Option r a -> Parser a
liftOpt opt = ConsP (fmap const opt) (pure ())

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

optMatches :: OptReader a -> String -> Maybe (Matcher a)
optMatches rdr arg = case rdr of
  OptReader names f
    | Just (arg1, val) <- parsed
    , arg1 `elem` names
    -> Just $ \args -> do
         (arg', args') <- tryP . uncons $ maybeToList val ++ args
         r <- tryP $ f arg'
         return (r, args')
    | otherwise -> Nothing
  FlagReader names x
    | Just (arg1, Nothing) <- parsed
    , arg1 `elem` names
    -> Just $ \args -> return (x, args)
  ArgReader f
    | Just result <- f arg
    -> Just $ \args -> return (result, args)
  CmdReader f
    | Just p <- f arg
    -> Just $ \args -> tryP $ runParser p args
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


data Parser a where
  NilP :: a -> Parser a
  ConsP :: Option r (a -> b)
        -> Parser a
        -> Parser b

instance Functor Parser where
  fmap f (NilP x) = NilP (f x)
  fmap f (ConsP opt p) = ConsP (fmap (f.) opt) p

instance Applicative Parser where
  pure = NilP
  NilP f <*> p = fmap f p
  ConsP opt p1 <*> p2 =
    ConsP (fmap uncurry opt) $ (,) <$> p1 <*> p2

data P a
  = ParseError
  | ParseResult a
  deriving Functor

instance Monad P where
  return = ParseResult
  ParseError >>= _ = ParseError
  ParseResult a >>= f = f a
  fail _ = ParseError

instance Applicative P where
  pure = return
  (<*>) = ap

tryP :: Maybe a -> P a
tryP = maybe ParseError return

stepParser :: Parser a -> String -> [String] -> P (Parser a, [String])
stepParser (NilP _) _ _ = ParseError
stepParser (ConsP opt p) arg args
  -- take first matcher
  | Just matcher <- optMatches (optMain opt) arg
  = do (r, args') <- matcher args
       liftOpt' <- tryP $ optCont opt r
       return (liftOpt' <*> p, args')
  | otherwise
  = do (p', args') <- stepParser p arg args
       return (ConsP opt p', args')

runParser :: Parser a -> [String] -> Maybe (a, [String])
runParser p args = case args of
  [] -> result
  (arg : argt) -> case stepParser p arg argt of
    ParseError -> result
    ParseResult (p', args') -> runParser p' args'
  where
    result = (,) <$> evalParser p <*> pure args

evalParser :: Parser a -> Maybe a
evalParser (NilP r) = pure r
evalParser (ConsP opt p) = optDefault opt <*> evalParser p

mapParser :: (forall r x . Option r x -> b)
          -> Parser a
          -> [b]
mapParser _ (NilP _) = []
mapParser f (ConsP opt p) = f opt : mapParser f p

generateHelp :: Parser a -> String
generateHelp = intercalate "\n" . mapParser doc
  where
    doc opt = ' ' : names 24 opt ++ " " ++ optHelp opt
    names size = pad size . intercalate ", " . map name . optNames . optMain
    pad size str = str ++ replicate (size - n `max` 0) ' '
      where n = length str
    name (OptLong n) = "--" ++ n
    name (OptShort n) = '-' : [n]
