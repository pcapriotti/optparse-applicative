{-# LANGUAGE Rank2Types, PatternGuards #-}
module Options.Applicative where

import Control.Applicative
import Data.Lens.Common
import Data.List
import Data.Maybe
import Data.Monoid

import Options.Applicative.Utils
import Options.Applicative.Types

optNameStr :: OptName -> String
optNameStr (OptLong name) = name
optNameStr (OptShort n) = [n]

isLong, isShort :: OptName -> Bool
isLong (OptLong _ ) = True
isLong _ = False
isShort (OptShort _ ) = True
isShort _ = False

optionNames :: OptReader a -> [OptName]
optionNames (OptReader names _) = names
optionNames (FlagReader names _) = names
optionNames _ = []

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
  CmdReader _ f
    | Just cmdInfo <- f arg
    -> Just $ \args -> tryP $ runParser (infoParser cmdInfo) args
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

tryP :: Maybe a -> P a
tryP = maybe ParseError return

stepParser :: Parser a -> String -> [String] -> P (Parser a, [String])
stepParser (NilP _) _ _ = ParseError
stepParser (ConsP opt p) arg args
  | Just matcher <- optMatches (opt^.optMain) arg
  = do (r, args') <- matcher args
       liftOpt' <- tryP $ getL optCont opt r
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
evalParser (ConsP opt p) = opt^.optDefault <*> evalParser p

mapParser :: (forall r x . Option r x -> b)
          -> Parser a
          -> [b]
mapParser _ (NilP _) = []
mapParser f (ConsP opt p) = f opt : mapParser f p

showOption :: OptName -> String
showOption (OptLong n) = "--" ++ n
showOption (OptShort n) = '-' : [n]

data OptDescStyle = OptDescStyle
  { descSep :: String
  , descHidden :: Bool
  , descSurround :: Bool }

optDesc :: OptDescStyle -> Option r a -> String
optDesc style opt =
  let ns = optionNames $ opt^.optMain
      mv = opt^.optMetaVar
      descs = map showOption (sort ns)
      desc' = intercalate (descSep style) descs <+> mv
      render text
        | not (opt^.optShow) && not (descHidden style)
        = ""
        | null text || not (descSurround style)
        = text
        | isJust (opt^.optDefault)
        = "[" ++ text ++ "]"
        | null (drop 1 descs)
        = text
        | otherwise
        = "(" ++ text ++ ")"
  in render desc'

shortDesc :: Parser a -> String
shortDesc = foldr (<+>) "" . mapParser (optDesc style)
  where
    style = OptDescStyle
      { descSep = "|"
      , descHidden = False
      , descSurround = True }

fullDesc :: Parser a -> String
fullDesc = tabulate' . catMaybes . mapParser doc
  where
    doc opt
      | null n = Nothing
      | null h = Nothing
      | otherwise = Just (n, h)
      where n = optDesc style opt
            h = opt^.optHelp
    style = OptDescStyle
      { descSep = ","
      , descHidden = True
      , descSurround = False }
