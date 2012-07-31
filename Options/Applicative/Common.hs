{-# LANGUAGE Rank2Types, PatternGuards #-}
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
  treeMapParser,
  optionNames
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Error
import Control.Monad.Trans.Writer
import Data.Maybe
import Data.Monoid
import Options.Applicative.Types

optionNames :: OptReader a -> [OptName]
optionNames (OptReader names _) = names
optionNames (FlagReader names _) = names
optionNames _ = []

-- | Create a parser composed of a single option.
liftOpt :: Option a -> Parser a
liftOpt = OptP

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
    | Just subp <- f arg
    -> Just $ \args -> do
          setContext (Just arg) subp
          runParser (infoParser subp) args
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
tryP = maybe empty return

runP :: P a -> (Either String a, Context)
runP = runWriter . runErrorT

setContext :: Maybe String -> ParserInfo a -> P ()
setContext name = lift . tell . Context name

stepParser :: Parser a -> String -> [String] -> P (Parser a, [String])
stepParser (NilP _) _ _ = empty
stepParser (OptP opt) arg args
  | Just matcher <- optMatches (optMain opt) arg
  = do (r, args') <- matcher args
       return (pure r, args')
  | otherwise = empty
stepParser (MultP p1 p2) arg args = msum
  [ do (p1', args') <- stepParser p1 arg args
       return (p1' <*> p2, args')
  , do (p2', args') <- stepParser p2 arg args
       return (p1 <*> p2', args') ]
stepParser (AltP p1 p2) arg args = msum
  [ stepParser p1 arg args
  , stepParser p2 arg args ]
stepParser (BindP p k) arg args = do
  (p', args') <- stepParser p arg args
  x <- evalParser p'
  return (k x, args')

-- | Apply a 'Parser' to a command line, and return a result and leftover
-- arguments.  This function returns an error if any parsing error occurs, or
-- if any options are missing and don't have a default value.
runParser :: Parser a -> [String] -> P (a, [String])
runParser p args = case args of
  [] -> result
  (arg : argt) -> do
    x <- catchError (Right <$> stepParser p arg argt)
                    (return . Left)
    case x of
      Left e -> result <|> throwError e
      Right (p', args') -> runParser p' args'
  where
    result = (,) <$> evalParser p <*> pure args

runParserFully :: Parser a -> [String] -> P a
runParserFully p args = do
  (r, args') <- runParser p args
  guard $ null args'
  return r

-- | The default value of a 'Parser'.  This function returns an error if any of
-- the options don't have a default value.
evalParser :: Parser a -> P a
evalParser (NilP r) = tryP r
evalParser (OptP _) = empty
evalParser (MultP p1 p2) = evalParser p1 <*> evalParser p2
evalParser (AltP p1 p2) = evalParser p1 <|> evalParser p2
evalParser (BindP p k) = evalParser p >>= evalParser . k

-- | Map a polymorphic function over all the options of a parser, and collect
-- the results in a list.
mapParser :: (forall x. OptHelpInfo -> Option x -> b)
              -> Parser a -> [b]
mapParser f = flatten . treeMapParser f
  where
    flatten (Leaf x) = [x]
    flatten (MultNode xs) = xs >>= flatten
    flatten (AltNode xs) = xs >>= flatten

-- | Like 'mapParser', but collect the results in a tree structure.
treeMapParser :: (forall x . OptHelpInfo -> Option x -> b)
          -> Parser a
          -> OptTree b
treeMapParser g = simplify . go False False g
  where
    has_default :: Parser a -> Bool
    has_default p = case runP (evalParser p) of
      (Left _, _) -> False
      (Right _, _) -> True

    go :: Bool -> Bool
       -> (forall x . OptHelpInfo -> Option x -> b)
       -> Parser a
       -> OptTree b
    go _ _ _ (NilP _) = MultNode []
    go m d f (OptP opt) = Leaf (f (OptHelpInfo m d) opt)
    go m d f (MultP p1 p2) = MultNode [go m d f p1, go m d f p2]
    go m d f (AltP p1 p2) = AltNode [go m d' f p1, go m d' f p2]
      where d' = d || has_default p1 || has_default p2
    go _ d f (BindP p _) = go True d f p

simplify :: OptTree a -> OptTree a
simplify (Leaf x) = Leaf x
simplify (MultNode xs) =
  case concatMap (remove_mult . simplify) xs of
    [x] -> x
    xs' -> MultNode xs'
  where
    remove_mult (MultNode ts) = ts
    remove_mult t = [t]
simplify (AltNode xs) =
  case concatMap (remove_alt . simplify) xs of
    []  -> MultNode []
    [x] -> x
    xs' -> AltNode xs'
  where
    remove_alt (AltNode ts) = ts
    remove_alt (MultNode []) = []
    remove_alt t = [t]
