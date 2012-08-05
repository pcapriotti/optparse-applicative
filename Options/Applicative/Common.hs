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
  mapParser,
  treeMapParser,
  optionNames
  ) where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Data.Monoid

import Options.Applicative.Internal
import Options.Applicative.Types

optionNames :: OptReader a -> [OptName]
optionNames (OptReader names _) = names
optionNames (FlagReader names _) = names
optionNames _ = []

isOptionPrefix :: OptName -> OptName -> Bool
isOptionPrefix (OptShort x) (OptShort y) = x == y
isOptionPrefix (OptLong x) (OptLong y) = x `isPrefixOf` y
isOptionPrefix _ _ = False

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

type Matcher m a = [String] -> m (a, [String])

optMatches :: MonadP m => Bool -> OptReader a -> String -> Maybe (Matcher m a)
optMatches disambiguate opt arg = case opt of
  OptReader names rdr
    | Just (arg1, val) <- parsed
    , has_name arg1 names
    -> Just $ \args -> do
         let mb_args = uncons $ maybeToList val ++ args
         (arg', args') <- maybe (missingArgP (crCompleter rdr)) return mb_args
         r <- liftMaybe $ crReader rdr arg'
         return (r, args')
    | otherwise -> Nothing
  FlagReader names x
    | Just (arg1, Nothing) <- parsed
    , has_name arg1 names
    -> Just $ \args -> return (x, args)
  ArgReader rdr
    | Just result <- crReader rdr arg
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
    has_name a
      | disambiguate = any (isOptionPrefix a)
      | otherwise = elem a

stepParser :: MonadP m => ParserPrefs -> Parser a -> String -> [String] -> [m (Parser a, [String])]
stepParser _ (NilP _) _ _ = []
stepParser prefs (OptP opt) arg args =
  case optMatches disambiguate (optMain opt) arg of
    Just matcher -> pure $ do
      (r, args') <- matcher args
      return (pure r, args')
    Nothing -> empty
  where
    disambiguate = prefDisambiguate prefs
                && optVisibility opt > Internal
stepParser prefs (MultP p1 p2) arg args = msum
  [ flip map (stepParser prefs p1 arg args) $ \m ->
      do (p1', args') <- m
         return (p1' <*> p2, args')
  , flip map (stepParser prefs p2 arg args) $ \m ->
      do (p2', args') <- m
         return (p1 <*> p2', args') ]
stepParser prefs (AltP p1 p2) arg args = msum
  [ stepParser prefs p1 arg args
  , stepParser prefs p2 arg args ]
stepParser prefs (BindP p k) arg args =
  flip map (stepParser prefs p arg args) $ \m -> do
    (p', args') <- m
    x <- liftMaybe $ evalParser p'
    return (k x, args')

-- | Apply a 'Parser' to a command line, and return a result and leftover
-- arguments.  This function returns an error if any parsing error occurs, or
-- if any options are missing and don't have a default value.
runParser :: MonadP m => Parser a -> [String] -> m (a, [String])
runParser p args = case args of
  [] -> exitP p result
  (arg : argt) -> do
    prefs <- getPrefs
    x <- tryP $ do_step prefs arg argt
    case x of
      Left e -> liftMaybe result <|> errorP e
      Right (p', args') -> runParser p' args'
  where
    result = (,) <$> evalParser p <*> pure args
    do_step prefs arg argt
      | prefDisambiguate prefs
      = case parses of
          [m] -> m
          _   -> empty
      | otherwise
      = msum parses
      where parses = stepParser prefs p arg argt

runParserFully :: MonadP m => Parser a -> [String] -> m a
runParserFully p args = do
  (r, args') <- runParser p args
  guard $ null args'
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
    has_default p = isJust (evalParser p)

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
