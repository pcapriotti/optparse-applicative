{-# LANGUAGE Rank2Types #-}
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
  showOption,

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

import Control.Applicative (pure, (<*>), (<$>), (<|>))
import Control.Monad (guard, mzero, msum)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..), get, put, runStateT)
import Data.List (isPrefixOf)
import Data.Maybe (maybeToList, isJust)
import Data.Monoid (Monoid(..))

import Options.Applicative.Internal
import Options.Applicative.Types

showOption :: OptName -> String
showOption (OptLong n) = "--" ++ n
showOption (OptShort n) = '-' : [n]

optionNames :: OptReader a -> [OptName]
optionNames (OptReader names _ _) = names
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

type Args = [String]

optMatches :: MonadP m => Bool -> OptReader a -> String -> Maybe (StateT Args m a)
optMatches disambiguate opt arg = case opt of
  OptReader names rdr no_arg_err -> do
    (arg1, val) <- parsed
    guard $ has_name arg1 names
    Just $ do
      args <- get
      let mb_args = uncons $ maybeToList val ++ args
      let missing_arg = lift $ missingArgP no_arg_err (crCompleter rdr)
      (arg', args') <- maybe missing_arg return mb_args
      put args'
      case runReadM (crReader rdr arg') of
        Left e -> lift $ errorFor arg1 e
        Right r -> return r
  FlagReader names x -> do
    (arg1, Nothing) <- parsed
    guard $ has_name arg1 names
    Just $ return x
  ArgReader rdr -> do
    result <- crReader rdr arg
    Just $ return result
  CmdReader _ f ->
    flip fmap (f arg) $ \subp -> StateT $ \args -> do
      setContext (Just arg) subp
      prefs <- getPrefs
      let runSubparser
            | prefBacktrack prefs = runParser
            | otherwise = \p a
            -> (,) <$> runParserFully p a <*> pure []
      runSubparser (infoParser subp) args
  where
    errorFor name (ErrorMsg msg) =
      errorP (ErrorMsg ("option " ++ showOption name ++ ": " ++ msg))
    errorFor _ e = errorP e

    parsed =
      case arg of
        '-' : '-' : arg1 ->
          Just $
          case span (/= '=') arg1 of
            (_, "") -> (OptLong arg1, Nothing)
            (arg1', _ : rest) -> (OptLong arg1', Just rest)
        '-' : arg1 ->
          case arg1 of
            [] -> Nothing
            (a : rest) -> Just (OptShort a, if null rest then Nothing else Just rest)
        _ -> Nothing
    has_name a
      | disambiguate = any (isOptionPrefix a)
      | otherwise = elem a

stepParser :: MonadP m => ParserPrefs -> String -> Parser a
           -> ListT (StateT Args m) (Parser a)
stepParser _ _ (NilP _) = mzero
stepParser prefs arg (OptP opt) = do
  case optMatches disambiguate (optMain opt) arg of
    Just matcher -> pure <$> lift matcher
    Nothing -> mzero
  where
    disambiguate = prefDisambiguate prefs
                && optVisibility opt > Internal
stepParser prefs arg (MultP p1 p2) = msum
  [ do p1' <- stepParser prefs arg p1
       return (p1' <*> p2)
  , do p2' <- stepParser prefs arg p2
       return (p1 <*> p2') ]
stepParser prefs arg (AltP p1 p2) = msum
  [ stepParser prefs arg p1
  , stepParser prefs arg p2 ]
stepParser prefs arg (BindP p k) = do
  p' <- stepParser prefs arg p
  x <- hoistMaybe $ evalParser p'
  return (k x)

-- | Apply a 'Parser' to a command line, and return a result and leftover
-- arguments.  This function returns an error if any parsing error occurs, or
-- if any options are missing and don't have a default value.
runParser :: MonadP m => Parser a -> Args -> m (a, Args)
runParser p args = case args of
  [] -> exitP p result
  (arg : argt) -> do
    prefs <- getPrefs
    x <- do_step prefs arg argt
    case x of
      Left e -> case (result, e) of
        (Just r, ErrorMsg _) -> return r
        _ -> errorP e
      Right (p', args') -> runParser p' args'
  where
    result = (,) <$> evalParser p <*> pure args
    do_step prefs arg argt = tryP
                           . (`runStateT` argt)
                           . (>>= maybe ((lift . parseError) arg) return)
                           . disamb (not (prefDisambiguate prefs))
                           $ stepParser prefs arg p

disamb :: Monad m => Bool -> ListT m a -> m (Maybe a)
disamb allow_amb xs = do
  xs' <- runListT . takeListT (if allow_amb then 1 else 2) $ xs
  return $ case xs' of
    [x] -> Just x
    _   -> Nothing

parseError :: MonadP m => String -> m a
parseError arg = errorP . ErrorMsg $ msg
  where
    msg = case arg of
      ('-':_) -> "Invalid option `" ++ arg ++ "'"
      _       -> "Invalid argument `" ++ arg ++ "'"

runParserFully :: MonadP m => Parser a -> Args -> m a
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
    go m d f (OptP opt)
      | optVisibility opt > Internal
      = Leaf (f (OptHelpInfo m d) opt)
      | otherwise
      = MultNode []
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
