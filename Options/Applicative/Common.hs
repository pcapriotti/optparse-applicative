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
  --
  -- A 'ParserPrefs' contains general preferences for all command-line
  -- options, and can be built with the 'prefs' function.
  ParserInfo(..),
  ParserPrefs(..),

  -- * Running parsers
  runParserInfo,
  runParserFully,
  runParser,
  evalParser,

  -- * Low-level utilities
  mapParser,
  treeMapParser,
  optionNames,
  optDesc,
  OptDescStyle (..)
  ) where

import Control.Applicative (pure, (<*>), (<*), (<$>), (<|>), (<$))
import Control.Arrow (left)
import Control.Monad (guard, mzero, msum, when, liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..), get, put, runStateT)
import Data.List (isPrefixOf, sort, intersperse)
import Data.Maybe (maybeToList)
import Data.Monoid (Monoid(..))

import Options.Applicative.Internal
import Options.Applicative.Types

import Options.Applicative.Help.Pretty
import Options.Applicative.Help.Chunk

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

argMatches :: MonadP m => OptReader a -> String
           -> Maybe (StateT Args m a)
argMatches opt arg = case opt of
  ArgReader rdr -> Just $ do
    result <- lift $ runReadM (crReader rdr) arg
    return result
  CmdReader _ f ->
    flip fmap (f arg) $ \subp -> StateT $ \args -> do
      enterContext arg subp
      prefs <- getPrefs
      let runSubparser
            | prefBacktrack prefs = \i a ->
                runParser (getPolicy i) (infoParser i) a
            | otherwise = \i a
            -> (,) <$> runParserInfo i a <*> pure []
      runSubparser subp args <* exitContext
  _ -> Nothing

optMatches :: MonadP m => Bool -> OptReader a -> OptWord -> Maybe (StateT Args m a)
optMatches disambiguate opt (OptWord arg1 val) = case opt of
  OptReader names rdr no_arg_err -> do
    guard $ has_name arg1 names
    Just $ do
      args <- get
      let mb_args = uncons $ maybeToList val ++ args
      let missing_arg = lift $ missingArgP no_arg_err (crCompleter rdr)
      (arg', args') <- maybe missing_arg return mb_args
      put args'
      lift $ runReadM (withReadM (errorFor arg1) (crReader rdr)) arg'
  FlagReader names x -> do
    guard $ has_name arg1 names
    Just $ do
      args <- get
      let val' = (\s -> '-' : s) <$> val
      put $ maybeToList val' ++ args
      return x
  _ -> Nothing
  where
    errorFor name msg = "option " ++ showOption name ++ ": " ++ msg

    has_name a
      | disambiguate = any (isOptionPrefix a)
      | otherwise = elem a

isArg :: OptReader a -> Bool
isArg (ArgReader _) = True
isArg _ = False

data OptWord = OptWord OptName (Maybe String)

parseWord :: String -> Maybe OptWord
parseWord ('-' : '-' : w) = Just $ let
  (opt, arg) = case span (/= '=') w of
    (_, "") -> (w, Nothing)
    (w', _ : rest) -> (w', Just rest)
  in OptWord (OptLong opt) arg
parseWord ('-' : w) = case w of
  [] -> Nothing
  (a : rest) -> Just $ let
    arg = rest <$ guard (not (null rest))
    in OptWord (OptShort a) arg
parseWord _ = Nothing

searchParser :: Monad m
             => ParserPrefs
             -> (forall r . Option r -> NondetT m r)
             -> Parser a -> NondetT m (Parser a)
searchParser _ _ (NilP _) = mzero
searchParser _ f (OptP opt) = liftM pure (f opt)
searchParser pprefs f (MultP p1 p2) = foldr1 (<!>)
  [ do p1' <- searchParser pprefs f p1
       return (p1' <*> p2)
  , do p2' <- searchParser pprefs f p2
       return (p1 <*> p2') ]
searchParser pprefs f (AltP p1 p2) = msum
  [ searchParser pprefs f p1
  , searchParser pprefs f p2 ]
searchParser pprefs f (BindP p k) = do
  p' <- searchParser pprefs f p
  case (evalParser False False (optDesc pprefs missingStyle) p') of
    Left _ -> mzero
    Right aa -> pure $ k aa

searchOpt :: MonadP m => ParserPrefs -> OptWord -> Parser a
          -> NondetT (StateT Args m) (Parser a)
searchOpt pprefs w = searchParser pprefs $ \opt -> do
  let disambiguate = prefDisambiguate pprefs
                  && optVisibility opt > Internal
  case optMatches disambiguate (optMain opt) w of
    Just matcher -> lift matcher
    Nothing -> mzero

searchArg :: MonadP m => ParserPrefs -> String -> Parser a
          -> NondetT (StateT Args m) (Parser a)
searchArg pprefs arg = searchParser pprefs $ \opt -> do
  when (isArg (optMain opt)) cut
  case argMatches (optMain opt) arg of
    Just matcher -> lift matcher
    Nothing -> mzero

stepParser :: MonadP m => ParserPrefs -> ArgPolicy -> String
           -> Parser a -> NondetT (StateT Args m) (Parser a)
stepParser pprefs SkipOpts arg p = case parseWord arg of
  Just w -> searchOpt pprefs w p
  Nothing -> searchArg pprefs arg p
stepParser pprefs AllowOpts arg p = msum
  [ searchArg pprefs arg p
  , do w <- hoistMaybe (parseWord arg)
       searchOpt pprefs w p ]

-- | Apply a 'Parser' to a command line, and return a result and leftover
-- arguments.  This function returns an error if any parsing error occurs, or
-- if any options are missing and don't have a default value.
runParser :: MonadP m => ArgPolicy -> Parser a -> Args -> m (a, Args)
runParser SkipOpts p ("--" : argt) = runParser AllowOpts p argt
runParser policy p args = case args of
  [] -> do
    prefs <- getPrefs
    exitP p $ MissingError `left` result prefs
  (arg : argt) -> do
    prefs <- getPrefs
    (mp', args') <- do_step prefs arg argt
    case mp' of
      Nothing -> hoistEither (MissingError `left` (result prefs)) <|> parseError arg
      Just p' -> runParser policy p' args'
  where
    result (prefs') = (,) <$> evalParser False False (optDesc prefs' missingStyle) p <*> pure args
    do_step prefs arg argt = (`runStateT` argt)
                           . disamb (not (prefDisambiguate prefs))
                           $ stepParser prefs policy arg p

parseError :: MonadP m => String -> m a
parseError arg = errorP . ErrorMsg $ msg
  where
    msg = case arg of
      ('-':_) -> "Invalid option `" ++ arg ++ "'"
      _       -> "Invalid argument `" ++ arg ++ "'"

getPolicy :: ParserInfo a -> ArgPolicy
getPolicy i = if infoIntersperse i
  then SkipOpts
  else AllowOpts

runParserInfo :: MonadP m => ParserInfo a -> Args -> m a
runParserInfo i = runParserFully (getPolicy i) (infoParser i)

runParserFully :: MonadP m => ArgPolicy -> Parser a -> Args -> m a
runParserFully policy p args = do
  (r, args') <- runParser policy p args
  guard $ null args'
  return r

-- | The default value of a 'Parser'.  This function returns an error if any of
-- the options don't have a default value.
evalParser :: Bool -> Bool
        -> (forall x . OptHelpInfo -> Option x -> b)
        -> Parser a
        -> Either (OptTree b) a
evalParser _ _ _ (NilP r) = maybeToEither (MultNode []) r
evalParser m d f (OptP opt)
      | optVisibility opt > Internal
      = Left $ Leaf (f (OptHelpInfo m d) opt)
      | otherwise
      = Left $ MultNode []
evalParser m d f (MultP p1 p2) = case evalParser m d f p1 <*> evalParser m d f p2 of
  Right a -> Right a
  Left _  -> case (evalParser m d f p1, evalParser m d f p2) of
    (Left a', Left b') -> Left $ MultNode [a', b']
    (Left a', _)       -> Left $ MultNode [a']
    (_, Left b')       -> Left $ MultNode [b']
    _                  -> Left $ MultNode []
evalParser m d f (AltP p1 p2) = case (evalParser m d f p1, evalParser m d f p2) of
  (Right a', _)        -> Right a'
  (_, Right b')        -> Right b'
  (Left a', Left b')   -> Left $ AltNode [a', b']
evalParser _ d f (BindP p k) = evalParser True d f p >>= (evalParser True d f) . k

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
    has_default p = either (const False) (const True) (evalParser False False g p)

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


-- | Style for rendering an option.
data OptDescStyle = OptDescStyle
  { descSep :: Doc
  , descHidden :: Bool
  , descSurround :: Bool }

-- | Generate description for a single option.
optDesc :: ParserPrefs -> OptDescStyle -> OptHelpInfo -> Option a -> Chunk Doc
optDesc pprefs style info opt =
  let ns = optionNames $ optMain opt
      mv = stringChunk $ optMetaVar opt
      descs = map (string . showOption) (sort ns)
      desc' = listToChunk (intersperse (descSep style) descs) <<+>> mv
      show_opt
        | optVisibility opt == Hidden
        = descHidden style
        | otherwise
        = optVisibility opt == Visible
      suffix
        | hinfoMulti info
        = stringChunk . prefMultiSuffix $ pprefs
        | otherwise
        = mempty
      render chunk
        | not show_opt
        = mempty
        | isEmpty chunk || not (descSurround style)
        = mappend chunk suffix
        | hinfoDefault info
        = mappend (fmap brackets chunk) suffix
        | null (drop 1 descs)
        = mappend chunk suffix
        | otherwise
        = mappend (fmap parens chunk) suffix
  in render desc'

missingStyle :: OptDescStyle
missingStyle = OptDescStyle
  { descSep = string "|"
  , descHidden = False
  , descSurround = True }

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither = flip maybe Right . Left
