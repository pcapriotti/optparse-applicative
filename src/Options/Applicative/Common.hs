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
  InvokedWith,
  mkInvokedWith,
  runParserInfo,
  runParserInfoEnv,
  runParserFully,
  runParserFullyEnv,
  runParserStep,
  runParserStepEnv,
  runParser,
  runParserEnv,
  evalParser,

  -- * Low-level utilities
  mapParser,
  treeMapParser,
  optionNames
  ) where

import Control.Applicative
import Control.Monad (guard, join, mzero, msum, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..), get, gets, modify, runStateT)
import Data.List (find, isPrefixOf)
import Data.Maybe (catMaybes, listToMaybe, maybeToList, isJust, isNothing)
import Prelude

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

data InvokedWith = InvokedWith
  { _invArgs :: Args
  , _invEnv :: Env
  }

mkInvokedWith :: Args -> Env -> InvokedWith
mkInvokedWith = InvokedWith

setInvArgs :: Args -> InvokedWith -> InvokedWith
setInvArgs args inv = inv{_invArgs = args}

optMatches :: MonadP m => Bool -> OptReader a -> OptWord -> Maybe (StateT InvokedWith m a)
optMatches disambiguate opt (OptWord arg1 val) = case opt of
  OptReader names rdr no_arg_err -> do
    guard $ has_name arg1 names
    Just $ do
      args <- gets _invArgs
      let mb_args = uncons $ maybeToList val ++ args
      let missing_arg = missingArgP (no_arg_err $ showOption arg1) (crCompleter rdr)
      (arg', args') <- maybe (lift missing_arg) return mb_args
      modify $ setInvArgs args'
      lift $ runReadM (withReadM (errorFor arg1) (crReader rdr)) arg'

  FlagReader names x -> do
    guard $ has_name arg1 names
    -- #242 Flags/switches succeed incorrectly when given an argument.
    -- We'll not match a long option for a flag if there's a word attached.
    -- This was revealing an implementation detail as
    -- `--foo=val` was being parsed as `--foo -val`, which is gibberish.
    guard $ isShortName arg1 || isNothing val
    Just $ do
      args <- gets _invArgs
      let val' = ('-' :) <$> val
      modify $ setInvArgs $ maybeToList val' ++ args
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
             => (forall r . Option r -> NondetT m (Parser r))
             -> Parser a -> NondetT m (Parser a)
searchParser _ (NilP _) = mzero
searchParser f (OptP opt) = f opt
searchParser _ (EnvP _ _) = mzero
searchParser f (MultP p1 p2) = foldr1 (<!>)
  [ do p1' <- searchParser f p1
       return (p1' <*> p2)
  , do p2' <- searchParser f p2
       return (p1 <*> p2') ]
searchParser f (AltP p1 p2) = msum
  [ searchParser f p1
  , searchParser f p2 ]
searchParser f (BindP p k) = msum
  [ do p' <- searchParser f p
       return $ BindP p' k
  , case evalParser p of
      Nothing -> mzero
      Just aa -> searchParser f (k aa) ]

searchOpt :: MonadP m => ParserPrefs -> OptWord -> Parser a
          -> NondetT (StateT InvokedWith m) (Parser a)
searchOpt pprefs w = searchParser $ \opt -> do
  let disambiguate = prefDisambiguate pprefs
                  && optVisibility opt > Internal
  case optMatches disambiguate (optMain opt) w of
    Just matcher -> lift $ fmap pure matcher
    Nothing -> mzero

searchArg :: MonadP m => ParserPrefs -> String -> Parser a
          -> NondetT (StateT InvokedWith m) (Parser a)
searchArg prefs arg =
  searchParser $ \opt -> do
    when (isArg (optMain opt)) cut
    case optMain opt of
      CmdReader _ _ f ->
        case (f arg, prefBacktrack prefs) of
          (Just subp, NoBacktrack) -> lift $ do
            inv <- get <* modify (setInvArgs [])
            fmap pure . lift $ enterContext arg subp *> runParserInfoEnv subp inv <* exitContext
          (Just subp, Backtrack) -> fmap pure . lift . StateT $ \inv ->
            enterContext arg subp
              *> runParserEnv (infoPolicy subp) CmdStart (infoParser subp) inv
              <* exitContext

          (Just subp, SubparserInline) -> lift $ do
            lift $ enterContext arg subp
            return $ infoParser subp

          (Nothing, _)  -> mzero
      ArgReader rdr ->
        fmap pure . lift . lift $ runReadM (crReader rdr) arg
      _ -> mzero

-- | Lookup environment for option value
searchEnv :: MonadP m => [String] -> ReadM a -> Env -> m a
searchEnv vars rdr env = do
  maybe mzero do_read $ listToMaybe $ catMaybes $ map (`lookupVar` env) vars
  where
    do_read (var,val) = runReadM (withReadM (error_for var) rdr) val
    lookupVar var = find $ (== var) . fst
    error_for var msg = "environment variable " ++ show var ++ ": " ++ msg

stepParser :: MonadP m => ParserPrefs -> ArgPolicy -> String
           -> Parser a -> NondetT (StateT InvokedWith m) (Parser a)
stepParser _ _ _ (EnvP vars rdr) =
  lift $ pure <$> (gets _invEnv >>= lift . searchEnv vars rdr)
stepParser pprefs AllPositionals arg p =
  searchArg pprefs arg p
stepParser pprefs ForwardOptions arg p = case parseWord arg of
  Just w -> searchOpt pprefs w p <|> searchArg pprefs arg p
  Nothing -> searchArg pprefs arg p
stepParser pprefs _ arg p = case parseWord arg of
  Just w -> searchOpt pprefs w p
  Nothing -> searchArg pprefs arg p


runParser :: MonadP m => ArgPolicy -> IsCmdStart -> Parser a -> Args -> m (a,Args)
runParser policy isCmdStart p args =
  fmap _invArgs <$> runParserEnv policy isCmdStart p (InvokedWith args [])

-- | Apply a 'Parser' to a command line, and return a result and leftover
-- arguments.  This function returns an error if any parsing error occurs, or
-- if any options are missing and don't have a default value.
runParserEnv :: MonadP m
            => ArgPolicy
            -> IsCmdStart
            -> Parser a
            -> InvokedWith
            -> m (a, InvokedWith)
runParserEnv policy _ p (InvokedWith ("--" : argt) env)
  | policy /= AllPositionals
  = runParserEnv AllPositionals CmdCont p (InvokedWith argt env)
runParserEnv policy isCmdStart p inv@(InvokedWith args env) = case args of
  [] -> result >>= exitP isCmdStart policy p
  (arg : argt) -> do
    (mp', args') <- do_step arg argt
    case mp' of
      Nothing -> (result >>= hoistMaybe) <|> parseError arg p
      Just p' -> runParserEnv (newPolicy arg) CmdCont p' (InvokedWith args' env)
  where
    result = do
      def <- defaultValueEnv env p
      pure $ flip (,) inv <$> def
    do_step arg =
      runParserStepEnv policy p arg env
    newPolicy a = case policy of
      NoIntersperse -> if isJust (parseWord a) then NoIntersperse else AllPositionals
      x             -> x

runParserStep :: MonadP m => ArgPolicy -> Parser a -> String -> Args -> m (Maybe (Parser a), Args)
runParserStep policy p arg = runParserStepEnv policy p arg []

runParserStepEnv :: MonadP m
                 => ArgPolicy
                 -> Parser a
                 -> String
                 -> Env
                 -> Args
                 -> m (Maybe (Parser a), Args)
runParserStepEnv policy p arg env args = do
  prefs <- getPrefs
  fmap (fmap _invArgs)
    $ flip runStateT (InvokedWith args env)
    $ disamb (not (prefDisambiguate prefs))
    $ stepParser prefs policy arg p

parseError :: MonadP m => String -> Parser x -> m a
parseError arg = errorP . UnexpectedError arg . SomeParser

runParserInfo :: MonadP m => ParserInfo a -> Args -> m a
runParserInfo i args = runParserInfoEnv i $ InvokedWith args []

runParserInfoEnv :: MonadP m => ParserInfo a -> InvokedWith -> m a
runParserInfoEnv i = runParserFullyEnv (infoPolicy i) (infoParser i)

runParserFully :: MonadP m => ArgPolicy -> Parser a -> Args -> m a
runParserFully policy p args = runParserFullyEnv policy p (InvokedWith args [])

runParserFullyEnv :: MonadP m => ArgPolicy -> Parser a -> InvokedWith -> m a
runParserFullyEnv policy p inv = do
  (r, inv') <- runParserEnv policy CmdStart p inv
  case _invArgs inv' of
    []  -> return r
    a:_ -> parseError a (pure ())

-- | The default value of a 'Parser'.  This function returns an error if any of
-- the options don't have a default value.
evalParser :: Parser a -> Maybe a
evalParser (NilP r) = r
evalParser (OptP _) = Nothing
evalParser (EnvP _ _) = Nothing
evalParser (MultP p1 p2) = evalParser p1 <*> evalParser p2
evalParser (AltP p1 p2) = evalParser p1 <|> evalParser p2
evalParser (BindP p k) = evalParser p >>= evalParser . k

-- | Either value from environment or default value of a 'Parser'.
defaultValueEnv :: (MonadP m) => Env -> Parser a -> m (Maybe a)
defaultValueEnv _ (NilP r) = pure r
defaultValueEnv _ (OptP _) = pure Nothing
defaultValueEnv env (EnvP vars rdr) = Just <$> searchEnv vars rdr env
defaultValueEnv env (MultP p1 p2) =
  liftA2 (<*>) (defaultValueEnv env p1) (defaultValueEnv env p2)
defaultValueEnv env (AltP p1 p2) =
  liftA2 (<|>) (defaultValueEnv env p1) (defaultValueEnv env p2)
defaultValueEnv env (BindP p k) =
  fmap join $ defaultValueEnv env p >>= mapM (defaultValueEnv env . k)

-- | Map a polymorphic function over all the options of a parser, and collect
-- the results in a list.
mapParser :: (forall x. ArgumentReachability -> Option x -> b)
          -> Parser a -> [b]
mapParser f = flatten . treeMapParser f
  where
    flatten (Leaf x) = [x]
    flatten (MultNode xs) = xs >>= flatten
    flatten (AltNode _ xs) = xs >>= flatten
    flatten (BindNode x) = flatten x

-- | Like 'mapParser', but collect the results in a tree structure.
treeMapParser :: (forall x. ArgumentReachability -> Option x -> b)
          -> Parser a
          -> OptTree b
treeMapParser g = simplify . go False g
  where
    has_default :: Parser a -> Bool
    has_default p = isJust (evalParser p)

    go :: Bool
       -> (forall x. ArgumentReachability -> Option x -> b)
       -> Parser a
       -> OptTree b
    go _ _ (NilP _) = MultNode []
    go r f (OptP opt)
      | optVisibility opt > Internal
      = Leaf (f (ArgumentReachability r) opt)
      | otherwise
      = MultNode []
    go _ _ (EnvP _ _) = MultNode []
    go r f (MultP p1 p2) =
      MultNode [go r f p1, go r' f p2]
      where r' = r || hasArg p1
    go r f (AltP p1 p2) =
      AltNode altNodeType [go r f p1, go r f p2]
      where
        -- The 'AltNode' indicates if one of the branches has a default.
        -- This is used for rendering brackets, as well as filtering
        -- out optional arguments when generating the "missing:" text.
        altNodeType =
          if has_default p1 || has_default p2
            then MarkDefault
            else NoDefault

    go r f (BindP p k) =
      let go' = go r f p
      in case evalParser p of
        Nothing -> BindNode go'
        Just aa -> BindNode (MultNode [ go', go r f (k aa) ])

    hasArg :: Parser a -> Bool
    hasArg (NilP _) = False
    hasArg (OptP p) = (isArg . optMain) p
    hasArg (EnvP _ _) = False
    hasArg (MultP p1 p2) = hasArg p1 || hasArg p2
    hasArg (AltP p1 p2) = hasArg p1 || hasArg p2
    hasArg (BindP p _) = hasArg p

simplify :: OptTree a -> OptTree a
simplify (Leaf x) = Leaf x
simplify (MultNode xs) =
  case concatMap (remove_mult . simplify) xs of
    [x] -> x
    xs' -> MultNode xs'
  where
    remove_mult (MultNode ts) = ts
    remove_mult t = [t]
simplify (AltNode b xs) =
  AltNode b (concatMap (remove_alt . simplify) xs)
  where
    remove_alt (AltNode _ ts) = ts
    remove_alt (MultNode []) = []
    remove_alt t = [t]
simplify (BindNode x) =
  BindNode $ simplify x
