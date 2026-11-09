{-# LANGUAGE RankNTypes #-}

module Options.Applicative.Internal
  ( P
  , MonadP(..)
  , ParseError(..)

  , uncons
  , hoistMaybe
  , hoistEither
  , runReadM
  , withReadM

  , runP
  , runPWithGlobals
  , ContextStep(..)

  , Completion
  , runCompletion
  , contextNames

  , ListT
  , takeListT
  , runListT
  , hoistList

  , NondetT
  , cut
  , (<!>)
  , disamb

  , mapParserOptions
  ) where

import Control.Applicative
import Prelude
import Control.Monad (MonadPlus(..), liftM, ap, guard)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Except
  (runExcept, runExceptT, withExcept, ExceptT(..), throwE)
import Control.Monad.Trans.Reader
  (mapReaderT, runReader, runReaderT, Reader, ReaderT, ask)
import Control.Monad.Trans.State (StateT, get, put, modify, evalStateT, runStateT)


import Options.Applicative.Types

class (Alternative m, MonadPlus m) => MonadP m where
  enterContext :: String -> ParserInfo a -> m ()
  enterContextInline :: [ContextStep] -> String -> ParserInfo a -> m ()
  enterContextInline _ = enterContext
  exitContext :: m ()
  continueContext :: m ()
  -- | Record only the structural choices used to reach an option.
  -- The default leaves custom parsing/completion monads unchanged.
  selectContext :: [ContextStep] -> m ()
  selectContext _ = pure ()
  getPrefs :: m ParserPrefs

  missingArgP :: ParseError -> Completer -> m a
  errorP :: ParseError -> m a
  exitP :: ArgPolicy -> Parser b -> Maybe a -> m a

-- The help tree follows the residual parser's shape, but retains consumed
-- options. In particular an accepted alternative drops only its sibling.
-- Bind continuations retain their original help: reevaluating a continuation
-- as consumed history can duplicate repeated options or lose supplied options.
data ContextStep
  = MultLeft | MultRight | AltLeft | AltRight | BindFirst
  | BindNext

data GlobalTree
  = GlobalLeaf (Parser ())
  | GlobalMult GlobalTree GlobalTree
  | GlobalAlt GlobalTree GlobalTree

globalTree :: Parser a -> GlobalTree
globalTree (MultP p q) = GlobalMult (globalTree p) (globalTree q)
globalTree (AltP p q) = GlobalAlt (globalTree p) (globalTree q)
globalTree p = GlobalLeaf (() <$ p)

globalParser :: GlobalTree -> Parser ()
globalParser (GlobalLeaf p) = p
globalParser (GlobalMult p q) = globalParser p *> globalParser q
globalParser (GlobalAlt p q) = globalParser p <|> globalParser q

selectGlobal :: [ContextStep] -> GlobalTree -> GlobalTree
selectGlobal [] tree = tree
selectGlobal (MultLeft : path) (GlobalMult p q) = GlobalMult (selectGlobal path p) q
selectGlobal (MultRight : path) (GlobalMult p q) = GlobalMult p (selectGlobal path q)
selectGlobal (AltLeft : path) (GlobalAlt p _) = selectGlobal path p
selectGlobal (AltRight : path) (GlobalAlt _ q) = selectGlobal path q
-- Disambiguation may inspect a second match after the first. A failed or
-- ambiguous parse must not acquire a new parsing failure from help bookkeeping.
selectGlobal _ tree = tree

-- Inline subparsers continue inside the enclosing residual parser. Their
-- anchor locates that subtree without comparing command names or options.
data GlobalContext = GlobalContext (Maybe [ContextStep]) GlobalTree

inlineAnchor :: [ContextStep] -> [ContextStep]
inlineAnchor [] = []
inlineAnchor (AltLeft : path) = inlineAnchor path
inlineAnchor (AltRight : path) = inlineAnchor path
inlineAnchor (BindNext : path) = inlineAnchor path
inlineAnchor (step : path) = step : inlineAnchor path

stripAnchor :: [ContextStep] -> [ContextStep] -> Maybe [ContextStep]
stripAnchor [] path = Just path
stripAnchor (MultLeft : xs) (MultLeft : ys) = stripAnchor xs ys
stripAnchor (MultRight : xs) (MultRight : ys) = stripAnchor xs ys
stripAnchor (BindFirst : xs) (BindFirst : ys) = stripAnchor xs ys
stripAnchor _ _ = Nothing

selectGlobals :: [ContextStep] -> [GlobalContext] -> [GlobalContext]
selectGlobals _ [] = []
selectGlobals path (GlobalContext Nothing tree : rest) =
  GlobalContext Nothing (selectGlobal path tree) : rest
selectGlobals path (context@(GlobalContext (Just anchor) tree) : rest) =
  case stripAnchor anchor path of
    Just relative -> GlobalContext (Just anchor) (selectGlobal relative tree) : rest
    Nothing -> context : selectGlobals path rest

contextGlobalParser :: GlobalContext -> Parser ()
contextGlobalParser (GlobalContext _ tree) = globalParser tree

newtype P a = P (ExceptT ParseError
  (StateT (IsCmdStart, [Context], [GlobalContext]) (Reader ParserPrefs)) a)

instance Functor P where
  fmap f (P m) = P $ fmap f m

instance Applicative P where
  pure a = P $ pure a
  P f <*> P a = P $ f <*> a

instance Alternative P where
  empty = P empty
  P x <|> P y = P $ x <|> y

instance Monad P where
  return = pure
  P x >>= k = P $ x >>= \a -> case k a of P y -> y

instance MonadPlus P where
  mzero = P mzero
  mplus (P x) (P y) = P $ mplus x y

contextNames :: [Context] -> [String]
contextNames ns =
  let go (Context n _) = n
  in  reverse $ go <$> ns

instance MonadP P where
  enterContext name pinfo = P $ lift $ modify $ \(_, ctxs, gs) ->
    (CmdStart, Context name pinfo : ctxs, GlobalContext Nothing (globalTree (infoParser pinfo)) : gs)
  enterContextInline path name pinfo = P $ lift $ modify $ \(_, ctxs, gs) ->
    (CmdStart, Context name pinfo : ctxs,
      GlobalContext (Just (inlineAnchor path)) (globalTree (infoParser pinfo)) : gs)
  exitContext = P $ lift $ modify $ \(start, ctxs, gs) -> (start, drop 1 ctxs, drop 1 gs)
  continueContext = P $ lift $ modify $ \(_, ctxs, gs) -> (CmdCont, ctxs, gs)
  selectContext path = P $ do
    settings <- lift . lift $ ask
    if prefHelpShowGlobal settings
      then lift $ modify $ \(start, ctxs, gs) ->
        (start, ctxs, selectGlobals path gs)
      else pure ()
  getPrefs = P . lift . lift $ ask

  missingArgP e _ = errorP e
  errorP = P . throwE
  exitP _ p ma = P $ do
    (isCmdStart, _, _) <- lift get
    maybe (throwE . MissingError isCmdStart . SomeParser $ p) return ma



hoistMaybe :: MonadPlus m => Maybe a -> m a
hoistMaybe = maybe mzero return

hoistEither :: MonadP m => Either ParseError a -> m a
hoistEither = either errorP return

runP :: P a -> ParserPrefs -> (Either ParseError a, [Context])
runP p settings =
  let (result, contexts, _) = runPWithGlobals p settings (pure ())
  in (result, contexts)

-- Original contexts remain intact for usage and errors; only global help uses
-- the parallel projections. runP's public result and Context are unchanged.
runPWithGlobals :: P a -> ParserPrefs -> Parser b
                -> (Either ParseError a, [Context], [Parser ()])
runPWithGlobals (P p) settings root =
  let (result, (_, contexts, globals)) = runReader
        (runStateT (runExceptT p) (CmdStart, [], [GlobalContext Nothing (globalTree root)])) settings
  in (result, contexts, map contextGlobalParser globals)


uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x : xs) = Just (x, xs)

runReadM :: MonadP m => ReadM a -> String -> m a
runReadM (ReadM r) s = hoistEither . runExcept $ runReaderT r s

withReadM :: (String -> String) -> ReadM a -> ReadM a
withReadM f = ReadM . mapReaderT (withExcept f') . unReadM
  where
    f' (ErrorMsg err) = ErrorMsg (f err)
    f' e = e

data ComplResult a
  = ComplParser SomeParser ArgPolicy
  | ComplOption Completer
  | ComplResult a

instance Functor ComplResult where
  fmap = liftM

instance Applicative ComplResult where
  pure = ComplResult
  (<*>) = ap

instance Monad ComplResult where
  return = pure
  m >>= f = case m of
    ComplResult r -> f r
    ComplParser p a -> ComplParser p a
    ComplOption c -> ComplOption c

newtype Completion a =
  Completion (ExceptT ParseError (ReaderT ParserPrefs ComplResult) a)

instance Functor Completion where
  fmap f (Completion m) = Completion $ fmap f m

instance Applicative Completion where
  pure a = Completion $ pure a
  Completion f <*> Completion a = Completion $ f <*> a

instance Alternative Completion where
  empty = Completion empty
  Completion x <|> Completion y = Completion $ x <|> y

instance Monad Completion where
  return = pure
  Completion x >>= k = Completion $ x >>= \a -> case k a of Completion y -> y

instance MonadPlus Completion where
  mzero = Completion mzero
  mplus (Completion x) (Completion y) = Completion $ mplus x y

instance MonadP Completion where
  enterContext _ _ = return ()
  exitContext = return ()
  continueContext = return ()
  getPrefs = Completion $ lift ask

  missingArgP _ = Completion . lift . lift . ComplOption
  exitP a p _ = Completion . lift . lift $ ComplParser (SomeParser p) a
  errorP = Completion . throwE

runCompletion :: Completion r -> ParserPrefs -> Maybe (Either (SomeParser, ArgPolicy) Completer)
runCompletion (Completion c) prefs = case runReaderT (runExceptT c) prefs of
  ComplResult _ -> Nothing
  ComplParser p' a' -> Just $ Left (p', a')
  ComplOption compl -> Just $ Right compl

-- A "ListT done right" implementation

newtype ListT m a = ListT
  { stepListT :: m (TStep a (ListT m a)) }

data TStep a x
  = TNil
  | TCons a x

bimapTStep :: (a -> b) -> (x -> y) -> TStep a x -> TStep b y
bimapTStep _ _ TNil = TNil
bimapTStep f g (TCons a x) = TCons (f a) (g x)

takeListT :: Monad m => Int -> ListT m a -> ListT m a
takeListT 0 = const mzero
takeListT n = ListT . liftM (bimapTStep id (takeListT (n - 1))) . stepListT

runListT :: Monad m => ListT m a -> m [a]
runListT xs = do
  s <- stepListT xs
  case s of
    TNil -> return []
    TCons x xt -> liftM (x :) (runListT xt)

instance Monad m => Functor (ListT m) where
  fmap f = ListT
         . liftM (bimapTStep f (fmap f))
         . stepListT

instance Monad m => Applicative (ListT m) where
  pure a = ListT (return (TCons a mzero))
  (<*>) = ap

instance Monad m => Monad (ListT m) where
  return = pure
  xs >>= f = ListT $ do
    s <- stepListT xs
    case s of
      TNil -> return TNil
      TCons x xt -> stepListT $ f x `mplus` (xt >>= f)

instance Monad m => Alternative (ListT m) where
  empty = mzero
  (<|>) = mplus

instance MonadTrans ListT where
  lift = ListT . liftM (`TCons` mzero)

instance Monad m => MonadPlus (ListT m) where
  mzero = ListT (return TNil)
  mplus xs ys = ListT $ do
    s <- stepListT xs
    case s of
      TNil -> stepListT ys
      TCons x xt -> return $ TCons x (xt `mplus` ys)

-- nondeterminism monad with cut operator

newtype NondetT m a = NondetT
  { runNondetT :: ListT (StateT Bool m) a }

instance Monad m => Functor (NondetT m) where
  fmap f = NondetT . fmap f . runNondetT

instance Monad m => Applicative (NondetT m) where
  pure = NondetT . pure
  NondetT m1 <*> NondetT m2 = NondetT (m1 <*> m2)

instance Monad m => Monad (NondetT m) where
  return = pure
  NondetT m1 >>= f = NondetT $ m1 >>= runNondetT . f

instance Monad m => MonadPlus (NondetT m) where
  mzero = NondetT mzero
  NondetT m1 `mplus` NondetT m2 = NondetT (m1 `mplus` m2)

instance Monad m => Alternative (NondetT m) where
  empty = mzero
  (<|>) = mplus

instance MonadTrans NondetT where
  lift = NondetT . lift . lift

(<!>) :: Monad m => NondetT m a -> NondetT m a -> NondetT m a
(<!>) m1 m2 = NondetT . mplus (runNondetT m1) $ do
  s <- lift get
  guard (not s)
  runNondetT m2

cut :: Monad m => NondetT m ()
cut = NondetT $ lift (put True)

disamb :: Monad m => Bool -> NondetT m a -> m (Maybe a)
disamb allow_amb xs = do
  xs' <- (`evalStateT` False)
       . runListT
       . takeListT (if allow_amb then 1 else 2)
       . runNondetT $ xs
  return $ case xs' of
    [x] -> Just x
    _   -> Nothing

hoistList :: Alternative m => [a] -> m a
hoistList = foldr cons empty
  where
    cons x xs = pure x <|> xs

-- | Maps an Option modifying function over the Parser.
--
-- @since 0.19.0.0
mapParserOptions :: (forall x. Option x -> Option x) -> Parser a -> Parser a
mapParserOptions f = go
  where
    go :: forall y. Parser y -> Parser y
    go (NilP x) = NilP x
    go (OptP o) = OptP (f o)
    go (MultP p1 p2) = MultP (go p1) (go p2)
    go (AltP p1 p2) = AltP (go p1) (go p2)
    go (BindP p1 p2) = BindP (go p1) (\x -> go (p2 x))
