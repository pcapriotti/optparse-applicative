{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances #-}
module Options.Applicative.Basic where

import Control.Applicative
import Control.Alternative.FreeStar
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Trans.Except
import Data.Bifunctor
import Data.Bimonoid
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Monoid

import Options.Applicative.Help.Chunk
import Options.Applicative.Help.Pretty
import Options.Applicative.Usage
import Options.Applicative.Types

class Pretty1 f where
  pretty1 :: f a -> Doc

instance Pretty1 Identity where
  pretty1 _ = mempty

data ParserState a = ParserState
  { pendingArgs :: [a]
  , skippedArgs :: [a] }
  deriving (Eq, Ord, Read, Show)

newtype ArgParser a = ArgParser
  { unArgParser :: StateT (ParserState String) (Except ParseError) a }
  deriving ( Functor, Applicative, Monad, Alternative, MonadPlus
           , MonadState (ParserState String), MonadError ParseError )

evalArgParser :: ArgParser a -> [String] -> Either ParseError a
evalArgParser p args = runExcept . (`evalStateT` st0) . unArgParser $ p
  where
    st0 = ParserState args []

errMsg :: String -> ArgParser a
errMsg = ArgParser . lift . throwE . ErrorMsg

nextArg :: ArgParser String
nextArg = (tryNextArg >>= hoistMaybe)
       <|> errMsg "argument required"

tryNextArg :: ArgParser (Maybe String)
tryNextArg = do
  xs <- gets pendingArgs
  case xs of
    [] -> pure Nothing
    (x : xt) -> Just x <$ modify (\s -> s { pendingArgs = xt })

skipArg :: String -> ArgParser ()
skipArg arg = modify $ \s -> s { skippedArgs = arg : skippedArgs s }

resetArgs :: ArgParser ()
resetArgs = modify $ \s -> s
  { pendingArgs = reverse (skippedArgs s)
  , skippedArgs = [] }

data BaseOption a
  = BaseReg [OptName] (ReadM a)
  | BaseFlag [OptName] a
  | BaseCommand String a

newtype Argument a = Argument
  { unArgument :: ReadM a }
  deriving Functor

instance Functor BaseOption where
  fmap f (BaseReg n v) = BaseReg  n (fmap f v)
  fmap f (BaseFlag n x) = BaseFlag n (f x)
  fmap f (BaseCommand n x) = BaseCommand n (f x)

class Functor f => Opt f where
  optFind :: String -> f a -> Maybe (ArgParser a)

instance Pretty1 BaseOption where
  pretty1 (BaseReg n _) = pretty n </> string "ARG"
  pretty1 (BaseFlag n _) = pretty n
  pretty1 (BaseCommand arg _) = string arg

instance Opt BaseOption where
  optFind arg (BaseReg ns v)
    | matchNames arg ns = Just (argParser1 v)
  optFind arg (BaseFlag ns x)
    | matchNames arg ns = Just (pure x)
  optFind arg (BaseCommand cmd x)
    | arg == cmd = Just (pure x)
  optFind _ _ = empty

data WithDesc i f a = WithDesc
  { bundledDesc :: i
  , unWithDesc :: f a }
  deriving (Eq, Ord, Read, Show)

instance Functor f => Functor (WithDesc i f) where
  fmap f (WithDesc d x) = WithDesc d (fmap f x)

instance Opt f => Opt (WithDesc i f) where
  optFind arg (WithDesc _ x) = optFind arg x

instance Pretty i => Pretty1 (WithDesc i f) where
  pretty1 (WithDesc d _) = pretty d

instance Opt Identity where
  optFind _ _ = Nothing

data OptSum f g a
  = OptLeft (f a)
  | OptRight (g a)
  deriving (Eq, Ord, Read, Show)

instance (Functor f, Functor g) => Functor (OptSum f g) where
  fmap f (OptLeft x) = OptLeft (fmap f x)
  fmap f (OptRight y) = OptRight (fmap f y)

instance (Opt f, Opt g) => Opt (OptSum f g) where
  optFind arg (OptLeft x) = optFind arg x
  optFind arg (OptRight y) = optFind arg y

instance (Pretty1 f, Pretty1 g) => Pretty1 (OptSum f g) where
  pretty1 (OptLeft x) = pretty1 x
  pretty1 (OptRight y) = pretty1 y

evalParser :: Functor f => Alt f a -> Maybe a
evalParser = runAlt $ const Nothing

-- | Convert a 'Validate' value into an argument parser that consumes exactly
-- one argument.
argParser1 :: ReadM a -> ArgParser a
argParser1 v = do
  x <- nextArg
  ArgParser . lift $ runReaderT (unReadM v) x

matchNames :: String -> [OptName] -> Bool
matchNames = any . matchName

matchName :: String -> OptName -> Bool
matchName ('-':[c]) (OptShort n) = n == c
matchName ('-':'-':arg) (OptLong n) = n == arg
matchName _ _ = False

-- Day convolution
data Day f g a = forall x . Day (f x) (g (x -> a))

instance Functor g => Functor (Day f g) where
  fmap f (Day x g) = Day x (fmap (f.) g)

-- | A one-off alternative functor used to step a parser over a single argument.
--
-- The first field is a matching option, together with the remainder of the
-- parser.  The second field is the full parser.  The second field is needed to
-- deal with 'some' appropriately, and in case more than one option matches.
data Step p f a = Step (Maybe (Day p f a)) (f a)

instance Functor f => Functor (Step p f) where
  fmap f (Step x x') = Step (fmap (fmap f) x) (fmap f x')

instance Applicative f => Applicative (Step p f) where
  pure = Step Nothing . pure
  Step (Just (Day x f)) f' <*> Step _ y'
    = Step (Just (Day x (flip <$> f <*> y'))) (f' <*> y')
  Step Nothing f' <*> Step (Just (Day y g)) y'
    = Step (Just (Day y ((.) <$> f' <*> g))) (f' <*> y')
  Step Nothing f' <*> Step Nothing y' = Step Nothing (f' <*> y')

instance Alternative f => Alternative (Step p f) where
  empty = Step Nothing empty
  Step (Just x) x' <|> Step _ y' = Step (Just x) (x' <|> y')
  Step _ x' <|> Step (Just y) y' = Step (Just y) (x' <|> y')
  Step Nothing x <|> Step Nothing y = Step Nothing (x <|> y)

  many x = some x <|> pure []
  some (Step (Just (Day x f)) x')
    = Step (Just (Day x (u <$> f <*> many x'))) (some x')
    where u f0 as x0 = f0 x0 : as
  some (Step Nothing x) = Step Nothing (some x)

stepParser :: Opt f => String -> Alt f a -> Step ArgParser (Alt f) a
stepParser arg = runAlt $ \opt -> (`Step` liftAlt opt) $ case optFind arg opt of
    Nothing -> Nothing
    Just r -> Just (Day r (pure id))

hoistMaybe :: Alternative f => Maybe a -> f a
hoistMaybe = maybe empty pure

class OptParser f where
  runParser :: f a -> ArgParser a

instance OptParser ArgParser where
  runParser = id

class HasUsage f where
  usage :: f a -> Usage Doc

instance Opt f => OptParser (Alt f) where
  runParser p0 = do
    marg <- tryNextArg
    case marg of
      Nothing -> case evalParser p0 of
        Nothing -> errMsg "missing options"
        Just val -> pure val
      Just arg -> go arg p0
    where
      go arg p = do
        case stepParser arg p of
          Step Nothing _ -> skipArg arg >> runParser p
          Step (Just (Day m p')) _ -> do
            r <- m
            runParser (fmap ($r) p')

instance (Functor f, Pretty1 f) => HasUsage (Alt f) where
  usage = getBConst . runAlt (BConst . pure . pretty1)

class Nat f g where
  nat :: f x -> g x

newtype ApSum f g x = ApSum
  { getApSum :: Either (f x) (g x) }

instance (Functor f, Functor g) => Functor (ApSum f g) where
  fmap f = ApSum . bimap (fmap f) (fmap f) . getApSum

instance (Nat g f, Applicative f, Applicative g) => Applicative (ApSum f g) where
  pure = ApSum . Right . pure
  af <*> ax = ApSum $ go (getApSum af) (getApSum ax)
    where
      go (Left f) (Left x) = Left (f <*> x)
      go (Left f) (Right x) = Left (f <*> nat x)
      go (Right f) (Left x) = Left (nat f <*> x)
      go (Right f) (Right x) = Right (f <*> x)

instance (Functor f, Applicative g) => Nat f (Compose f g) where
  nat = Compose . fmap pure

--- Subparsers

newtype WithSub p f a = WithSub
  { unWithSub :: f (Either (p a) a) }

liftSub :: Functor f => f a -> WithSub p f a
liftSub = WithSub . fmap Right

wrapSub :: Functor f => f (p a) -> WithSub p f a
wrapSub = WithSub . fmap Left

instance (Functor f, Functor p) => Functor (WithSub p f) where
  fmap f (WithSub x) = WithSub (fmap (bimap (fmap f) f) x)

instance (Opt f, Functor p, OptParser p) => Opt (WithSub p f) where
  optFind arg (WithSub opt) = (<$> optFind arg opt) $ \m ->
    m >>= \x -> case x of
      Left p -> runParser p
      Right r -> pure r

instance Pretty1 f => Pretty1 (WithSub p f) where
  pretty1 = pretty1 . unWithSub

---

-- | Add a description to a functor.
data WithInfo i f a = WithInfo
  { bundledInfo :: i
  , unWithInfo :: f a }
  deriving (Eq, Ord, Read, Show)

instance Functor f => Functor (WithInfo i f) where
  fmap f (WithInfo i x) = WithInfo i (fmap f x)

instance Opt f => Opt (WithInfo i f) where
  optFind arg = optFind arg . unWithInfo

instance Pretty1 f => Pretty1 (WithInfo i f) where
  pretty1 = pretty1 . unWithInfo

instance OptParser f => OptParser (WithInfo i f) where
  runParser = runParser . unWithInfo

instance HasUsage f => HasUsage (WithInfo i f) where
  usage = usage . unWithInfo

-- | Compose parsers.
instance (OptParser f, OptParser g) => OptParser (Compose f g) where
  runParser p = do
    p' <- runParser (getCompose p)
    resetArgs
    runParser p'

instance HasUsage f => HasUsage (Compose f g) where
  usage = usage . getCompose

overInfo :: (i -> i) -> WithInfo i f a -> WithInfo i f a
overInfo f i = i { bundledInfo = f (bundledInfo i) }

--

data Metadata = Metadata
  { mdFullDesc :: Bool      -- ^ whether the help text should contain full
                            -- documentation
  , mdProgDesc :: Chunk Doc -- ^ brief parser description
  , mdHeader :: Chunk Doc   -- ^ header of the full parser description
  , mdFooter :: Chunk Doc   -- ^ footer of the full parser description
  , mdFailureCode :: Int    -- ^ exit code for a parser failure
  , mdIntersperse :: Bool   -- ^ allow regular options and flags to occur
                              -- after arguments (default: True)
  }
