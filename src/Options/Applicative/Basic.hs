{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances #-}
module Options.Applicative.Basic where

import Control.Applicative
import Control.Alternative.FreeStar
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Bimonoid
import Data.Functor.Identity
import Data.Semigroup hiding (Option, Product)
import qualified Data.Semigroup as S
import Data.Traversable (sequenceA)

import Options.Applicative.Help.Pretty
import Options.Applicative.Usage

class Pretty1 f where
  pretty1 :: f a -> Doc

data Name
  = LongName String
  | ShortName Char
  deriving (Eq, Read, Show)

instance Pretty Name where
  pretty (LongName n) = string $ '-' : '-' : n
  pretty (ShortName c) = string $ '-' : [c]

newtype ParseError = ParseError
  { unParseError :: S.Option (Last ParseError1) }
  deriving (Eq, Ord, Read, Show, Monoid)

data ParseError1 = ErrMsg String
  deriving (Eq, Ord, Read, Show)

type Err = Except ParseError

errMsg :: MonadError ParseError m => String -> m a
errMsg = throwError . ParseError . S.Option . Just . Last . ErrMsg

newtype Validate a = Validate
  { runValidate :: ReaderT String Err a }
  deriving ( Functor, Applicative, Monad, Alternative, MonadPlus
           , MonadReader String, MonadError ParseError )

data ParserState a = ParserState
  { pendingArgs :: [a]
  , skippedArgs :: [a] }
  deriving (Eq, Ord, Read, Show)

newtype ArgParser a = ArgParser
  { runArgParser :: StateT (ParserState String) Err a }
  deriving ( Functor, Applicative, Monad, Alternative, MonadPlus
           , MonadState (ParserState String), MonadError ParseError )

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

data Option a
  = Option Name (Validate a)
  | Flag Name a
  | Command String a

instance Functor Option where
  fmap f (Option n v) = Option n (fmap f v)
  fmap f (Flag n x) = Flag n (f x)
  fmap f (Command n x) = Command n (f x)

class Functor f => Opt f where
  optFind :: String -> f a -> Maybe (ArgParser a)

instance Pretty1 Option where
  pretty1 (Option n _) = pretty n </> string "ARG"
  pretty1 (Flag n _) = pretty n
  pretty1 (Command arg _) = string arg

instance Opt Option where
  optFind arg (Option n v)
    | matchName n arg = Just (argParser1 v)
  optFind arg (Flag n x)
    | matchName n arg = Just (pure x)
  optFind arg (Command cmd x)
    | arg == cmd = Just (pure x)
  optFind _ _ = empty

data WithDesc i f a = WithDesc
  { desc :: i
  , unWithDesc :: f a }
  deriving (Eq, Ord, Read, Show)

instance Functor f => Functor (WithDesc i f) where
  fmap f (WithDesc d x) = WithDesc d (fmap f x)

instance Pretty i => Pretty1 (WithDesc i f) where
  pretty1 (WithDesc d _) = pretty d

type Parser = Alt Option

evalParser :: Functor f => Alt f a -> Maybe a
evalParser = runAlt $ const Nothing

-- | Convert a 'Validate' value into an argument parser that consumes exactly
-- one argument.
argParser1 :: Validate a -> ArgParser a
argParser1 v = do
  x <- nextArg
  ArgParser . lift $ runReaderT (runValidate v) x

matchName :: Name -> String -> Bool
matchName (ShortName n) ('-':[c]) = n == c
matchName (LongName n) ('-':'-':arg) = n == arg
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
  usage :: f a -> Usage Doc

instance (Pretty1 f, Opt f) => OptParser (Alt f) where
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

instance Applicative f => Nat Identity f where
  nat = pure . runIdentity

-- | Add subparser support to a parser.
newtype WithSub f a = WithSub
  { unWithSub :: f (ApSum (WithSub f) Identity a) }

instance Functor f => Functor (WithSub f) where
  fmap f = WithSub . fmap (fmap f) . unWithSub

instance Applicative f => Applicative (WithSub f) where
  pure = WithSub . pure . pure
  f <*> x = WithSub $ (<*>) <$> unWithSub f <*> unWithSub x

instance Alternative f => Alternative (WithSub f) where
  empty = WithSub empty
  x <|> y = WithSub $ unWithSub x <|> unWithSub y

  some = WithSub . fmap sequenceA . some . unWithSub
  many p = some p <|> pure []

instance OptParser f => OptParser (WithSub f) where
  runParser p = do
    x <- getApSum <$> runParser (unWithSub p)
    case x of
      Left p' -> runParser p'
      Right (Identity r) -> pure r
  usage = usage . unWithSub

-- | Add a description to a parser.
data WithInfo i f a = WithInfo
  { info :: i
  , unWithInfo :: f a }
  deriving (Eq, Ord, Read, Show)

instance Functor f => Functor (WithInfo i f) where
  fmap f (WithInfo i x) = WithInfo i (fmap f x)

instance OptParser f => OptParser (WithInfo i f) where
  runParser = runParser . unWithInfo
  usage = usage . unWithInfo
