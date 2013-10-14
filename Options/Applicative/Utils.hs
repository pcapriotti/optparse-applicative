module Options.Applicative.Utils
  ( mappendWith
  , Chunk(..)
  , chunked
  , listToChunk
  , (<<+>>)
  , vcatChunks
  , isEmpty
  , stringChunk
  , extract
  , duplicate
  , tabulate
  ) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid hiding ((<>))
import Text.PrettyPrint.ANSI.Leijen

import qualified Text.PrettyPrint.ANSI.Leijen as PP

mappendWith :: Monoid a => a -> a -> a -> a
mappendWith s x y = mconcat [x, s, y]

-- the free monoid on a semigroup a
newtype Chunk a = Chunk
  { unChunk :: Maybe a }

instance Functor Chunk where
  fmap f = Chunk . fmap f . unChunk

instance Applicative Chunk where
  pure = Chunk . pure
  Chunk f <*> Chunk x = Chunk (f <*> x)

instance Monad Chunk where
  return = pure
  m >>= f = Chunk $ unChunk m >>= unChunk . f

instance MonadPlus Chunk where
  mzero = Chunk mzero
  mplus m1 m2 = Chunk $ mplus (unChunk m1) (unChunk m2)

-- Given a semigroup structure on a, return a monoid structure on Chunk.  Note
-- that this is *not* the same as liftA2
chunked :: (a -> a -> a)
        -> Chunk a -> Chunk a -> Chunk a
chunked _ (Chunk Nothing) y = y
chunked _ x (Chunk Nothing) = x
chunked f (Chunk (Just x)) (Chunk (Just y)) = Chunk (Just (f x y))

-- | Concatenate a list into a Chunk.  'listToChunk' satisfies:
--
-- listToChunk = mconcat . fmap pure
listToChunk :: Monoid a => [a] -> Chunk a
listToChunk [] = mempty
listToChunk xs = pure (mconcat xs)

instance Monoid a => Monoid (Chunk a) where
  mempty = Chunk Nothing
  mappend = chunked mappend

-- a constrained comonad instance
extract :: Monoid a => Chunk a -> a
extract = fromMaybe mempty . unChunk
duplicate :: Monoid a => Chunk a -> Chunk (Chunk a)
duplicate = fmap pure

-- | Concatenates two 'Chunk's with a space in between.  If one is empty, this
-- just returns the other one.
--
-- Unlike '(<+>)' for 'Doc', this operation has a unit element, namely the empty
-- 'Chunk.
(<<+>>) :: Chunk Doc -> Chunk Doc -> Chunk Doc
(<<+>>) = chunked (<+>)

vcatChunks :: [Chunk Doc] -> Chunk Doc
vcatChunks = foldr (chunked (PP.<$>)) mempty

isEmpty :: Chunk a -> Bool
isEmpty = isJust . unChunk

stringChunk :: String -> Chunk Doc
stringChunk "" = mempty
stringChunk s = pure (string s)

tabulate' :: Int -> [(Doc, Doc)] -> Chunk Doc
tabulate' _ [] = mempty
tabulate' size table = pure $ vcat
  [ nest 2 (fillBreak size key <+> value)
  | (key, value) <- table ]

-- | Display pairs of strings in a table.
tabulate :: [(Doc, Doc)] -> Chunk Doc
tabulate = tabulate' 24
