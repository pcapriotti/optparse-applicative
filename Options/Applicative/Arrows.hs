module Options.Applicative.Arrows (
  A(..),
  asA,
  runA,
  ParserA,
  module Control.Arrow
  ) where

import Control.Arrow
import Control.Category
import Options.Applicative

import Prelude hiding ((.), id)

newtype A f a b = A
  { unA :: f (a -> b) }

asA :: Applicative f => f a -> A f () a
asA x = A $ const <$> x

runA :: Applicative f => A f () a -> f a
runA a = unA a <*> pure ()

instance Applicative f => Category (A f) where
  id = A $ pure id
  -- use reverse composition, because we want effects to run from
  -- top to bottom in the arrow syntax
  (A f) . (A g) = A $ flip (.) <$> g <*> f

instance Applicative f => Arrow (A f) where
  arr = A . pure
  first (A f) = A $ first <$> f

type ParserA = A Parser
