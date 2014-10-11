{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances #-}
module Options.Applicative.Classes where

import Control.Alternative.FreeStar
import Control.Applicative
import Data.Functor.Compose

import Options.Applicative.Basic

class HasOption f g where
  liftOption :: f a -> g a

class HasMetadata p where
  getMetadata :: p a -> Metadata

instance HasOption f g => HasOption f (Alt g) where
  liftOption = liftAlt . liftOption

instance (Functor g, HasOption f g) => HasOption f (WithSub p g) where
  liftOption = liftSub . liftOption

instance HasOption f g => HasOption f (OptSum g h) where
  liftOption = OptLeft . liftOption

instance HasOption f (OptSum g f) where
  liftOption = OptRight

instance (HasOption f g, Functor g, Applicative h) => HasOption f (Compose g h) where
  liftOption = Compose . fmap pure . liftOption

instance HasOption f g => HasOption (WithInfo i f) (WithInfo i g) where
  liftOption (WithInfo i opt) = WithInfo i (liftOption opt)

instance HasOption BaseOption BaseOption where
  liftOption = id
