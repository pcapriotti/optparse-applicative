{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances #-}
module Options.Applicative.Classes where

import Control.Alternative.FreeStar
import Control.Applicative

import Options.Applicative.Basic

class HasOption f g where
  liftOption :: f a -> g a

class HasOption f g => HasSubOption p f g where
  liftSubOption :: f (p a) -> g a

class HasInfo i p where
  getInfo :: p a -> i

instance HasOption f g => HasOption f (Alt g) where
  liftOption = liftAlt . liftOption

instance HasSubOption p f g => HasSubOption p f (Alt g) where
  liftSubOption = liftAlt . liftSubOption

instance (Functor g, HasOption f g) => HasOption f (WithSub p g) where
  liftOption = liftSub . liftOption

instance (Functor g, HasOption f g) => HasSubOption p f (WithSub p g) where
  liftSubOption  = wrapSub . liftOption

instance HasOption f g => HasOption f (OptSum g h) where
  liftOption = OptLeft . liftOption

instance HasSubOption p f g => HasSubOption p f (OptSum g h) where
  liftSubOption = OptLeft . liftSubOption

instance HasOption f (OptSum g f) where
  liftOption = OptRight

instance (HasOption f g, Functor g, Applicative h) => HasOption f (ComposeP g h) where
  liftOption = ComposeP . fmap pure . liftOption

instance (HasSubOption p f g, Functor g, Applicative h) => HasSubOption p f (ComposeP g h) where
  liftSubOption = ComposeP . fmap pure . liftSubOption

instance HasOption f g => HasOption (WithInfo i f) (WithInfo i g) where
  liftOption (WithInfo i opt) = WithInfo i (liftOption opt)

instance HasSubOption p f g => HasSubOption p (WithInfo i f) (WithInfo i g) where
  liftSubOption (WithInfo i opt) = WithInfo i (liftSubOption opt)

instance HasOption BaseOption BaseOption where
  liftOption = id

instance HasInfo i (WithInfo i f) where
  getInfo = bundledInfo
