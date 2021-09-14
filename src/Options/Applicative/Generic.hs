{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Options.Applicative.Generic
    ( simpleOptions
    , simpleCommands
    ) where

import GHC.Generics
import GHC.TypeLits ( KnownSymbol, symbolVal, TypeError, ErrorMessage(..) )
import Data.Proxy
import Data.Kind (Type)
import Data.Typeable

import Options.Applicative.Types
import Options.Applicative.Builder
import Data.Char (toUpper)


simpleOptions :: (Generic a, GSimpleOptions (Rep a)) => Parser a
simpleOptions = to <$> gOptionsParserSimple


simpleCommands :: (Generic a, GSimpleCommands (Rep a)) => Parser a
simpleCommands = to <$> toParser (gCommandFields Proxy Proxy)



class GSimpleOptions a where
    gOptionsParserSimple :: Parser (a x)

type SimpleOptionsMalformed = 'Text "simpleOptions expects exactly one, nonempty constructor on the data type."

instance TypeError SimpleOptionsMalformed => GSimpleOptions (a :+: b) where
    gOptionsParserSimple = undefined
instance TypeError SimpleOptionsMalformed => GSimpleOptions V1 where
    gOptionsParserSimple = undefined
instance TypeError SimpleOptionsMalformed => GSimpleOptions U1 where
    gOptionsParserSimple = undefined

-- Datatype name isn't used
instance GSimpleOptions a => GSimpleOptions (D1 m a) where
    gOptionsParserSimple = M1 <$> gOptionsParserSimple

-- Constructor name isn't used in simple case
instance GSimpleOptions a => GSimpleOptions (C1 m a) where
    gOptionsParserSimple = M1 <$> gOptionsParserSimple

instance (KnownSymbol n, GIsOption (ToOptUniverse a) a) => GSimpleOptions (S1 ('MetaSel ('Just n) u ss ds) (K1 t a)) where
    gOptionsParserSimple = M1 . K1 <$> gOption (Proxy :: Proxy (ToOptUniverse a)) n
      where
        n = symbolVal (Proxy :: Proxy n)
instance GIsArg (ToArgUniverse a) a => GSimpleOptions (S1 ('MetaSel 'Nothing u ss ds) (K1 t a)) where
    gOptionsParserSimple = M1 . K1 <$> gArg (Proxy :: Proxy (ToArgUniverse a))

instance (GSimpleOptions a, GSimpleOptions b) => GSimpleOptions (a :*: b) where
    gOptionsParserSimple = (:*:) <$> gOptionsParserSimple <*> gOptionsParserSimple


newtype Commands a = Commands
    { getCommands :: [(String, Parser a)]
    }
  deriving (Functor, Semigroup)

toParser :: Commands a -> Parser a
toParser = subparser . foldMap (\(n, p) -> command n (info p mempty)) . getCommands

class GSimpleCommands a where
    gCommandFields :: proxy0 x -> proxy1 a -> Commands (a x)

-- Datatype name isn't used
instance GSimpleCommands a => GSimpleCommands (D1 m a) where
    gCommandFields px _ = M1 <$> gCommandFields px (Proxy :: Proxy a)

-- From constructor we delegate to GSimpleOptions
instance (KnownSymbol n, GSimpleOptions a) => GSimpleCommands (C1 ('MetaCons n f r) a) where
    gCommandFields _ _ = Commands [(n, gOptionsParserSimple)]
      where
        n = symbolVal (Proxy :: Proxy n)

instance (GSimpleCommands a, GSimpleCommands b) => GSimpleCommands (a :+: b) where
    gCommandFields px _ = (L1 <$> gCommandFields px (Proxy :: Proxy a)) <> (R1 <$> gCommandFields px (Proxy :: Proxy b))


typeName :: forall a f. (HasMetavar f, Typeable a) => Mod f a
typeName = metavar (map toUpper . show . typeRep $ (Proxy :: Proxy a))

named :: HasName f => String -> Mod f a
named n = long n <> short (head n)

-- get around overlapping instances
data OptUniverse = OptBool | OptString | OptOther
type family ToOptUniverse (a :: Type) :: OptUniverse where
    ToOptUniverse Bool   = 'OptBool
    ToOptUniverse String = 'OptString
    ToOptUniverse a      = 'OptOther

class GIsOption (b :: OptUniverse) a where
    gOption :: proxy0 b -> String -> Parser a

instance GIsOption 'OptBool Bool where
    gOption _ n = switch (named n)
instance GIsOption 'OptString String where
    gOption _ n = strOption (named n <> metavar "STRING")
instance (Read a, Typeable a) => GIsOption 'OptOther a where
    gOption _ n = option auto (named n <> typeName)

-- get around overlapping instances
data ArgUniverse = ArgString | ArgOther
type family ToArgUniverse (a :: Type) :: ArgUniverse where
    ToArgUniverse String = 'ArgString
    ToArgUniverse a      = 'ArgOther

class GIsArg (b :: ArgUniverse) a where
    gArg :: proxy0 b -> Parser a

instance GIsArg 'ArgString String where
    gArg _ = argument str (metavar "STRING")
instance (Read a, Typeable a) => GIsArg 'ArgOther a where
    gArg _ = argument auto typeName
