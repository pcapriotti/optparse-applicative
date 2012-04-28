module Options.Applicative.Builder where

import Control.Applicative
import Data.Lens.Common
import Options.Applicative

-- lenses --

readerL :: Lens (Option r) (OptReader r)
readerL = lens optReader $ \r opt -> opt { optReader = r }

mainL :: Lens (OptionGroup r a) (Option r)
mainL = lens optMain $ \m opts -> opts { optMain = m }

aliasesL :: Lens (OptionGroup r a) [Option r]
aliasesL = lens optAliases $ \as opts -> opts { optAliases = as }

nameL :: Lens (Option r) OptName
nameL = lens optName $ \n opt -> opt { optName = n }

defaultL :: Lens (OptionGroup r a) (Maybe a)
defaultL = lens optDefault $ \x opts -> opts { optDefault = x }

-- readers --

auto :: Read a => OptReader a
auto = OptReader $ \arg -> case reads arg of
  [(r, "")] -> Just r
  _         -> Nothing

str :: OptReader String
str = OptReader Just

disabled :: OptReader a
disabled = OptReader $ const Nothing

fixed :: a -> OptReader a
fixed = FlagReader

-- combinators --

long :: String -> Option r -> Option r
long lname = nameL ^= OptLong lname

short :: Char -> Option r -> Option r
short sname = nameL ^= OptShort sname

value :: a -> OptionGroup r a -> OptionGroup r a
value r = defaultL ^= Just r

reader :: OptReader r -> Option r -> Option r
reader = setL readerL

this :: (Option r -> Option r)
     -> OptionGroup r a
     -> OptionGroup r a
this = modL mainL

alias :: (Option r -> Option r)
      -> OptionGroup r a
      -> OptionGroup r a
alias f opts = modL aliasesL (opt:) opts
  where opt = f (opts ^. mainL)

option :: Read a => String -> (OptionGroup a a -> OptionGroup a b) -> Parser b
option lname f = liftOpt (f opts)
  where
    opts = OptionGroup
      { optMain = opt
      , optAliases = []
      , optCont = Just . pure
      , optDefault = Nothing }
    opt = Option
      { optName = OptLong lname
      , optReader = auto
      }

multi :: OptionGroup r a -> OptionGroup r [a]
multi opts = mkOptGroup []
  where
    mkOptGroup xs = opts
      { optDefault = Just xs
      , optCont = mkCont xs }
    mkCont xs r = do
      p' <- optCont opts r
      x <- evalParser p'
      return $ liftOpt (mkOptGroup (x:xs))
