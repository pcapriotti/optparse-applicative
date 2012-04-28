import Options.Applicative
import Options.Applicative.Builder
import Control.Applicative
import Control.Arrow

data User = User
  { userName :: String
  , userId :: Integer
  } deriving Show

example :: Parser User
example = User
  <$> option "name" (value "unnamed" >>>
                     this (reader str) >>>
                     alias (short 'n'))
  <*> option "id" (value 0 >>> alias (short 'i'))
