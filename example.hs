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
  <$> strOption "name" (   help "Specify a username"
                       >>> alias (short 'n'))
  <*> option "id" (   value 0
                  >>> help "Specify the user id"
                  >>> alias (short 'i'))
