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
  <$> strOption (
        long "name" >>> short 'n' >>>
        help "Specify a username")
  <*> option (
        long "id" >>> short 'i' >>>
        value 0 >>>
        help "Specify the user id")
