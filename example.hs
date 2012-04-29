import Options.Applicative
import Options.Applicative.Builder
import Control.Applicative

data User = User
  { userName :: String
  , userId :: Integer
  } deriving Show

example :: Parser User
example = User
  <$> strOption
      ( long "name"
      . short 'n'
      . metavar "NAME"
      . help "Specify a username" )
  <*> option
      ( long "id"
      . short 'i'
      . metavar "ID"
      . value 0
      . help "Specify the user id" )
