import Options.Applicative
import Control.Applicative

data User = User
  { userName :: String
  , userId :: Integer
  } deriving Show

l :: String -> OptName
l = OptLong

parser :: Parser User
parser = User
  <$> option (l "name") Nothing Just
  <*> optionR (l "id") (Just 0)
