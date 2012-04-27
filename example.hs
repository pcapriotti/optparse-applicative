import Options.Applicative
import Control.Applicative

data User = User
  { userName :: String
  , userId :: Integer
  } deriving Show

parser :: Parser User
parser = User
  <$> option "name" 'n' Nothing Just
  <*> optionR "id" 'i' (Just 0)
