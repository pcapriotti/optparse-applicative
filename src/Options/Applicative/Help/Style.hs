{-# LANGUAGE CPP #-}

module Options.Applicative.Help.Style
  ( SetStyle (..)
  , ColorIntensity (..)
  , Layer (..)
  , ConsoleIntensity (..)
  , Underlining (..)
  , Italicized (..)
  , Color (..)
  , color
  , bgColor
  , colorDull
  , bgColorDull
  , bold
  , underlined
  , italicized
  , styleToRawText
  , defaultStyle
  ) where

import Control.Applicative
import Data.Maybe
#if __GLASGOW_HASKELL__ <= 802
import Data.Semigroup hiding (option)
#endif
import System.Console.ANSI (ConsoleIntensity (..), ColorIntensity (..), Underlining (..))

import qualified System.Console.ANSI    as ANSI

data SetStyle = SetStyle
  { ansiReset             :: Bool
  , ansiForeground        :: Maybe (ColorIntensity, Color)  -- ^ Set the foreground color, or keep the old one.
  , ansiBackground        :: Maybe (ColorIntensity, Color)  -- ^ Set the background color, or keep the old one.
  , ansiConsoleIntensity  :: Maybe ConsoleIntensity         -- ^ Adjust boldness
  , ansiItalics           :: Maybe Italicized               -- ^ Adjust italics
  , ansiUnderlining       :: Maybe Underlining              -- ^ Adjust underlining
  } deriving (Eq, Ord, Show)

instance Monoid SetStyle where
    mempty = SetStyle False Nothing Nothing Nothing Nothing Nothing
    mappend = (<>)

defaultStyle :: SetStyle
defaultStyle = SetStyle
  { ansiReset             = True
  , ansiForeground        = Nothing
  , ansiBackground        = Nothing
  , ansiConsoleIntensity  = Just NormalIntensity
  , ansiItalics           = Just NoItalics
  , ansiUnderlining       = Just NoUnderline
  }

isItalicised :: Italicized -> Bool
isItalicised Italicized = True
isItalicised NoItalics = False

styleToRawText :: SetStyle -> String
styleToRawText = ANSI.setSGRCode . stylesToSgrs
  where
    stylesToSgrs :: SetStyle -> [ANSI.SGR]
    stylesToSgrs (SetStyle r fg bg b i u) = catMaybes
        [ if r then Just ANSI.Reset else Nothing
        , fmap (\(intensity, c) -> ANSI.SetColor ANSI.Foreground intensity (convertColor c)) fg
        , fmap (\(intensity, c) -> ANSI.SetColor ANSI.Background intensity (convertColor c)) bg
        , fmap ANSI.SetConsoleIntensity b
        , fmap (ANSI.SetItalicized . isItalicised) i
        , fmap ANSI.SetUnderlining u
        ]

    convertColor :: Color -> ANSI.Color
    convertColor = \c -> case c of
        Black   -> ANSI.Black
        Red     -> ANSI.Red
        Green   -> ANSI.Green
        Yellow  -> ANSI.Yellow
        Blue    -> ANSI.Blue
        Magenta -> ANSI.Magenta
        Cyan    -> ANSI.Cyan
        White   -> ANSI.White

data Layer = Foreground | Background
    deriving (Eq, Ord, Show)

data Italicized = Italicized | NoItalics deriving (Eq, Ord, Show)

instance Semigroup SetStyle where
  cs1 <> cs2 = SetStyle
    { ansiReset             = ansiReset             cs1 &&  ansiReset             cs2
    , ansiForeground        = ansiForeground        cs1 <|> ansiForeground        cs2
    , ansiBackground        = ansiBackground        cs1 <|> ansiBackground        cs2
    , ansiConsoleIntensity  = ansiConsoleIntensity  cs1 <|> ansiConsoleIntensity  cs2
    , ansiItalics           = ansiItalics           cs1 <|> ansiItalics           cs2
    , ansiUnderlining       = ansiUnderlining       cs1 <|> ansiUnderlining       cs2
    }

data Color = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
    deriving (Eq, Ord, Show)

-- | Style the foreground with a vivid color.
color :: Color -> SetStyle
color c = mempty { ansiForeground = Just (Vivid, c) }

-- | Style the background with a vivid color.
bgColor :: Color -> SetStyle
bgColor c =  mempty { ansiBackground = Just (Vivid, c) }

-- | Style the foreground with a dull color.
colorDull :: Color -> SetStyle
colorDull c =  mempty { ansiForeground = Just (Dull, c) }

-- | Style the background with a dull color.
bgColorDull :: Color -> SetStyle
bgColorDull c =  mempty { ansiBackground = Just (Dull, c) }

-- | Render in __bold__.
bold :: SetStyle
bold = mempty { ansiConsoleIntensity = Just BoldIntensity }

-- | Render in /italics/.
italicized :: SetStyle
italicized = mempty { ansiItalics = Just Italicized }

-- | Render underlined.
underlined :: SetStyle
underlined = mempty { ansiUnderlining = Just SingleUnderline }
