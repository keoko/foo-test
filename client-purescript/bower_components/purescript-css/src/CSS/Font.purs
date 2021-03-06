module CSS.Font where

import Prelude

import Data.Generic (class Generic)
import Data.NonEmpty (NonEmpty, oneOf)

import CSS.Color (Color)
import CSS.Property (class Val, Value, value, quote)
import CSS.Size (Size)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, key)

color :: Color -> CSS
color = key $ fromString "color"

newtype GenericFontFamily = GenericFontFamily Value

derive instance eqGenericFontFamily :: Eq GenericFontFamily
derive instance ordGenericFontFamily :: Ord GenericFontFamily
derive instance genericGenericFontFamily :: Generic GenericFontFamily

instance valGenericFontFamily :: Val GenericFontFamily where
  value (GenericFontFamily v) = v

sansSerif :: GenericFontFamily
sansSerif = GenericFontFamily $ fromString "sans-serif"

fontFamily :: Array String -> NonEmpty Array GenericFontFamily -> CSS
fontFamily a b = key (fromString "font-family") <<< value $ (value <<< quote <$> a) <> oneOf (value <$> b)

fontSize :: forall a. Size a -> CSS
fontSize = key $ fromString "font-size"

newtype FontWeight = FontWeight Value

derive instance eqFontWeight :: Eq FontWeight
derive instance ordFontWeight :: Ord FontWeight
derive instance genericFontWeight :: Generic FontWeight

instance valFontWeight :: Val FontWeight where
  value (FontWeight v) = v

bold :: FontWeight
bold = FontWeight $ fromString "bold"

bolder :: FontWeight
bolder = FontWeight $ fromString "bolder"

lighter :: FontWeight
lighter = FontWeight $ fromString "lighter"

weight :: Number -> FontWeight
weight i = FontWeight $ value i

fontWeight :: FontWeight -> CSS
fontWeight = key $ fromString "font-weight"
