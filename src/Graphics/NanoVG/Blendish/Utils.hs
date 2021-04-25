
module Graphics.NanoVG.Blendish.Utils where

import NanoVG (Color(..))
import Foreign.C.Types (CFloat(..))
import qualified NanoVG
import qualified Data.Bits

offsetColor :: Color -> Float -> Color
offsetColor (Color (CFloat r) (CFloat g) (CFloat b) a) delta = Color (CFloat $ r + by) (CFloat $ g + by) (CFloat $ b + by) a
  where
    by = delta / 255

talpha :: Float
talpha = 0.666

trans :: Color -> Color
trans = flip trans' talpha

opaq :: Color -> Color
opaq = flip setAlpha 0

trans' :: Color -> Float -> Color
trans' (Color r g b a) factor = Color r g b (a * CFloat factor)

getAlpha :: Color -> Float
getAlpha (Color _r _g _b (CFloat a)) = a

setAlpha :: Color -> Float -> Color
setAlpha (Color r g b _a) a = Color r g b (CFloat a)

rgbf :: Float -> Float -> Float -> Color
rgbf r g b = NanoVG.rgbf (CFloat r) (CFloat g) (CFloat b)

rgba :: Int -> Int -> Int -> Int -> Color
rgba r g b a = NanoVG.rgba (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)

rgbaf :: Float -> Float -> Float -> Float -> Color
rgbaf r g b a = NanoVG.rgbaf (CFloat r) (CFloat g) (CFloat b) (CFloat a)

rgbf1 :: Float -> Color
rgbf1 x = rgbf x x x

rgbaf1 :: Float -> Float -> Color
rgbaf1 x = rgbaf x x x

-- 0 0 -> A1 (topleft)
-- 1 0 -> A2 (topleft -> this)
iconIdFromXY :: Integer -> Integer -> Integer
iconIdFromXY x y = x + y `Data.Bits.shiftL` 8
