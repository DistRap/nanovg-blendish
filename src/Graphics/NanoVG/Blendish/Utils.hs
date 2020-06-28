
module Graphics.NanoVG.Blendish.Utils where

import NanoVG (Color(..))
import Foreign.C.Types (CFloat(..))
import qualified NanoVG

offsetColor :: Color -> Integer -> Color
offsetColor (Color r g b a) delta = Color (r + by) (g + by) (b + by) a
  where
    by = (fromIntegral delta) / 255

talpha :: Float
talpha = 0.666

trans = flip trans' talpha
opaq = flip setAlpha 0

trans' :: Color -> Float -> Color
trans' (Color r g b a) factor = Color r g b (a * (CFloat factor))

setAlpha :: Color -> Float -> Color
setAlpha (Color r g b _a) a = Color r g b (CFloat a)


