
module Graphics.NanoVG.Blendish.Shorthand where

import NanoVG (Color)
import qualified NanoVG

black, white, gray :: Color
black = NanoVG.rgba 0 0 0 255
white = NanoVG.rgba 255 255 255 255
gray  = NanoVG.rgba 25 25 25 255


red, green, blue :: Color
red   = NanoVG.rgbf 1 0 0
green = NanoVG.rgbf 0 1 0
blue  = NanoVG.rgbf 0 0 1


