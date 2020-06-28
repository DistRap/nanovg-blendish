{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphics.NanoVG.Blendish.Theme where

import Data.Text (Text)
import NanoVG (Color, Image, rgba, rgbf, rgbaf)
import Graphics.NanoVG.Blendish.Types (WidgetFocus(..))
import Graphics.NanoVG.Blendish.Shorthand
import Graphics.NanoVG.Blendish.Utils

import qualified NanoVG

data WidgetTheme = WidgetTheme {
    wtOutline       :: Color -- ^ Widget outline color
  , wtItem          :: Color
  , wtInner         :: Color
  , wtInnerSelected :: Color
  , wtText          :: Color -- ^ Text color
  , wtTextSelected  :: Color -- ^ Color of selected text
  , wtShadeTop      :: Integer -- ^ Delta modifier for upper part of gradient (-100 to 100)
  , wtShadeDown     :: Integer -- ^ Delta modifier for lower part of gradient (-100 to 100)

  }

def = WidgetTheme
  (NanoVG.rgbf 0.098 0.098 0.098)
  (NanoVG.rgbf 0.098 0.098 0.098)
  (NanoVG.rgbf 0.6   0.6   0.6  )
  (NanoVG.rgbf 0.392 0.392 0.392)
  bndColorText
  bndColorTextSelected
  0 0

-- theme
bndColorText =  black
bndColorTextSelected = white

shadeTopDown top down x = x { wtShadeTop = top, wtShadeDown = down }

fifteenShades = shadeTopDown 15 (-15)

offsetColorTopDown c WidgetTheme{..} =
  (offsetColor c wtShadeTop, offsetColor c wtShadeDown)

shapeColors a b c d x = x {
    wtOutline = a
  , wtItem = b
  , wtInner = c
  , wtInnerSelected = d }

textColors a b x = x {
    wtText = a
  , wtTextSelected = b
  }

toolTheme =  fifteenShades def

radioTheme = fifteenShades $ shapeColors
  black white
  (rgbf 0.275 0.275 0.275)
  (rgbf 0.337 0.502 0.761)
  def

textFieldTheme = shadeTopDown 0 25 $ shapeColors
  (rgbf 0.098 0.098 0.098)
  (rgbf 0.353 0.353 0.353)
  (rgbf 0.6   0.6   0.6  )
  (rgbf 0.6   0.6   0.6  )
  def

optionTheme = fifteenShades $ shapeColors
  black white
  (rgbf 0.275 0.275 0.275)
  (rgbf 0.275 0.275 0.275)
  def

choiceTheme = textColors
  bndColorTextSelected
  (rgbf 0.8   0.8   0.8  )
  optionTheme

numberFieldTheme = shadeTopDown (-20) 0 $ shapeColors
  (rgbf 0.098 0.098 0.098)
  (rgbf 0.353 0.353 0.353)
  (rgbf 0.706 0.706 0.706)
  (rgbf 0.6   0.6   0.6  )
  def

sliderTheme = numberFieldTheme { wtItem = rgbf 0.502 0.502 0.502 }

scrollBarTheme = shadeTopDown 5 (-5) $ shapeColors
  (rgbf 0.196 0.196 0.196)
  (rgbf 0.502 0.502 0.502)
  (rgbaf 0.314 0.314 0.314 0.706)
  (rgbaf 0.392 0.392 0.392 0.706)
  def

toolTipTheme = shapeColors
  black
  (rgbf 0.392 0.392 0.392)
  (rgbaf 0.098 0.098 0.098 0.902)
  (rgbaf 0.176 0.176 0.176 0.902)
  $ textColors
  (rgbf1 0.627)
  bndColorTextSelected
  def

-- lolo
rgbf1 x = rgbf x x x
rgbaf1 x a = rgbaf x x x a

menuTheme' = shapeColors
  black
  (rgbf1 0.392)
  (rgbaf1 0.098 0.902)
  (rgbaf1 0.176 0.902)
  $ textColors
  (rgbf1 0.627)
  bndColorTextSelected
  def

menuItemTheme' = shadeTopDown 38 0 $ shapeColors
  black
  (rgbaf1 0.675 0.502)
  (rgbaf1 0     0    )
  (rgbf   0.337 0.502 0.761)
  $ textColors
  bndColorTextSelected
  bndColorText
  def


data NodeTheme = NodeTheme {
    ntSelected      :: Color
  , ntWires         :: Color
  , ntTextSelected  :: Color
  , ntActiveNode    :: Color
  , ntWireSelect    :: Color
  , ntNodeBackdrop  :: Color
  , ntNoodleCurving :: Int -- how much a noodle curves (0 to 10)
  }

defNT = NodeTheme
  (rgbf 0.945 0.345 0    )
  black
  (rgbf 0.498 0.439 0.439)
  (rgbf 1     0.666 0.251)
  white
  (rgbaf1 0.608 0.627)
  5

data Theme = Theme {
    tBg          :: Color
  , tRegular     :: WidgetTheme
  , tTool        :: WidgetTheme
  , tRadio       :: WidgetTheme
  , tTextField   :: WidgetTheme
  , tOption      :: WidgetTheme
  , tChoice      :: WidgetTheme
  , tNumberField :: WidgetTheme
  , tSlider      :: WidgetTheme
  , tScrollBar   :: WidgetTheme
  , tToolTip     :: WidgetTheme
  , tMenu        :: WidgetTheme
  , tMenuItem    :: WidgetTheme
  , tNode        :: NodeTheme
  , tIcons       :: Image
  , tFont        :: Text
  , tFontSize    :: Int
  }

defTheme iconz = Theme
  (rgbf 0.447 0.447 0.447)
  def
  toolTheme
  radioTheme
  textFieldTheme
  optionTheme
  choiceTheme
  numberFieldTheme
  sliderTheme
  scrollBarTheme
  toolTipTheme
  menuTheme'
  menuItemTheme'
  defNT
  iconz
  "sans"
  13

innerColors :: WidgetFocus -> WidgetTheme -> Bool -> (Color, Color)
innerColors NoFocus wt@WidgetTheme{..} _ = offsetColorTopDown wtInner wt
innerColors HasFocus wt@WidgetTheme{..} _ = offsetColorTopDown (offsetColor wtInner bndHoverShade) wt
innerColors ActiveFocus wt@WidgetTheme{..} flip = 
  case flip of
    False -> offsetColorTopDown wtInnerSelected wt
    True -> offsetColorTopDown wtInnerSelected $ -- flipped top/down
      wt { wtShadeTop = wtShadeDown, wtShadeDown = wtShadeTop }

textColor :: WidgetFocus -> WidgetTheme -> Color
textColor ActiveFocus WidgetTheme{..} = wtTextSelected
textColor _           WidgetTheme{..} = wtText

nodeWireColor :: WidgetFocus -> NodeTheme -> Color
nodeWireColor ActiveFocus NodeTheme{..} = ntActiveNode
nodeWireColor HasFocus    NodeTheme{..} = ntWireSelect
nodeWireColor NoFocus     NodeTheme{..} = rgbf1 0.5

clamp x low high | x < low = low
clamp x low high | x > high = high
clamp x low high | otherwise = x

scrollHandleRect x y w h off' size' =
  let off = clamp off' 0 1
      size = clamp size' 0 1
  in if h > w
    then 
      let hs = max (size * h) (w + 1)
      in (x, y + (h - hs) * off, w, hs)
    else
      let ws = max (size * w) (h - 1)
      in (x + (w - ws) * off, y, ws, h)


-- default text size
--bndLabelFontSize = 15

-- default text padding in inner box
bndPad = 8
bndPadLeft = bndPad
bndPadRight = bndPad

bndLabelSep = ": "

bndTransparentAlpha = 0.643

bndBevelShade = 30
bndInsetBevelShade = 30
bndHoverShade = 15
bndSplitterShade = 100

bndIconSheetWidth = 602
bndIconSheetHeight = 640
bndIconSheetGrid = 21
bndIconSheetXOff = 5
bndIconSheetYOff = 10
bndIconSheetRes = 16

bndToolRadius = 4

bndOptionRadius = 4
bndOptionWidth = 14
bndOptionHeight = 15

bndTextRadius = 4

bndNumberRadius = 10

bndShadowFeather = 12
bndShadowAlpha = 0.5

bndScrollBarRadius = 7
bndScrollBarActiveShade = 16

bndMaxGlyphs = 1024
bndMaxRows = 32

bndTextPadDown = 7

bndNodeWireOutlineWidth = 4
bndNodeWireWidth = 2
bndNodeRadius = 8
bndNodeTitleFeather = 1
bndNodeArrowSize = 9

-- lala
bndWidgetHeight = 21
bndToolWidth = 20

bndScrollbarWidth = 13
bndScrollbarHeight = 14

bndVspacing = 1
bndVspacingGroup = 8
bndHspacing = 8


