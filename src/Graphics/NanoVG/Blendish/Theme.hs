{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-record-wildcards #-}

module Graphics.NanoVG.Blendish.Theme where

import Data.Text (Text)
import NanoVG (Color, Image)
import Graphics.NanoVG.Blendish.Types (WidgetFocus(..))
import Graphics.NanoVG.Blendish.Shorthand
import Graphics.NanoVG.Blendish.Utils
import Linear

import qualified NanoVG

data WidgetTheme = WidgetTheme {
    wtOutline       :: Color -- ^ Widget outline color
  , wtItem          :: Color
  , wtInner         :: Color
  , wtInnerSelected :: Color
  , wtText          :: Color -- ^ Text color
  , wtTextSelected  :: Color -- ^ Color of selected text
  , wtShadeTop      :: Float -- ^ Delta modifier for upper part of gradient (-100 to 100)
  , wtShadeDown     :: Float -- ^ Delta modifier for lower part of gradient (-100 to 100)
  }

def :: WidgetTheme
def = WidgetTheme
  (NanoVG.rgbf 0.098 0.098 0.098)
  (NanoVG.rgbf 0.098 0.098 0.098)
  (NanoVG.rgbf 0.6   0.6   0.6  )
  (NanoVG.rgbf 0.392 0.392 0.392)
  bndColorText
  bndColorTextSelected
  0 0

bndColorText :: Color
bndColorText =  black

bndColorTextSelected :: Color
bndColorTextSelected = white

shadeTopDown
  :: Float
  -> Float
  -> WidgetTheme
  -> WidgetTheme
shadeTopDown top down x = x { wtShadeTop = top, wtShadeDown = down }

fifteenShades :: WidgetTheme -> WidgetTheme
fifteenShades = shadeTopDown 15 (-15)

offsetColorTopDown :: Color -> WidgetTheme -> (Color, Color)
offsetColorTopDown c WidgetTheme{..} =
  (offsetColor c wtShadeTop, offsetColor c wtShadeDown)

shapeColors
  :: Color
  -> Color
  -> Color
  -> Color
  -> WidgetTheme
  -> WidgetTheme
shapeColors a b c d x = x
  { wtOutline = a
  , wtItem = b
  , wtInner = c
  , wtInnerSelected = d
  }

textColors
  :: Color
  -> Color
  -> WidgetTheme
  -> WidgetTheme
textColors a b x = x
  { wtText = a
  , wtTextSelected = b
  }

toolTheme :: WidgetTheme
toolTheme =  fifteenShades def

radioTheme :: WidgetTheme
radioTheme =
    fifteenShades
  $ shapeColors
      black white
      (rgbf 0.275 0.275 0.275)
      (rgbf 0.337 0.502 0.761)
  $ textColors
      bndColorTextSelected
      bndColorText
      def

choiceTheme :: WidgetTheme
choiceTheme = textColors
  bndColorTextSelected
  (rgbf 0.8   0.8   0.8  )
  optionTheme

textFieldTheme :: WidgetTheme
textFieldTheme = shadeTopDown 0 25 $ shapeColors
  (rgbf 0.098 0.098 0.098)
  (rgbf 0.353 0.353 0.353)
  (rgbf 0.6   0.6   0.6  )
  (rgbf 0.6   0.6   0.6  )
  def

optionTheme :: WidgetTheme
optionTheme = fifteenShades $ shapeColors
  black white
  (rgbf 0.275 0.275 0.275)
  (rgbf 0.275 0.275 0.275)
  def

numberFieldTheme :: WidgetTheme
numberFieldTheme = shadeTopDown (-20) 0 $ shapeColors
  (rgbf 0.098 0.098 0.098)
  (rgbf 0.353 0.353 0.353)
  (rgbf 0.706 0.706 0.706)
  (rgbf 0.6   0.6   0.6  )
  def

sliderTheme :: WidgetTheme
sliderTheme = numberFieldTheme { wtItem = rgbf 0.502 0.502 0.502 }

scrollBarTheme :: WidgetTheme
scrollBarTheme = shadeTopDown 5 (-5) $ shapeColors
  (rgbf 0.196 0.196 0.196)
  (rgbf 0.502 0.502 0.502)
  (rgbaf 0.314 0.314 0.314 0.706)
  (rgbaf 0.392 0.392 0.392 0.706)
  def

toolTipTheme :: WidgetTheme
toolTipTheme = shapeColors
  black
  (rgbf 0.392 0.392 0.392)
  (rgbaf 0.098 0.098 0.098 0.902)
  (rgbaf 0.176 0.176 0.176 0.902)
  $ textColors
  (rgbf1 0.627)
  bndColorTextSelected
  def

menuTheme :: WidgetTheme
menuTheme = shapeColors
  black
  (rgbf1 0.392)
  (rgbaf1 0.098 0.902)
  (rgbaf1 0.176 0.902)
  $ textColors
  (rgbf1 0.627)
  bndColorTextSelected
  def

menuItemTheme :: WidgetTheme
menuItemTheme = shadeTopDown 38 0 $ shapeColors
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

defaultNodeTheme :: NodeTheme
defaultNodeTheme = NodeTheme
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

defTheme :: Image -> Theme
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
  menuTheme
  menuItemTheme
  defaultNodeTheme
  iconz
  "sans"
  13

innerColors :: WidgetFocus -> WidgetTheme -> Bool -> (Color, Color)
innerColors NoFocus wt@WidgetTheme{..} _ = offsetColorTopDown wtInner wt
innerColors HasFocus wt@WidgetTheme{..} _ = offsetColorTopDown (offsetColor wtInner bndHoverShade) wt
innerColors ActiveFocus wt@WidgetTheme{..} flipped =
  case flipped of
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

clamp :: Ord a => a -> a -> a -> a
clamp x low _    | x < low = low
clamp x _   high | x > high = high
clamp x _   _    | otherwise = x

scrollHandleRect
  :: V2 Float
  -> V2 Float
  -> Float -- ^ offset
  -> Float -- ^ size
  -> V4 Float
scrollHandleRect (V2 x y) (V2 w h) off' size' =
  let off = clamp off' 0 1
      size = clamp size' 0 1
  in if h > w
    then
      let hs = max (size * h) (w + 1)
      in (V4 x (y + (h - hs) * off) w hs)
    else
      let ws = max (size * w) (h - 1)
      in (V4 (x + (w - ws) * off) y ws h)


-- default text size
--bndLabelFontSize = 15

-- default text padding in inner box
bndPad :: Integer
bndPad = 8

bndPadLeft :: Integer
bndPadLeft = bndPad

bndPadRight :: Integer
bndPadRight = bndPad

bndLabelSep :: String
bndLabelSep = ": "

bndTransparentAlpha :: Float
bndTransparentAlpha = 0.643

bndBevelShade :: Float
bndBevelShade = 30

bndInsetBevelShade :: Float
bndInsetBevelShade = 30

bndHoverShade :: Float
bndHoverShade = 15

bndSplitterShade :: Float
bndSplitterShade = 100

bndIconSheetWidth :: Integer
bndIconSheetWidth = 602

bndIconSheetHeight :: Integer
bndIconSheetHeight = 640

bndIconSheetGrid :: Integer
bndIconSheetGrid = 21

bndIconSheetXOff :: Integer
bndIconSheetXOff = 5

bndIconSheetYOff :: Integer
bndIconSheetYOff = 10

bndIconSheetRes :: Integer
bndIconSheetRes = 16

bndToolRadius :: Float
bndToolRadius = 4

bndOptionRadius :: Float
bndOptionRadius = 4

bndOptionWidth :: Float
bndOptionWidth = 14

bndOptionHeight :: Float
bndOptionHeight = 15

bndTextRadius :: Integer
bndTextRadius = 4

bndNumberRadius :: Float
bndNumberRadius = 10

bndShadowFeather :: Float
bndShadowFeather = 12

bndShadowAlpha :: Float
bndShadowAlpha = 0.5


bndScrollBarRadius :: Float
bndScrollBarRadius = 7

bndScrollBarActiveShade :: Float
bndScrollBarActiveShade = 16


bndMaxGlyphs :: Integer
bndMaxGlyphs = 1024

bndMaxRows :: Integer
bndMaxRows = 32

bndTextPadDown :: Float
bndTextPadDown = 6

bndNumberArrowSize :: Float
bndNumberArrowSize = 4

bndNodeWireOutlineWidth :: Float
bndNodeWireOutlineWidth = 4

bndNodeWireWidth :: Float
bndNodeWireWidth = 2

bndNodeRadius :: Float
bndNodeRadius = 8

bndNodeTitleFeather :: Float
bndNodeTitleFeather = 1

bndNodeArrowSize :: Float
bndNodeArrowSize = 9

-- lala
bndWidgetHeight :: Float
bndWidgetHeight = 21

bndToolWidth :: Float
bndToolWidth = 20


bndScrollbarWidth :: Float
bndScrollbarWidth = 13

bndScrollbarHeight :: Float
bndScrollbarHeight = 14


bndVspacing :: Float
bndVspacing = 1

bndVspacingGroup :: Float
bndVspacingGroup = 8

bndHspacing :: Float
bndHspacing = 8
