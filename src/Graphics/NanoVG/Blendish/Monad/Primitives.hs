{-# LANGUAGE RecordWildCards #-}

-- | Blendish primitives

module Graphics.NanoVG.Blendish.Monad.Primitives where

import Data.Text (Text)
import NanoVG (Color(..), Image)

import qualified NanoVG
import qualified Data.Set
import qualified Data.Text

import Graphics.NanoVG.Blendish.Context
import Graphics.NanoVG.Blendish.Icon
import Graphics.NanoVG.Blendish.Theme
import Graphics.NanoVG.Blendish.Types
import Graphics.NanoVG.Blendish.Monad.Wrappers
import Graphics.NanoVG.Blendish.Monad.Combinators
import Graphics.NanoVG.Blendish.Utils
import Graphics.NanoVG.Blendish.Shorthand

import Linear (V2(V2), V4(V4))

data Align = ALeft | ARight | ACenter
  deriving (Eq, Show, Ord)

roundBox
  :: V2 Float
  -> V2 Float
  -> Corners Float
  -> Draw ()
roundBox (V2 x y) (V2 w h) (Corners cr0 cr1 cr2 cr3) = do
    beginPath
    moveTo (V2 x (y + h*0.5))
    arcTo (V2 x       y)       (V2 (x + w) y)       (min cr0 (d / 2))
    arcTo (V2 (x + w) y)       (V2 (x + w) (y + h)) (min cr1 (d / 2))
    arcTo (V2 (x + w) (y + h)) (V2 x       (y + h)) (min cr2 (d / 2))
    arcTo (V2 x       (y + h)) (V2 x       y)       (min cr3 (d / 2))
    closePath
  where
    d = min w h

background
 :: V2 Float
 -> V2 Float
 -> Color
 -> Draw ()
background (V2 x y) (V2 w h) bgColor = withFillColor bgColor $ rect (V2 x y) (V2 w h)

bevel
  :: V2 Float
  -> V2 Float
  -> Color
  -> Draw ()
bevel (V2 x' y') (V2 w' h') bgColor = do
  let x = x' + 0.5
      y = y' + 0.5
      w = w' - 1
      h = h' - 1

      c1 = trans $ offsetColor bgColor bndBevelShade
      c2 = trans $ offsetColor bgColor (-bndBevelShade)

  strokeWidth 1
  withStrokeColor c1 $ do
    moveTo (V2  x      (y + h))
    lineTo (V2 (x + w) (y + h))
    lineTo (V2 (x + w)  y     )

  withStrokeColor c2 $ do
    moveTo (V2  x      (y + h))
    lineTo (V2  x       y     )
    lineTo (V2 (x + w)  y     )

bevelInset
 :: V2 Float
 -> V2 Float
 -> Corners Float
 -> Color
 -> Draw ()
bevelInset (V2 x y') (V2 w h) corners bgColor = do
  let (cr2', cr3') = downCorners corners
      y = y' - 0.5
      d = min w h
      cr2 = min cr2' (d / 2)
      cr3 = min cr3' (d / 2)
      bevelColor = offsetColor bgColor bndInsetBevelShade

  withStroke
    (do
      strokeWidth 1
      pain <- linearGradient
          (V2 x (y + h - (max cr2 cr3) - 1))
          (V2 x (y + h - 1))
          (opaq bevelColor)
          bevelColor

      strokePaint pain
    )
    (do
      moveTo (V2 (x + w) (y + h - cr2))
      arcTo  (V2 (x + w) (y + h)) (V2 x (y + h)) cr2
      arcTo  (V2  x      (y + h)) (V2 x  y)      cr3
    )

dropShadow
 :: V2 Float
 -> V2 Float
 -> Float
 -> Float
 -> Int
 -> Draw ()
dropShadow (V2 x y') (V2 w h') r feather alpha = do
  let y = y' + feather
      h = h' - feather

  withFill (do
    pain <- boxGradient
      (V2 (x - feather * 0.5) (y - feather * 0.5))
      (V2 (w + feather) (h + feather))
      (r + feather * 0.5)
      feather
      (rgba 0 0 0 (alpha ^ (2 :: Int)))
      (rgba 0 0 0 0)

    fillPaint pain
    )
    (do
      moveTo (V2 (x - feather)     (y - feather))
      lineTo (V2  x                (y - feather))
      lineTo (V2  x                (y + h - feather))

      arcTo  (V2  x                (y + h)) (V2 (x + r) (y + h)) r
      arcTo  (V2 (x + h)           (y + h)) (V2 (x + w) (y + h - r)) r

      lineTo (V2 (x + w)           (y - feather))
      lineTo (V2 (x + w + feather) (y - feather))
      lineTo (V2 (x + w + feather) (y + h + feather))
      lineTo (V2 (x - feather)     (y + h + feather))

      closePath
    )

innerBox
  :: V2 Float
  -> V2 Float
  -> Corners Float
  -> Color
  -> Color
  -> Draw ()
innerBox (V2 x y) (V2 w h) corners shadeTop shadeDown = do
  withFill (do
    pain <- linearGradient
      (V2 x y)
      (V2 (if h - 2 > w then x + w else x)
          (if h - 2 > w then y else y + h))
      shadeTop
      shadeDown

    fillPaint pain
    ) $
    roundBox
      (V2 (x + 1) (y + 1))
      (V2 (w - 2) (h - 3))
      (fmap (\cr -> max 0 (cr - 1)) corners)

outlineBox
  :: V2 Float
  -> V2 Float
  -> Corners Float
  -> Color
  -> Draw ()
outlineBox (V2 x y) (V2 w h) corners color = do
  withStrokeColor color
    $ roundBox
      (V2 (x + 0.5) (y + 0.5))
      (V2 (w - 1) (h - 2))
      corners

icon :: NanoVG.Image -> Float -> Float -> Icon -> Draw ()
icon icons x y ico = do
  let
    (ix, iy) = iconXY ico
    u = fromIntegral (bndIconSheetXOff + ix * bndIconSheetGrid)
    v = fromIntegral (bndIconSheetYOff + iy * bndIconSheetGrid)
    res = bndIconSheetRes
  withFill (do
      pat <- imagePattern
        (V2 (x - u) (y - v))
        (V2 (fromIntegral bndIconSheetWidth) (fromIntegral bndIconSheetHeight))
        0 -- rot?
        icons
        1 -- ??
      fillPaint pat
    ) (rect (V2 x y) (pure (fromIntegral res)))

iconLabelValue
  :: Image
  -> V2 Float
  -> V2 Float
  -> Maybe Icon
  -> Color
  -> Align
  -> Text
  -> Int
  -> Maybe Text
  -> Maybe Text
  -> Draw ()
iconLabelValue icons (V2 x y) (V2 w h) mIconId color align font fontSize' mLabel mValue = do
  case mIconId of
    Nothing -> return ()
    Just iconId -> icon icons (x + 4) (y + 2) iconId

  let pLeft = bndPadLeft + maybe 0 (pure bndIconSheetRes) mIconId
      label = maybe mempty id mLabel

  fontFace font
  fontSize $ fromIntegral fontSize'
  withFillColor color $ return ()

  case mValue of
    Nothing -> do
      textAlign (Data.Set.fromList [if align == ALeft then NanoVG.AlignLeft else NanoVG.AlignCenter, NanoVG.AlignBaseline])
      textBox
        (V2 (x + fromIntegral pLeft) (y + h - bndTextPadDown))
        (w - fromIntegral bndPadRight - fromIntegral pLeft)
        label

    Just value -> do
      let sep = Data.Text.pack bndLabelSep

      labWidth <- textWidth label
      sepWidth <- textWidth sep
      valWidth <- textWidth value

      textAlign (Data.Set.fromList [NanoVG.AlignLeft, NanoVG.AlignBaseline])
      xoff <- case align of
        ACenter -> do
          let width = labWidth + sepWidth + valWidth
          return $
              x
            + fromIntegral pLeft
            + (((w - (fromIntegral $ bndPadRight - pLeft)) - width) * 0.5)
        _ -> return $ x + fromIntegral pLeft

      let yoff = y + h - bndTextPadDown
          fudge = 1.5 -- XXX: "fix" kerning of 'val : 1'

      text (V2 xoff yoff) label
      text (V2 (xoff + labWidth - fudge) yoff) sep
      text (V2 (xoff + labWidth + sepWidth) yoff) value

      -- debug value separator boxes
      {--
      withFillColor black $ do
        rect (V2 xoff yoff) (V2 labWidth 10)

      withFillColor white $ do
        rect (V2 (xoff + labWidth) yoff) (V2 sepWidth 10)

      withFillColor (trans red) $ do
        rect (V2 (xoff + labWidth + sepWidth) yoff) (V2 valWidth (-10))
      --}

nodeIconLabel
  :: Image
  -> V2 Float
  -> V2 Float
  -> Maybe Icon
  -> Color
  -> Color
  -> Align
  -> Text
  -> Int
  -> Text
  -> Draw ()
nodeIconLabel icons (V2 x y) (V2 w h) mIconId color shadowColor align font fontSize' label = do
  case mIconId of
    Nothing -> return ()
    Just iconId -> icon icons (x + 4) (y + 2) iconId

  let pLeft = bndPadLeft + maybe 0 (pure bndIconSheetRes) mIconId

  fontFace font
  fontSize $ fromIntegral fontSize'
  withFillColor shadowColor $ return ()
  fontBlur bndShadowFeather
  textAlign (Data.Set.fromList [if align == ALeft then NanoVG.AlignLeft else NanoVG.AlignCenter, NanoVG.AlignBaseline])
  textBox
        (V2 (1 + x + fromIntegral pLeft) (3 + y + h - bndTextPadDown))
        (w - fromIntegral bndPadRight - fromIntegral pLeft)
        label

  fontBlur 0
  withFillColor color $ return ()
  textBox
    (V2 (x + fromIntegral pLeft) (y + h - bndTextPadDown))
    (w - fromIntegral bndPadRight - fromIntegral pLeft)
    label


labelWidth
  :: Maybe a
  -> Text
  -> p
  -> Draw Integer
labelWidth mIconId label _font = do
  let w = bndPadLeft + bndPadRight + (maybe 0 (pure bndIconSheetRes) mIconId)

  Theme{..} <- theme

  -- when nonempty label
  fontFace tFont
  fontSize $ fromIntegral tFontSize
  tw <- textWidth label
  return $ w + round tw

textWidth :: Text -> Draw Float
textWidth label = do
  (V4 b0 _b1 b2 _b3) <- textBounds 1 1 label
  return (b2 - b0)

labelHeight
  :: Maybe Integer
  -> Text
  -> Integer
  -> Draw Float
labelHeight mIconId label width' = do
  let h = bndWidgetHeight
      width = width' - (bndTextRadius * 2) - (maybe bndIconSheetRes id mIconId)

  Theme{..} <- theme

  -- when nonempty label
  fontFace tFont
  fontSize $ fromIntegral tFontSize

  (V4 _b0 b1 _b2 b3) <- textBoxBounds 1 1 (fromIntegral width) label
  let bh = (b3 - b1) + bndTextPadDown
  return $ if bh > h then bh else h

-- XXX
iconLabelCarret :: Draw ()
iconLabelCarret = undefined

-- | Check mark
check :: V2 Float -> Color -> Draw ()
check (V2 x y) color = withStroke (do
  strokeWidth 2
  strokeColor color
  lineCap NanoVG.Butt
  lineJoin NanoVG.Miter
  )
  (do
    moveTo (V2 (x + 4)  (y + 5))
    lineTo (V2 (x + 7)  (y + 8))
    lineTo (V2 (x + 14) (y + 1))
  )

arrow :: V2 Float -> Float -> Color -> Draw ()
arrow (V2 x y) s color = withFillColor color $ do
  moveTo (V2 x y)
  lineTo (V2 (x - s) (y + s))
  lineTo (V2 (x - s) (y - s))
  closePath

upDownArrow :: V2 Float -> Float -> Color -> Draw ()
upDownArrow (V2 x y) s color = withFillColor color $ do
  moveTo (V2 x (y - 1))
  lineTo (V2 (x + 0.5 * w) (y - s - 1))
  lineTo (V2 (x + w) (y - 1))
  closePath
  moveTo (V2 x (y + 1))
  lineTo (V2 (x + 0.5 * w) (y + s + 1))
  lineTo (V2 (x + w) (y + 1))
  closePath
  where w = s * 1.1


