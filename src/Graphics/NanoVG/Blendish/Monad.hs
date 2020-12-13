{-# LANGUAGE RecordWildCards #-}

module Graphics.NanoVG.Blendish.Monad where

import Data.Text (Text)
import Data.Set (Set)
import NanoVG (Color(..), Paint(..), Image, LineCap, Bounds)

import qualified NanoVG
import qualified Data.Set
import           Foreign.C.Types (CFloat(CFloat))
import qualified Data.Text

import Graphics.NanoVG.Blendish.Context
import Graphics.NanoVG.Blendish.Icon
import Graphics.NanoVG.Blendish.Types
import Graphics.NanoVG.Blendish.Theme
import Graphics.NanoVG.Blendish.Utils

import Linear (V2(V2), V4(V4))

withFill
  :: UI ()
  -> UI ()
  -> UI ()
withFill paintAct act = do
  beginPath
  act
  paintAct
  fill

withFillColor
  :: Color
  -> UI ()
  -> UI ()
withFillColor bgColor = withFill (fillColor bgColor)

withStroke
  :: UI ()
  -> UI ()
  -> UI ()
withStroke paintAct act = do
  beginPath
  act
  paintAct
  stroke

withStrokeColor
  :: Color
  -> UI ()
  -> UI ()
withStrokeColor bgColor = withStroke (strokeColor bgColor)

boxGradient
  :: V2 Float -- ^ Box origin
  -> V2 Float -- ^ Box size
  -> Float -- ^ Radius
  -> Float -- ^ Feather
  -> Color
  -> Color
  -> UI Paint
boxGradient (V2 x y) (V2 w h) radius feather color1 color2 = withCtx $ \c ->
  NanoVG.boxGradient c (CFloat x) (CFloat y) (CFloat w) (CFloat h) (CFloat radius) (CFloat feather) color1 color2

data Align = ALeft | ARight | ACenter
  deriving (Eq, Show, Ord)

-- blendish primitives
--
roundBox
  :: V2 Float
  -> V2 Float
  -> Float
  -> Float
  -> Float
  -> Float
  -> UI ()
roundBox (V2 x y) (V2 w h) cr0 cr1 cr2 cr3 = do
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
 -> UI ()
background (V2 x y) (V2 w h) bgColor = withFillColor bgColor $ rect (V2 x y) (V2 w h)

bevel
  :: V2 Float
  -> V2 Float
  -> Color
  -> UI ()
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
 -> Float
 -> Float
 -> Color
 -> UI ()
bevelInset (V2 x y') (V2 w h) cr2' cr3' bgColor = do
  let y = y' - 0.5
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
 -> UI ()
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
  -> Float
  -> Float
  -> Float
  -> Float
  -> Color
  -> Color
  -> UI ()
innerBox (V2 x y) (V2 w h) cr0 cr1 cr2 cr3 shadeTop shadeDown = do
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
      (max 0 (cr0 - 1))
      (max 0 (cr1 - 1))
      (max 0 (cr2 - 1))
      (max 0 (cr3 - 1))

outlineBox
  :: V2 Float
  -> V2 Float
  -> Float
  -> Float
  -> Float
  -> Float
  -> Color
  -> UI ()
outlineBox (V2 x y) (V2 w h) cr0 cr1 cr2 cr3 color = do
  withStrokeColor color
    $ roundBox
      (V2 (x + 0.5) (y + 0.5))
      (V2 (w - 1) (h - 2))
      cr0 cr1 cr2 cr3

icon :: NanoVG.Image -> Float -> Float -> Icon -> UI ()
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
  :: Integral a
  => Image
  -> V2 Float
  -> V2 Float
  -> Maybe Icon
  -> Color
  -> Align
  -> Data.Text.Text
  -> a
  -> Maybe String
  -> Maybe Text
  -> UI ()
iconLabelValue icons (V2 x y) (V2 w h) mIconId color align font fontSize' mLabel mValue = do
  case mIconId of
    Nothing -> return ()
    Just iconId -> icon icons (x + 4) (y + 2) iconId

  let pLeft = bndPadLeft + maybe 0 (pure bndIconSheetRes) mIconId
      label = Data.Text.pack $ maybe mempty id mLabel

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

      text (V2 xoff yoff) label
      text (V2 (xoff + labWidth) yoff) sep
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

labelWidth
  :: Maybe a
  -> Text
  -> p
  -> UI Integer
labelWidth mIconId label _font = do
  let w = bndPadLeft + bndPadRight + (maybe 0 (pure bndIconSheetRes) mIconId)

  Theme{..} <- theme

  -- when nonempty label
  fontFace tFont
  fontSize $ fromIntegral tFontSize
  tw <- textWidth label
  return $ w + round tw

textWidth :: Text -> UI Float
textWidth label = do
  (V4 b0 _b1 b2 _b3) <- textBounds 1 1 label
  return (b2 - b0)

labelHeight
  :: Maybe Integer
  -> Text
  -> Integer
  -> UI Float
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
iconLabelCarret :: UI ()
iconLabelCarret = undefined

-- | Check mark
check :: V2 Float -> Color -> UI ()
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

arrow :: V2 Float -> Float -> Color -> UI ()
arrow (V2 x y) s color = withFillColor color $ do
  moveTo (V2 x y)
  lineTo (V2 (x - s) (y + s))
  lineTo (V2 (x - s) (y - s))
  closePath

upDownArrow :: V2 Float -> Float -> Color -> UI ()
upDownArrow (V2 x y) s color = withFillColor color $ do
  moveTo (V2 x (y - 1))
  lineTo (V2 (x + 0.5 * w) (y - s - 1))
  lineTo (V2 (x + w) (y - 1))
  closePath
  moveTo (V2 x (y - 1))
  lineTo (V2 (x + 0.5 * w) (y + s + 1))
  lineTo (V2 (x + w) (y - 1))
  closePath
  where w = s * 1.1

-- Themed

toolButton
  :: V2 Float
  -> V2 Float
  -> [SharpCorner]
  -> WidgetFocus
  -> Maybe Icon
  -> Maybe String
  -> UI ()
toolButton pos sz flags state mIcon mLabel = do
  let [cr0, cr1, cr2, cr3] = selectCorners bndToolRadius flags

  Theme{..} <- theme

  bevelInset pos sz (cr2) (cr3) tBg
  let (i1, i2) = innerColors state tTool True

  innerBox   pos sz cr0 cr1 cr2 cr3 i1 i2
  outlineBox pos sz cr0 cr1 cr2 cr3 (trans (wtOutline tTool))

  let tc = textColor state tTool
  iconLabelValue tIcons pos sz mIcon tc ACenter tFont tFontSize mLabel Nothing

radioButton
  :: V2 Float
  -> V2 Float
  -> [SharpCorner]
  -> WidgetFocus
  -> Maybe Icon
  -> Maybe String
  -> UI ()
radioButton pos sz flags state mIcon mLabel = do
  let [cr0, cr1, cr2, cr3] = selectCorners bndToolRadius flags

  Theme{..} <- theme

  bevelInset pos sz cr2 cr3 tBg
  let (i1, i2) = innerColors state tRadio True

  innerBox   pos sz cr0 cr1 cr2 cr3 i1 i2
  outlineBox pos sz cr0 cr1 cr2 cr3 (trans (wtOutline tRadio))

  let tc = textColor state tRadio
  iconLabelValue tIcons pos sz mIcon tc ACenter tFont tFontSize mLabel Nothing

choiceButton
  :: V2 Float
  -> V2 Float
  -> [SharpCorner]
  -> WidgetFocus
  -> Maybe Icon
  -> Maybe String
  -> UI ()
choiceButton pos@(V2 x y) sz@(V2 w _h) flags state mIcon mLabel = do
  let [cr0, cr1, cr2, cr3] = selectCorners bndToolRadius flags

  Theme{..} <- theme

  bevelInset pos sz cr2 cr3 tBg
  let (i1, i2) = innerColors state tChoice True

  innerBox   pos sz cr0 cr1 cr2 cr3 i1 i2
  outlineBox pos sz cr0 cr1 cr2 cr3 (trans (wtOutline tChoice))

  let tc = textColor state tRadio
  iconLabelValue tIcons pos sz mIcon tc ACenter tFont tFontSize mLabel Nothing
  upDownArrow (V2 (x + w - 10) (y + 10)) 5 (trans (wtItem tChoice))

-- Boring wrappers

beginPath :: UI ()
beginPath = withCtx NanoVG.beginPath

closePath :: UI ()
closePath = withCtx NanoVG.closePath

moveTo :: V2 Float -> UI ()
moveTo (V2 x y) = withCtx $ \c -> NanoVG.moveTo c (CFloat x) (CFloat y)

lineTo :: V2 Float -> UI ()
lineTo (V2 x y) = withCtx $ \c -> NanoVG.lineTo c (CFloat x) (CFloat y)

arcTo :: V2 Float -> V2 Float -> Float -> UI ()
arcTo (V2 x y) (V2 j k) r = withCtx $ \c ->
  NanoVG.arcTo c (CFloat x) (CFloat y) (CFloat j) (CFloat k) (CFloat r)

rect :: V2 Float -> V2 Float -> UI ()
rect (V2 x y) (V2 w h) = withCtx $ \c ->
  NanoVG.rect c (CFloat x) (CFloat y) (CFloat w) (CFloat h)

fillColor :: Color -> UI ()
fillColor x = withCtx $ \c -> NanoVG.fillColor c x

fillPaint :: Paint -> UI ()
fillPaint = withCtx . flip NanoVG.fillPaint

fill :: UI ()
fill = withCtx NanoVG.fill

strokeWidth :: Float -> UI ()
strokeWidth w = withCtx $ \c -> NanoVG.strokeWidth c (CFloat w)

strokeColor :: Color -> UI ()
strokeColor x = withCtx $ \c -> NanoVG.strokeColor c x

strokePaint :: Paint -> UI ()
strokePaint = withCtx . flip NanoVG.strokePaint

stroke :: UI ()
stroke = withCtx NanoVG.stroke

cvt :: Float -> CFloat
cvt = CFloat

linearGradient 
  :: V2 Float
  -> V2 Float
  -> Color
  -> Color
  -> UI Paint
linearGradient (V2 x y) (V2 a b) color1 color2 = withCtx $ \c ->
  NanoVG.linearGradient c (cvt x) (cvt y) (cvt a) (cvt b) color1 color2

imagePattern
  :: V2 Float
  -> V2 Float
  -> Float
  -> Image
  -> Float
  -> UI Paint
imagePattern (V2 x y) (V2 w h) r img a = withCtx $ \c ->
  NanoVG.imagePattern c (cvt x) (cvt y) (cvt w) (cvt h) (cvt r) img (cvt a)

lineCap :: LineCap -> UI ()
lineCap = withCtx . flip NanoVG.lineCap

lineJoin :: LineCap -> UI ()
lineJoin = withCtx . flip NanoVG.lineJoin

fontFace :: Text -> UI ()
fontFace = withCtx . flip NanoVG.fontFace

fontSize :: Float -> UI ()
fontSize = withCtx . flip NanoVG.fontSize . cvt

text :: V2 Float -> Text -> UI ()
text (V2 x y) txt = withCtx $ \c -> NanoVG.text c (cvt x) (cvt y) txt

textBox :: V2 Float -> Float -> Text -> UI ()
textBox (V2 x y) w txt = withCtx $ \c -> NanoVG.textBox c (cvt x) (cvt y) (cvt w) txt

textAlign :: Set NanoVG.Align -> UI ()
textAlign = withCtx . flip NanoVG.textAlign

cvtBounds :: Bounds -> V4 Float
cvtBounds (NanoVG.Bounds (NanoVG.V4 (CFloat x0) (CFloat y0) (CFloat x1) (CFloat y1))) 
  = V4 x0 y0 x1 y1

textBounds :: Float -> Float -> Text -> UI (V4 Float)
textBounds a b t = withCtx $ \c ->
  NanoVG.textBounds c (CFloat a) (CFloat b) t >>= return . cvtBounds

textBoxBounds :: Float -> Float -> Float -> Text -> UI (V4 Float)
textBoxBounds a b w t = withCtx $ \c ->
  NanoVG.textBoxBounds c (CFloat a) (CFloat b) (CFloat w) t >>= return . cvtBounds
