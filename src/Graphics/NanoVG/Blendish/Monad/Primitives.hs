{-# LANGUAGE RecordWildCards #-}

-- | Blendish primitives

module Graphics.NanoVG.Blendish.Monad.Primitives where

import Control.Monad (when)
import Data.Text (Text)
import NanoVG (Color(..), Image)
import Foreign.C.Types (CFloat(CFloat))

import qualified NanoVG
import qualified NanoVG.Internal.Text
import qualified Data.IORef
import qualified Data.Maybe
import qualified Data.Set
import qualified Data.Text
import qualified Data.Vector

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
          (V2 x (y + h - max cr2 cr3 - 1))
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
 -> Float
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
      (rgbaf 0 0 0 (alpha ** 2))
      (rgba 0 0 0 0)

    fillPaint pain
    )
    (do
      moveTo (V2 (x - feather)     (y - feather))
      lineTo (V2  x                (y - feather))
      lineTo (V2  x                (y + h - feather))

      arcTo  (V2  x                (y + h)) (V2 (x + r) (y + h)) r
      arcTo  (V2 (x + w)           (y + h)) (V2 (x + w) (y + h - r)) r

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
      label = Data.Maybe.fromMaybe mempty mLabel

  fontFace font
  fontSize $ fromIntegral fontSize'
  fillColor color

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
            + (((w - fromIntegral (bndPadRight - pLeft)) - width) * 0.5)
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
    Just iconId -> icon icons (x + w - fromIntegral bndIconSheetRes) (y + 3) iconId

  fontFace font
  fontSize $ fromIntegral fontSize'
  fillColor shadowColor

  fontBlur bndNodeTitleFeather
  textAlign (Data.Set.fromList [if align == ALeft then NanoVG.AlignLeft else NanoVG.AlignCenter, NanoVG.AlignBaseline])
  textBox
        (V2 (1 + x) (3 + y + h - bndTextPadDown))
        (w - fromIntegral bndPadRight)
        label

  fontBlur 0
  fillColor color
  textBox
    (V2 x (2 + y + h - bndTextPadDown))
    (w - fromIntegral bndPadRight)
    label


labelWidth
  :: Maybe a
  -> Text
  -> p
  -> Draw Integer
labelWidth mIconId label _font = do
  let w =
        bndPadLeft
        + bndPadRight
        + maybe 0 (pure bndIconSheetRes) mIconId

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
      width =
        width'
        - (round bndTextRadius * 2)
        - Data.Maybe.fromMaybe bndIconSheetRes mIconId

  Theme{..} <- theme

  -- when nonempty label
  fontFace tFont
  fontSize $ fromIntegral tFontSize

  (V4 _b0 b1 _b2 b3) <- textBoxBounds 1 1 (fromIntegral width) label
  let bh = (b3 - b1) + bndTextPadDown
  return $ if bh > h then bh else h

labelCarret
  :: V2 Float
  -> V2 Float
  -> Color -- ^ Text color
  -> Int   -- ^ Font size
  -> Text
  -> Int   -- ^ Caret start
  -> Int   -- ^ Caret end
  -> Draw ()
labelCarret (V2 x y) (V2 w _h) textColor' fontSize' txt caretStart caretEnd = do

  fontSize $ fromIntegral fontSize'
  textAlign $ Data.Set.fromList [NanoVG.AlignLeft, NanoVG.AlignBaseline]
  fillColor textColor'

  Theme{..} <- theme
  let caretColor = wtItem tTextField

  withCtx $ \c -> do
    counter <- Data.IORef.newIORef 0

    NanoVG.save c
    (_, CFloat desc, CFloat lineh) <- NanoVG.textMetrics c

    -- TODO what if textBreakLines function was a fold?
    NanoVG.textBreakLines c txt (cvt w) 3 $ \row i -> do
      let y' = y + fromIntegral i * lineh
          textY = y' + lineh

      -- text bounding boxes
      when False $ do
        NanoVG.beginPath c
        NanoVG.strokeColor c red
        NanoVG.rect c (cvt $ x - 1) (cvt $ y' - desc) (NanoVG.Internal.Text.width row) (cvt lineh)
        NanoVG.stroke c

      glyphs' <- NanoVG.textGlyphPositions c (cvt x) (cvt textY)
        (NanoVG.Internal.Text.start row) (NanoVG.Internal.Text.end row)
        100

      cpos <- Data.IORef.readIORef counter
      --print (cpos, length glyphs, (caretStart, caretEnd), (cpos >= caretStart, cpos <= caretEnd))
      let
        epos = cpos + length glyphs'
        inCaretStart = caretStart >= cpos && caretStart <= epos
        inCaretEnd = caretEnd >= cpos && caretEnd <= epos
        glyphs = Data.Vector.cons (cvt x) (Data.Vector.map NanoVG.Internal.Text.glyphPosMaxX glyphs')

      Data.IORef.writeIORef counter epos
      --print (cpos, length glyphs, (inCaretStart, inCaretEnd))
      --
      let coloredRect color x' y'' w' h' = do
            NanoVG.fillColor c color
            NanoVG.beginPath c
            NanoVG.rect c x' y'' w' h'
            NanoVG.fill c

      case (inCaretStart, inCaretEnd) of
        -- single line
        (True, True) -> case caretStart == caretEnd of
          -- no selection
          True -> do
            let g  = glyphs Data.Vector.! (caretStart - cpos)
            coloredRect (rgbaf 0.137 0.302 0.561 0.666) (g - 1.5) (cvt $ y' - desc) 2 (cvt $ lineh + 1)

          -- single line selection
          False -> do
            let gl = glyphs Data.Vector.! (caretStart - cpos)
                gr = glyphs Data.Vector.! (caretEnd - cpos)

            coloredRect caretColor gl (cvt $ y' - desc) (gr - gl) (cvt $ lineh + 1)

        -- selection starts on this line
        (True, False) -> do
          let gl = glyphs Data.Vector.! (caretStart - cpos)

          coloredRect caretColor gl (cvt $ y' - desc) (NanoVG.Internal.Text.width row - (gl - cvt x)) (cvt $ lineh + 1)

        -- selection ends on this line
        (False, True) -> do
          let gr = glyphs Data.Vector.! (caretEnd - cpos)

          coloredRect caretColor (cvt x) (cvt $ y' - desc) (gr - cvt x) (cvt $ lineh + 1)

        -- row in-between
        (False, False) -> do
          -- possibly selected
          when (caretStart < cpos && caretEnd > epos) $ do
            coloredRect caretColor (cvt $ x - 1) (cvt $ y' - desc) (NanoVG.Internal.Text.width row) (cvt lineh)

      -- draw text itself
      NanoVG.fillColor c textColor'
      NanoVG.Internal.Text.text c (cvt x) (cvt textY)
        (NanoVG.Internal.Text.start row) (NanoVG.Internal.Text.end row)

-- | Draw carret position based on cursor position
--
-- Algorithm from https://github.com/cocreature/nanovg-hs/blob/master/example/Example.hs#L247
caretP
  :: V2 CFloat
  -> V2 CFloat
  -> Text
  -> Draw ()
caretP (V2 x y) (V2 w _h) txt = do
  (V2 mx' my') <- mouse
  -- TODO weird
  let mx :: CFloat
      mx = (fromIntegral :: Integer -> CFloat) $ round mx'
      my :: CFloat
      my = (fromIntegral :: Integer -> CFloat) $ round my'
  withCtx $ \c -> do
    (_, desc, lineh) <- NanoVG.textMetrics c

    -- what if textBreakLines function was a fold?
    NanoVG.textBreakLines c txt w 3 $ \row i -> do
       let y' = y + fromIntegral i * lineh
           hit = mx > x && mx < (x+w) && my >= y' && my < (y' + lineh)

       when hit $ do
         let caretxInit = if mx < x+ NanoVG.Internal.Text.width row / 2 then x else x + NanoVG.Internal.Text.width row

         glyphs <- NanoVG.textGlyphPositions c x y (NanoVG.Internal.Text.start row) (NanoVG.Internal.Text.end row) 100

         let leftBorders = Data.Vector.map NanoVG.Internal.Text.glyphX glyphs
             rightBorders = Data.Vector.snoc (Data.Vector.drop 1 leftBorders) (x + NanoVG.Internal.Text.width row)
             rightPoints = Data.Vector.zipWith (\xa ya -> 0.3*xa+0.7*ya) leftBorders rightBorders
             leftPoints = Data.Vector.cons x (Data.Vector.take (Data.Vector.length glyphs - 1) rightPoints)
             caretx = maybe caretxInit (NanoVG.Internal.Text.glyphX . (glyphs Data.Vector.!))
               $ Data.Vector.findIndex (\(px,gx) -> mx >= px && mx < gx) $ Data.Vector.zip leftPoints rightPoints
         NanoVG.beginPath c
         NanoVG.fillColor c (rgba 255 192 0 255)
         NanoVG.rect c caretx (y' - desc) 1 lineh
         NanoVG.fill c

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
upDownArrow (V2 x y) s color = do
  withFillColor color $ do
    moveTo (V2 x (y - 1))
    lineTo (V2 (x + 0.5 * w) (y - s - 1))
    lineTo (V2 (x + w) (y - 1))
  withFillColor color $ do
    moveTo (V2 x (y + 1))
    lineTo (V2 (x + 0.5 * w) (y + s + 1))
    lineTo (V2 (x + w) (y + 1))
  where w = s * 1.1

nodeArrowDown
  :: V2 Float
  -> Float
  -> Color
  -> Draw ()
nodeArrowDown pos@(V2 x y) size color = withFillColor color $ do
  moveTo pos
  lineTo (V2 (x + 0.5 * size) (y - size))
  lineTo (V2 (x - 0.5 * size) (y - size))

nodePort
  :: V2 Float
  -> WidgetFocus
  -> Color
  -> Draw ()
nodePort pos focus color = do
  Theme{..} <- theme
  withStrokeColor (ntWires tNode) $ do
    withFillColor (if focus == NoFocus then color else offsetColor color bndHoverShade) $ do
      strokeWidth 1
      circle pos bndNodePortRadius
