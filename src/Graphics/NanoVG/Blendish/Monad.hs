{-# LANGUAGE RecordWildCards #-}

-- | Themed widgets

module Graphics.NanoVG.Blendish.Monad where

import Control.Monad (when, forM_)
import Data.Text (Text)

import Graphics.NanoVG.Blendish.Context
import Graphics.NanoVG.Blendish.Icon
import Graphics.NanoVG.Blendish.Shorthand
import Graphics.NanoVG.Blendish.Types
import Graphics.NanoVG.Blendish.Theme
import Graphics.NanoVG.Blendish.Utils
import Graphics.NanoVG.Blendish.Monad.Combinators
import Graphics.NanoVG.Blendish.Monad.Primitives
import Graphics.NanoVG.Blendish.Monad.Wrappers

import NanoVG (Color)
import Linear (V2(V2))


toolButton
  :: V2 Float
  -> V2 Float
  -> Corners Bool
  -> WidgetFocus
  -> Maybe Icon
  -> Maybe Text
  -> Draw ()
toolButton pos sz corners state mIcon mLabel = do
  let cf = selectCorners bndToolRadius corners

  Theme{..} <- theme

  bevelInset pos sz cf tBg
  let (i1, i2) = innerColors state tTool False

  innerBox   pos sz cf i1 i2
  outlineBox pos sz cf (trans (wtOutline tTool))

  let tc = textColor state tTool
  iconLabelValue tIcons pos sz mIcon tc ACenter tFont tFontSize mLabel Nothing

radioButton
  :: V2 Float
  -> V2 Float
  -> Corners Bool
  -> WidgetFocus
  -> Maybe Icon
  -> Maybe Text
  -> Draw ()
radioButton pos sz corners state mIcon mLabel = do
  let cf = selectCorners bndToolRadius corners

  Theme{..} <- theme

  bevelInset pos sz cf tBg
  let (i1, i2) = innerColors state tRadio False

  innerBox   pos sz cf i1 i2
  outlineBox pos sz cf (trans (wtOutline tRadio))

  let tc = textColor state tRadio
  iconLabelValue tIcons pos sz mIcon tc ACenter tFont tFontSize mLabel Nothing

choiceButton
  :: V2 Float
  -> V2 Float
  -> Corners Bool
  -> WidgetFocus
  -> Maybe Icon
  -> Maybe Text
  -> Draw ()
choiceButton pos@(V2 x y) sz@(V2 w _h) corners state mIcon mLabel = do
  let cf = selectCorners bndToolRadius corners

  Theme{..} <- theme

  bevelInset pos sz cf tBg
  let (i1, i2) = innerColors state tChoice False

  innerBox   pos sz cf i1 i2
  outlineBox pos sz cf (trans (wtOutline tChoice))

  let tc = textColor state tChoice
  iconLabelValue tIcons pos sz mIcon tc ACenter tFont tFontSize mLabel Nothing
  upDownArrow (V2 (x + w - 10) (y + 10)) 5 (trans (wtItem tChoice))

optionButton
  :: V2 Float
  -> V2 Float
  -> WidgetFocus
  -> Text
  -> Draw ()
optionButton (V2 x y) (V2 w h) state labelText = do
  Theme{..} <- theme

  let (i1, i2) = innerColors state tOption False
      corners = pure bndOptionRadius
      checkBoxPos = V2 x (y + h - bndOptionHeight - 3)
      checkBoxSize = V2 bndOptionWidth bndOptionHeight

  bevelInset checkBoxPos checkBoxSize (pure bndOptionRadius) tBg
  innerBox   checkBoxPos checkBoxSize corners i1 i2
  outlineBox checkBoxPos checkBoxSize corners (trans (wtOutline tOption))

  when (state == ActiveFocus) $ do
    check checkBoxPos (trans (wtItem tOption))

  let tc = textColor state tOption
  iconLabelValue tIcons (V2 (x + 12) y) (V2 (w - 12) h) Nothing tc ALeft tFont tFontSize (Just labelText) Nothing

colorButton
  :: V2 Float
  -> V2 Float
  -> Corners Bool
  -> Color
  -> Draw ()
colorButton pos sz corners color = do
  let cf = selectCorners bndToolRadius corners

  Theme{..} <- theme

  bevelInset pos sz cf tBg
  innerBox   pos sz cf color color
  outlineBox pos sz cf (trans (wtOutline tTool))

numberField
  :: V2 Float
  -> V2 Float
  -> Corners Bool
  -> WidgetFocus
  -> Text
  -> Text
  -> Draw ()
numberField pos@(V2 x y) sz@(V2 w _h) corners focus labelText value = do
  let cf = selectCorners bndNumberRadius corners

  Theme{..} <- theme

  bevelInset pos sz cf tBg
  let (i1, i2) = innerColors focus tNumberField False

  innerBox   pos sz cf i1 i2
  outlineBox pos sz cf (trans (wtOutline tNumberField))

  let tc = textColor focus tNumberField
  iconLabelValue tIcons pos sz Nothing tc ACenter tFont tFontSize (Just labelText) (Just value)
  arrow (V2 (x + 8) (y + 10)) (-bndNumberArrowSize) (trans (wtItem tNumberField))
  arrow (V2 (x + w - 8) (y + 10)) bndNumberArrowSize (trans (wtItem tNumberField))

label
  :: V2 Float
  -> V2 Float
  -> Maybe Icon
  -> Text
  -> Draw ()
label pos sz mIcon labelText = do
  Theme{..} <- theme
  let tc = wtText tRegular
  iconLabelValue tIcons pos sz mIcon tc ACenter tFont tFontSize (Just labelText) Nothing

slider
  :: V2 Float
  -> V2 Float
  -> Corners Bool
  -> WidgetFocus
  -> Float -- ^ progress
  -> Text
  -> Text
  -> Draw ()
slider pos sz@(V2 w h) corners focus progress labelText value = do
  let cf = selectCorners bndNumberRadius corners

  Theme{..} <- theme

  bevelInset pos sz cf tBg
  let (i1, i2) = innerColors focus tSlider False

  innerBox   pos sz cf i1 i2

  let (innerShade1, innerShade2) = case focus of
        ActiveFocus ->
          ( offsetColor (wtItem tSlider) (wtShadeTop tSlider)
          , offsetColor (wtItem tSlider) (wtShadeDown tSlider)
          )
        _ ->
          ( offsetColor (wtItem tSlider) (wtShadeDown tSlider)
          , offsetColor (wtItem tSlider) (wtShadeTop tSlider)
          )

  withScissor pos (V2 (8 + (w - 8) * clamp progress 0 1) h) $
    innerBox pos sz cf innerShade1 innerShade2

  outlineBox pos sz cf (trans (wtOutline tSlider))

  let tc = textColor focus tSlider
  iconLabelValue tIcons pos sz Nothing tc ACenter tFont tFontSize (Just labelText) (Just value)

scrollbar
  :: V2 Float
  -> V2 Float
  -> WidgetFocus
  -> Float -- ^ offset
  -> Float -- ^ size
  -> Draw ()
scrollbar pos@(V2 x y) sz@(V2 w h) focus offset size = do
  Theme{..} <- theme

  bevelInset pos sz (pure bndScrollBarRadius) tBg
  innerBox pos sz (pure bndScrollBarRadius)
    (offsetColor (wtInner tScrollBar) (3 * wtShadeDown tScrollBar))
    (offsetColor (wtInner tScrollBar) (3 * wtShadeTop tScrollBar))

  outlineBox pos sz (pure bndScrollBarRadius) (wtOutline tScrollBar)

  let itemColor = offsetColor (wtItem tScrollBar) (if focus == ActiveFocus then bndScrollBarActiveShade else 0)
  let (handlePos, handleSize) =
        let cSize = clamp size 0 1
            cOffset = clamp offset 0 1
        in
        if h > w then let hs = max (cSize * h) (w + 1) in (V2 x (y + (h-hs)*cOffset), V2 w hs)
                 else let ws = max (cSize * w) (h - 1) in (V2 (x + (w-ws)*cOffset) y, V2 ws h)

  innerBox handlePos handleSize (pure bndScrollBarRadius)
    (offsetColor itemColor (3 * wtShadeDown tScrollBar))
    (offsetColor itemColor (3 * wtShadeTop tScrollBar))
  outlineBox handlePos handleSize (pure bndScrollBarRadius) (trans $ wtOutline tScrollBar)

tooltipBackground
  :: V2 Float
  -> V2 Float
  -> Draw ()
tooltipBackground pos sz@(V2 w h) = do
  Theme{..} <- theme

  let cf = pure bndMenuRadius
      (i1, i2) = innerColors NoFocus tToolTip False

  innerBox   pos (V2 w (h+1)) cf i1 i2
  outlineBox pos (V2 w (h+1)) cf (trans (wtOutline tToolTip))
  dropShadow pos sz bndMenuRadius bndShadowFeather bndShadowAlpha

tooltip
  :: V2 Float
  -> V2 Float
  -> Text
  -> Draw ()
tooltip pos@(V2 x y) sz@(V2 w h) txt = do
  tooltipBackground pos sz
  fillColor (trans white)
  -- XXX: this fails if textBox breaks lines
  textBox (V2 (x + fromIntegral bndPadRight)  (y + h - bndTextPadDown)) w txt

menuBackground
  :: V2 Float
  -> V2 Float
  -> Corners Bool
  -> Draw ()
menuBackground pos sz@(V2 w h) corners = do
  Theme{..} <- theme

  let cf = selectCorners bndToolRadius corners
      (i1, i2) = innerColors NoFocus tMenu False

  innerBox   pos (V2 w (h+1)) cf i1 i2
  outlineBox pos (V2 w (h+1)) cf (trans (wtOutline tMenu))
  dropShadow pos sz bndMenuRadius bndShadowFeather bndShadowAlpha

nodeBackground
  :: V2 Float
  -> V2 Float
  -> WidgetFocus
  -> Maybe Icon
  -> Text
  -> Color
  -> Bool
  -> Draw ()
nodeBackground pos@(V2 x y) sz@(V2 w h) focus mIcon title titleColor showArrow = do
  Theme{..} <- theme
  let cf = pure bndNodeRadius

  innerBox
    pos
    (V2 w (bndNodeTitleHeight + 2))
    (Corners bndNodeRadius bndNodeRadius 0 0)
    (trans (offsetColor titleColor bndBevelShade))
    (trans titleColor)

  innerBox
    (V2 x (y + bndNodeTitleHeight))
    (V2 w (h + 2 - bndNodeTitleHeight))
    (Corners 0 0 bndNodeRadius bndNodeRadius)
    (trans (ntNodeBackdrop tNode))
    (trans (ntNodeBackdrop tNode))

  nodeIconLabel
    tIcons
    (V2 (x + bndNodeArrowAreaWidth) y)
    (V2 (w - bndNodeArrowAreaWidth - bndNodeMarginSide) bndNodeTitleHeight)
    mIcon
    (wtText tRegular)
    (offsetColor titleColor bndBevelShade)
    ALeft
    tFont
    bndLabelFontSize
    title

  let (borderColor, arrowColor) = case focus of
        NoFocus -> (black, offsetColor titleColor (-bndBevelShade))
        HasFocus -> (ntSelected tNode, ntSelected tNode)
        ActiveFocus -> (ntActiveNode tNode, ntSelected tNode)

  outlineBox pos (V2 w (h + 1)) cf (trans borderColor)
  when showArrow $
    nodeArrowDown (V2 (x + bndNodeMarginSide) (y + bndNodeTitleHeight - 4)) bndNodeArrowSize arrowColor
  dropShadow pos sz bndNodeRadius bndShadowFeather bndShadowAlpha

coloredNodeWire
  :: V2 Float
  -> V2 Float
  -> Color
  -> Color
  -> Draw ()
coloredNodeWire pos1@(V2 x0 y0) pos2@(V2 x1 y1) color0 color1 = do
  Theme{..} <- theme

  let len = max (abs (x1 - x0)) (abs (y1 - y0))
      delta = len * (ntNoodleCurving tNode / 10)
      color = setAlpha
                (ntWires tNode)
                $ if getAlpha color0 < getAlpha color1
                    then getAlpha color0
                    else getAlpha color1

      draw = do
        moveTo pos1
        bezierTo (V2 (x0 + delta) y0) (V2 (x1 - delta) y1) pos2

  withStrokeColor color $ do
    strokeWidth bndNodeWireOutlineWidth
    draw

  withStroke
    (do
      pain <- linearGradient (V2 x0 y1) (V2 x1 y1) color0 color1
      strokePaint pain
    )
    (do
      strokeWidth bndNodeWireWidth
      draw
    )

  -- XXX: this is weird
  -- reset stroke width
  strokeWidth 1

nodeWire
  :: V2 Float
  -> V2 Float
  -> WidgetFocus
  -> WidgetFocus
  -> Draw ()
nodeWire pos sz focus0 focus1 = do
  nt <- tNode <$> theme
  coloredNodeWire pos sz
    (wireColor nt focus0)
    (wireColor nt focus1)
  where wireColor _nodeTheme NoFocus = rgbf1 0.5
        wireColor nodeTheme HasFocus = ntWireSelect nodeTheme
        wireColor nodeTheme ActiveFocus = ntActiveNode nodeTheme

-- | Draw triangles in top right and bottom left corners marking resizable area
splitter
  :: V2 Float
  -> V2 Float
  -> Draw ()
splitter (V2 x y) (V2 w h) = do
  Theme{..} <- theme
  let insetLight = offsetColor tBg bndSplitterShade
      insetDark = trans $ offsetColor tBg (-bndSplitterShade)
      inset = trans tBg
      (x2, y2) = (x + w, y + h)
      _tops :: [Int]
      _tops = [13, 9, 5]
      _bots :: [Int]
      _bots = [11, 7, 3]

  withStrokeColor insetDark $ do
    forM_ [13, 9, 5] $ \k -> do
      moveTo (V2 x (y2 - k))
      lineTo (V2 (x + k) y2)

    forM_ [11, 7, 3] $ \k -> do
      moveTo (V2 (x2 - k) y)
      lineTo (V2 x2 (y + k))

  withStrokeColor insetLight $ do
    forM_ [11, 7, 3] $ \k -> do
      moveTo (V2 x (y2 - k))
      lineTo (V2 (x + k) y2)

    forM_ [13, 9, 5] $ \k -> do
      moveTo (V2 (x2 - k) y)
      lineTo (V2 x2 (y + k))

  withStrokeColor inset $ do
    forM_ [12, 8, 4] $ \k -> do
      moveTo (V2 x (y2 - k))
      lineTo (V2 (x + k) y2)

    forM_ [12, 8, 4] $ \k -> do
      moveTo (V2 (x2 - k) y)
      lineTo (V2 x2 (y + k))

menuLabel
  :: V2 Float
  -> V2 Float
  -> Maybe Icon
  -> Text
  -> Draw ()
menuLabel pos sz mIcon labelText = do
  Theme{..} <- theme
  iconLabelValue
    tIcons
    pos
    sz
    mIcon
    (textColor NoFocus tMenu)
    ALeft
    tFont
    bndLabelFontSize
    (Just labelText)
    Nothing

menuItem
  :: V2 Float
  -> V2 Float
  -> Maybe Icon
  -> WidgetFocus
  -> Text
  -> Draw ()
menuItem pos sz mIcon focus labelText = do
  Theme{..} <- theme
  focus' <- case focus of
    NoFocus -> return focus
    _ -> do
      innerBox
        pos
        sz
        (pure 0)
        (offsetColor (wtInnerSelected tMenuItem) (wtShadeTop tMenuItem))
        (offsetColor (wtInnerSelected tMenuItem) (wtShadeDown tMenuItem))
      return ActiveFocus

  iconLabelValue
    tIcons
    pos
    sz
    mIcon
    (textColor focus' tMenuItem)
    ALeft
    tFont
    bndLabelFontSize
    (Just labelText)
    Nothing

textField
  :: V2 Float
  -> V2 Float
  -> Corners Bool
  -> WidgetFocus
  -> Maybe Icon
  -> Text
  -> Int -- ^ Caret start index
  -> Int -- ^ Caret end index
  -> Draw ()
textField pos sz corners focus mIcon txt caretBegin caretEnd = do
  let cf = selectCorners bndTextRadius corners

  Theme{..} <- theme

  bevelInset pos sz cf tBg
  let (i1, i2) = innerColors focus tTextField False

  innerBox   pos sz cf i1 i2
  outlineBox pos sz cf (trans (wtOutline tTextField))

  let tc = textColor focus tTextField
      (V2 x y) = pos

  case mIcon of
    Nothing -> return ()
    Just iconId -> icon tIcons (x + 4) (y + 2) iconId

  let pLeft = 3 + maybe 0 (pure (bndIconSheetRes + 3)) mIcon
      pos' = V2 (x + fromIntegral pLeft) y
  labelCarret pos' sz tc tFontSize txt caretBegin caretEnd

