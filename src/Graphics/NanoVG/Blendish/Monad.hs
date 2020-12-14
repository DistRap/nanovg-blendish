{-# LANGUAGE RecordWildCards #-}

module Graphics.NanoVG.Blendish.Monad where

import Control.Monad (when)
import Data.Text (Text)

import Graphics.NanoVG.Blendish.Context
import Graphics.NanoVG.Blendish.Icon
import Graphics.NanoVG.Blendish.Types
import Graphics.NanoVG.Blendish.Theme
import Graphics.NanoVG.Blendish.Utils
import Graphics.NanoVG.Blendish.Monad.Combinators
import Graphics.NanoVG.Blendish.Monad.Primitives
--import Graphics.NanoVG.Blendish.Monad.Wrappers

import NanoVG (Color)
import Linear (V2(V2))
-- Themed

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
        if h > w then let hs = max (cSize * h) (w + 1) in ((V2 x (y + (h-hs)*cOffset)), (V2 w hs))
                 else let ws = max (cSize * w) (h - 1) in ((V2 (x + (w-ws)*cOffset) y), (V2 ws h))

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
