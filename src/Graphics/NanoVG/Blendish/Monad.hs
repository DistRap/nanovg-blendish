{-# LANGUAGE RecordWildCards #-}

module Graphics.NanoVG.Blendish.Monad where

import Graphics.NanoVG.Blendish.Context
import Graphics.NanoVG.Blendish.Icon
import Graphics.NanoVG.Blendish.Types
import Graphics.NanoVG.Blendish.Theme
import Graphics.NanoVG.Blendish.Utils
--import Graphics.NanoVG.Blendish.Monad.Combinators
import Graphics.NanoVG.Blendish.Monad.Primitives
--import Graphics.NanoVG.Blendish.Monad.Wrappers

import Linear (V2(V2))
-- Themed

toolButton
  :: V2 Float
  -> V2 Float
  -> [SharpCorner]
  -> WidgetFocus
  -> Maybe Icon
  -> Maybe String
  -> Draw ()
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
  -> Draw ()
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
  -> Draw ()
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
