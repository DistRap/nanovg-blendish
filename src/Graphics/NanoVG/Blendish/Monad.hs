{-# LANGUAGE RecordWildCards #-}

module Graphics.NanoVG.Blendish.Monad where

import Data.Text (Text)

import Graphics.NanoVG.Blendish.Context
import Graphics.NanoVG.Blendish.Icon
import Graphics.NanoVG.Blendish.Types
import Graphics.NanoVG.Blendish.Theme
import Graphics.NanoVG.Blendish.Utils
--import Graphics.NanoVG.Blendish.Monad.Combinators
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
  let (i1, i2) = innerColors state tTool True

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
  let (i1, i2) = innerColors state tRadio True

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
  let (i1, i2) = innerColors state tChoice True

  innerBox   pos sz cf i1 i2
  outlineBox pos sz cf (trans (wtOutline tChoice))

  let tc = textColor state tChoice
  iconLabelValue tIcons pos sz mIcon tc ACenter tFont tFontSize mLabel Nothing
  upDownArrow (V2 (x + w - 10) (y + 10)) 5 (trans (wtItem tChoice))
