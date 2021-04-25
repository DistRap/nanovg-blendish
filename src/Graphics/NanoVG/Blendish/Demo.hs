{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphics.NanoVG.Blendish.Demo where

import Graphics.NanoVG.Blendish.Context
import Graphics.NanoVG.Blendish.Icon
import Graphics.NanoVG.Blendish.Monad
import Graphics.NanoVG.Blendish.Monad.Primitives
import Graphics.NanoVG.Blendish.Theme
import Graphics.NanoVG.Blendish.Types
import Graphics.NanoVG.Blendish.Utils

import Linear

demoUI :: Draw ()
demoUI = do
  let sp = bndWidgetHeight + 5
  Theme{..} <- theme
  background (V2 0 0) (V2 600 600) tBg
  splitter (V2 0 0) (V2 600 600)

  label (V2 0 bndWidgetHeight) (V2 100 10) Nothing "Demo Label"

  toolButton (V2 100 10) (V2 200 21)
    (pure True) NoFocus (Just Icon'Particles) (Just "Tool button")

  toolButton (V2 100 (10 + sp)) (V2 200 bndWidgetHeight)
    (pure True) HasFocus (Just Icon'Speaker) (Just "Focus button")

  toolButton (V2 100 (10 + (2*sp))) (V2 200 bndWidgetHeight)
    (pure True) ActiveFocus (Just Icon'Physics) (Just "Active button")

  radioButton (V2 100 (10 + (3*sp))) (V2 200 bndWidgetHeight)
    (pure True) NoFocus (Just Icon'9_AA) (Just "Radio button")

  choiceButton (V2 100 (10 + (4*sp))) (V2 200 bndWidgetHeight)
    (pure True) NoFocus (Just Icon'Bookmarks) (Just "Choice button")

  --colorButton (V2 100 (10 + (5*sp))) (V2 200 bndWidgetHeight)
  --  (pure True) (rgbf 0.5 0.3 0.2)
  --
  numberField (V2 100 (10 + (5*sp))) (V2 200 bndWidgetHeight)
    (pure True) NoFocus "Value" "3000"

  numberField (V2 100 (10 + (6*sp))) (V2 200 bndWidgetHeight)
    (pure True) HasFocus "Focus Val" "666"

  numberField (V2 100 (10 + (7*sp))) (V2 200 bndWidgetHeight)
    (pure True) ActiveFocus "Active Val" "1337"

  optionButton (V2 100 (10 + (8*sp))) (V2 200 bndWidgetHeight)
    NoFocus "Option"
  optionButton (V2 100 (10 + (9*sp))) (V2 200 bndWidgetHeight)
    HasFocus "Focused Option"
  optionButton (V2 100 (10 + (10*sp))) (V2 200 bndWidgetHeight)
    ActiveFocus "Active option"
  let row2 = 100
  slider (V2 row2 (10 + (11*sp))) (V2 200 bndWidgetHeight)
    (pure True) NoFocus 0.5 "Default slider" "0.5"
  slider (V2 row2 (10 + (12*sp))) (V2 200 bndWidgetHeight)
    (pure True) HasFocus 0.7 "Focused slider" "0.7"
  slider (V2 row2 (10 + (13*sp))) (V2 200 bndWidgetHeight)
    (pure True) ActiveFocus 0.9 "Active slider" "0.9"

  scrollbar (V2 20 30) (V2 10 100)
    NoFocus 0.5 0.4
  scrollbar (V2 40 30) (V2 10 100)
    HasFocus 0.5 0.4
  scrollbar (V2 60 30) (V2 10 100)
    ActiveFocus 0.5 0.4

  --tooltipBackground (V2 10 150) (V2 60 60)
  tooltip (V2 10 150) (V2 120 20) "This is a tooltip"

  menuBackground (V2 260 230) (V2 120 100) (pure True)
  menuLabel (V2 260 230) (V2 150 bndWidgetHeight) (Just Icon'MenuPanel) "Menu label"
  menuItem (V2 260 (bndWidgetHeight - 2 + 230)) (V2 120 bndWidgetHeight) (Just Icon'Blender) NoFocus "Menu item"
  menuItem (V2 260 (2*(bndWidgetHeight - 2) + 230)) (V2 120 bndWidgetHeight) (Just Icon'Trash) HasFocus "Focused"
  menuItem (V2 260 (3*(bndWidgetHeight - 2) + 230)) (V2 120 bndWidgetHeight) (Just Icon'Options) ActiveFocus "Active"

  --coloredNodeWire (V2 160 460) (V2 200 440) red green
  nodeWire (V2 160 460) (V2 200 440) NoFocus ActiveFocus

  nodeBackground (V2 200 400) (V2 150 180)
    NoFocus (Just Icon'ArrowLeftright) "Translate" (rgbf1 0.392) True

  label (V2 200 440) (V2 30 10) Nothing "In"
  label (V2 280 460) (V2 100 10) Nothing "Out"
  nodePort (V2 200 440) NoFocus (rgbf1 0.5)
  nodePort (V2 350 460) NoFocus (rgbf1 0.5)

  numberField (V2 220 (480 + (0*sp))) (V2 100 bndWidgetHeight)
    (pure True) HasFocus "X" "0"

  numberField (V2 220 (480 + (1*sp))) (V2 100 bndWidgetHeight)
    (pure True) HasFocus "Y" "10"

  numberField (V2 220 (480 + (2*sp))) (V2 100 bndWidgetHeight)
    (pure True) HasFocus "Z" "10"

  nodeBackground (V2 10 400) (V2 150 180)
    HasFocus (Just Icon'Matcube) "Cube" (rgbf1 0.392) True

  numberField (V2 20 (480 + (0*sp))) (V2 120 bndWidgetHeight)
    (pure True) HasFocus "Round" "10"

  label (V2 90 460) (V2 100 10) Nothing "Out"
  nodePort (V2 160 460) HasFocus (rgbf1 0.5)

  let t = "Test text bla lorem ipsum jumps over lazy cat"
  textField (V2 310 010) (V2 100 100) (pure True) NoFocus Nothing
    t
    0 42
  caretP (V2 313 010) (V2 100 100) t

  textField (V2 310 110) (V2 100 100) (pure True) NoFocus Nothing
    t
    1 41

  textField (V2 420 010) (V2 100 bndWidgetHeight) (pure True) NoFocus Nothing
    "Crazy cat"
    0 0

  textField (V2 420 (010 + sp)) (V2 100 bndWidgetHeight) (pure True) NoFocus Nothing
    "Crazy fox"
    6 6

  textField (V2 420 (010 + (2 * sp))) (V2 100 bndWidgetHeight) (pure True) NoFocus Nothing
    "Lazy cat"
    5 5

  textField (V2 420 (010 + (3 *sp))) (V2 100 bndWidgetHeight) (pure True) NoFocus Nothing
    "Lazy dog"
    8 8

  textField (V2 420 (010 + (4 *sp))) (V2 100 bndWidgetHeight) (pure True) NoFocus Nothing
    "Foxy cat"
    1 7

  textField (V2 420 (010 + (5 *sp))) (V2 100 bndWidgetHeight) (pure True) NoFocus (Just Icon'GhostEnabled)
    "Foxy owl"
    0 8
