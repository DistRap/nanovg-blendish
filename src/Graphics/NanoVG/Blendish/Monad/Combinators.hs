{-# LANGUAGE RecordWildCards #-}

module Graphics.NanoVG.Blendish.Monad.Combinators where

import NanoVG (Color(..))

import Graphics.NanoVG.Blendish.Context
import Graphics.NanoVG.Blendish.Monad.Wrappers

withFill
  :: Draw ()
  -> Draw ()
  -> Draw ()
withFill paintAct act = do
  beginPath
  act
  paintAct
  fill

withFillColor
  :: Color
  -> Draw ()
  -> Draw ()
withFillColor bgColor = withFill (fillColor bgColor)

withStroke
  :: Draw ()
  -> Draw ()
  -> Draw ()
withStroke paintAct act = do
  beginPath
  act
  paintAct
  stroke

withStrokeColor
  :: Color
  -> Draw ()
  -> Draw ()
withStrokeColor bgColor = withStroke (strokeColor bgColor)
