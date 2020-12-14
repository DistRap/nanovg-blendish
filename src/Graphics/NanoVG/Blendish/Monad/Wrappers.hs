-- | Boring wrappers

module Graphics.NanoVG.Blendish.Monad.Wrappers where

import Data.Text (Text)
import Data.Set (Set)
import Foreign.C.Types (CFloat(CFloat))
import NanoVG (Color(..), Paint(..), Image, LineCap, Bounds)

import qualified NanoVG

import Graphics.NanoVG.Blendish.Context

import Linear (V2(V2), V4(V4))

beginPath :: Draw ()
beginPath = withCtx NanoVG.beginPath

closePath :: Draw ()
closePath = withCtx NanoVG.closePath

scissor :: V2 Float -> V2 Float -> Draw ()
scissor (V2 x y) (V2 w h) = withCtx $ \c -> NanoVG.scissor c (cvt x) (cvt y) (cvt w) (cvt h)

resetScissor :: Draw ()
resetScissor = withCtx NanoVG.resetScissor

moveTo :: V2 Float -> Draw ()
moveTo (V2 x y) = withCtx $ \c -> NanoVG.moveTo c (cvt x) (cvt y)

lineTo :: V2 Float -> Draw ()
lineTo (V2 x y) = withCtx $ \c -> NanoVG.lineTo c (cvt x) (cvt y)

arcTo :: V2 Float -> V2 Float -> Float -> Draw ()
arcTo (V2 x y) (V2 j k) r = withCtx $ \c ->
  NanoVG.arcTo c (cvt x) (cvt y) (cvt j) (cvt k) (cvt r)

rect :: V2 Float -> V2 Float -> Draw ()
rect (V2 x y) (V2 w h) = withCtx $ \c ->
  NanoVG.rect c (cvt x) (cvt y) (cvt w) (cvt h)

fillColor :: Color -> Draw ()
fillColor x = withCtx $ \c -> NanoVG.fillColor c x

fillPaint :: Paint -> Draw ()
fillPaint = withCtx . flip NanoVG.fillPaint

fill :: Draw ()
fill = withCtx NanoVG.fill

strokeWidth :: Float -> Draw ()
strokeWidth w = withCtx $ \c -> NanoVG.strokeWidth c (cvt w)

strokeColor :: Color -> Draw ()
strokeColor x = withCtx $ \c -> NanoVG.strokeColor c x

strokePaint :: Paint -> Draw ()
strokePaint = withCtx . flip NanoVG.strokePaint

stroke :: Draw ()
stroke = withCtx NanoVG.stroke

cvt :: Float -> CFloat
cvt = CFloat

boxGradient
  :: V2 Float -- ^ Box origin
  -> V2 Float -- ^ Box size
  -> Float -- ^ Radius
  -> Float -- ^ Feather
  -> Color
  -> Color
  -> Draw Paint
boxGradient (V2 x y) (V2 w h) radius' feather' color1 color2 = withCtx $ \c ->
  NanoVG.boxGradient c (CFloat x) (CFloat y) (CFloat w) (CFloat h) (CFloat radius') (CFloat feather') color1 color2

linearGradient
  :: V2 Float
  -> V2 Float
  -> Color
  -> Color
  -> Draw Paint
linearGradient (V2 x y) (V2 a b) color1 color2 = withCtx $ \c ->
  NanoVG.linearGradient c (cvt x) (cvt y) (cvt a) (cvt b) color1 color2

imagePattern
  :: V2 Float
  -> V2 Float
  -> Float
  -> Image
  -> Float
  -> Draw Paint
imagePattern (V2 x y) (V2 w h) r img a = withCtx $ \c ->
  NanoVG.imagePattern c (cvt x) (cvt y) (cvt w) (cvt h) (cvt r) img (cvt a)

lineCap :: LineCap -> Draw ()
lineCap = withCtx . flip NanoVG.lineCap

lineJoin :: LineCap -> Draw ()
lineJoin = withCtx . flip NanoVG.lineJoin

fontFace :: Text -> Draw ()
fontFace = withCtx . flip NanoVG.fontFace

fontSize :: Float -> Draw ()
fontSize = withCtx . flip NanoVG.fontSize . cvt

fontBlur :: Float -> Draw ()
fontBlur = withCtx . flip NanoVG.fontBlur . cvt

text :: V2 Float -> Text -> Draw ()
text (V2 x y) txt = withCtx $ \c -> NanoVG.text c (cvt x) (cvt y) txt

textBox :: V2 Float -> Float -> Text -> Draw ()
textBox (V2 x y) w txt = withCtx $ \c -> NanoVG.textBox c (cvt x) (cvt y) (cvt w) txt

textAlign :: Set NanoVG.Align -> Draw ()
textAlign = withCtx . flip NanoVG.textAlign

cvtBounds :: Bounds -> V4 Float
cvtBounds (NanoVG.Bounds (NanoVG.V4 (CFloat x0) (CFloat y0) (CFloat x1) (CFloat y1)))
  = V4 x0 y0 x1 y1

textBounds :: Float -> Float -> Text -> Draw (V4 Float)
textBounds a b t = withCtx $ \c ->
  NanoVG.textBounds c (cvt a) (cvt b) t >>= return . cvtBounds

textBoxBounds :: Float -> Float -> Float -> Text -> Draw (V4 Float)
textBoxBounds a b w t = withCtx $ \c ->
  NanoVG.textBoxBounds c (cvt a) (cvt b) (cvt w) t >>= return . cvtBounds
