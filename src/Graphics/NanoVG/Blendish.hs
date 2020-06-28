{-# LANGUAGE PackageImports, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- net
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.NanoVG.Blendish where

import "GLFW-b" Graphics.UI.GLFW as GLFW hiding (Image)
import qualified Data.Map as M
import qualified Data.Vector as V

--import LambdaCube.GL as LC -- renderer
--import LambdaCube.GL.Mesh as LC

--import Codec.Picture as Juicy

--import Data.Aeson
import qualified Data.ByteString as SB

--import qualified Linear as L
--import Icon
--import Matrix

-- 2D
import           Data.Bits hiding (rotate)
--import "gl"      Graphics.GL.Core32
--import qualified Graphics.NanoVG.Picture as P -- NVGS
import qualified Data.Set as S


import NanoVG (Color(..), Font, CreateFlags(..), rgba, rgbf, rgbaf)
import qualified NanoVG
import           Foreign.C.Types

import Data.Text (Text, pack, unpack)
import Data.List
import Data.Char (toLower, toUpper, ord, chr)

-- loadDat
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Maybe

-- net
import Control.Event.Handler

import Reactive.Banana
import Reactive.Banana.Frameworks

import Control.Concurrent.STM hiding (check)-- .TVar 

-- ours
import Graphics.NanoVG.Blendish.Icon
import Graphics.NanoVG.Blendish.Context
import Graphics.NanoVG.Blendish.Shorthand
import Graphics.NanoVG.Blendish.Types
import Graphics.NanoVG.Blendish.Theme
import Graphics.NanoVG.Blendish.Utils

foreign import ccall unsafe "initGlew"
  glewInit :: IO CInt

labelWidth mIconId label font = do
  let w = bndPadLeft + bndPadRight + (maybe 0 (pure bndIconSheetRes) mIconId)

  Theme{..} <- theme

  -- when nonempty label
  fontFace tFont
  fontSize $ fromIntegral tFontSize
  tw <- textWidth label
  return $ w + tw

textWidth label = do
  (b0, _b1, b2, _b3) <- textBounds 1 1 label
  return (round $ b2 - b0)

textWidth' label = do
  (b0, _b1, b2, _b3) <- textBounds 1 1 label
  return (b2 - b0)

textWidthF label = do
  (CFloat b0, _b1, CFloat b2, _b3) <- textBounds 1 1 label
  return (b2 - b0)




labelHeight mIconId label width' = do
  let h = bndWidgetHeight
      width = width' - (bndTextRadius * 2) - (maybe bndIconSheetRes id mIconId)

  Theme{..} <- theme

  -- when nonempty label
  fontFace tFont
  fontSize $ fromIntegral tFontSize

  (_b0, b1, _b2, b3) <- textBoxBounds 1 1 (fromIntegral width) label
  let bh = (round $ b3 - b1) + bndTextPadDown
  return $ if bh > h then bh else h
{--
--}

{--
--main :: IO ()
main = do
    Just pipelineDesc <- decodeStrict <$> SB.readFile "tutorial.json"


    win <- initWindow "LambdaCube 3D DSL Hello World" 640 640

    -- setup render data
    let inputSchema = makeSchema $ do
          defObjectArray "objects" Triangles $ do
            "position"  @: Attribute_V3F
            "uv"        @: Attribute_V2F
          defUniforms $ do
            "time"           @: Float
            "diffuseTexture" @: FTexture2D
            "model"          @: M44F
            "cam"            @: M44F
            "proj"           @: M44F

    storage <- LC.allocStorage inputSchema

    -- upload geometry to GPU and add to pipeline input
    m <- LC.uploadMeshToGPU cubeMesh
    LC.addMeshToObjectArray storage "objects" [] m
    --LC.uploadMeshToGPU triangleB >>= LC.addMeshToObjectArray storage "objects" []

    -- load image and upload texture
    Right img <- Juicy.readImage "logo.png"
    textureData <- LC.uploadTexture2DToGPU img

    -- allocate GL pipeline
    renderer <- LC.allocRenderer pipelineDesc

    c@(NanoVG.Context _c') <- NanoVG.createGL3 (S.fromList [Antialias,StencilStrokes,Debug])
    --fbo' <- alloca (\ptr -> glGenFramebuffers 1 ptr >> peek ptr)

    mdata <- runMaybeT $ loadData c
    da <- case mdata of
      Nothing -> error "Unable to load data"
      Just x -> return x

    --GLFW.setMouseButtonCallback win . pure $

    LC.setStorage renderer storage >>= \case -- check schema compatibility
      Just err -> putStrLn err
      Nothing  -> loop
        where loop = do
                -- update graphics input
                (w,h) <- GLFW.getWindowSize win >>= \(w,h) -> do
                  LC.setScreenSize storage (fromIntegral w) (fromIntegral h)
                  return (w, h)

                Just t <- GLFW.getTime
                (mx, my) <- GLFW.getCursorPos win
                (fbW, fbH) <- GLFW.getFramebufferSize win
                let pxRatio = fromIntegral fbW / fromIntegral w

                LC.updateUniforms storage $ do
                  "diffuseTexture" @= return textureData
                  "time" @= return (realToFrac t :: Float)
                  --"model" @= return (identity)
                  "model" @= return (convLC $ quatMatrix $ L.axisAngle (L.normalize $ L.V3 1 1 3) 0)
                  "cam"   @= return (cameraMatrix 0)
                  "proj"  @= return (projMatrix (fromIntegral w/ fromIntegral h))

                -- LC
                glViewport 0 0 (fromIntegral fbW) (fromIntegral fbH)
                LC.renderFrame renderer

                -- NANO
                glViewport 0 0 (fromIntegral fbW) (fromIntegral fbH)

                --glViewport 0 0 (fromIntegral w) (fromIntegral h)
                --glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT)
                --glClear (GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT)
                --beginFrame c (fromIntegral w) (fromIntegral h) pxRatio
                glClear (GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT)

                --print (pxRatio, w, h, fbW, fbH, mx, my)
                --NanoVG.beginFrame c (fromIntegral w `div` 2) (fromIntegral h `div` 2) (pxRatio * 2.9)
                mb1 <- GLFW.getMouseButton win MouseButton'1 
                let mb = case mb1 of
                            GLFW.MouseButtonState'Pressed -> [ MouseButton'1 ]
                            _ -> []
                NanoVG.beginFrame c (fromIntegral w `div` 1) (fromIntegral h `div` 1) (pxRatio * 1.0)
                renderUI c da mx my mb
                NanoVG.endFrame c

                --glEnable (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT)
                --glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
                --glEnable GL_CULL_FACE
                --glCullFace GL_BACK

                GLFW.swapBuffers win
                GLFW.pollEvents

                let keyIsPressed k = fmap (==KeyState'Pressed) $ GLFW.getKey win k
                escape <- keyIsPressed Key'Escape
                q <- keyIsPressed Key'Q
                shouldClose <- GLFW.windowShouldClose win
                if (escape || q || shouldClose) then return () else loop

    LC.disposeRenderer renderer
    LC.disposeStorage storage
    GLFW.destroyWindow win
    GLFW.terminate

--}

data NData = NData Font NanoVG.Image


loadData c = do
  --normal <- MaybeT $ NanoVG.createFont c "sans" (NanoVG.FileName "Roboto-Regular.ttf")
  sans <- MaybeT $ NanoVG.createFont c "sans" (NanoVG.FileName "DejaVuSans.ttf")
  icons <- MaybeT $ NanoVG.createImage c (NanoVG.FileName "blender_icons16.png") 0
  pure (NData sans icons)

renderUI ctx (NData _ icons) x y mb = do
  --bg <- linearGradient c x y x (y+h) (rgba 0 16 192 0) (rgba 0 160 192 64)
  --NanoVG.beginPath c
  --NanoVG.rect c 10 10 100 300
  --NanoVG.fillColor c (rgba 255 255 255 32)
  --NanoVG.fill c

  NanoVG.fontSize ctx 148
  NanoVG.fontFace ctx "sans"

  NanoVG.globalAlpha ctx 0.9

  w <- flip runReaderT (UIContext ctx (defTheme icons) (x, y)) $ do
    --withFillColor (rgba 0 0 0 100) $ withCtx $ \c ->
    --  NanoVG.text c 20 250 "Lambdacube nanovg !!"

    dropShadow 10 10 100 100 10 10 10
    withFillColor gray $ roundBox 10 10 100 100 10 10 10 10

    background 100 10 100 300 (rgba 255 255 255 32)

    --bevelInset 10 10 100 100 10 10 somec
    --
    check 100 50 black
    arrow 100 60 10 black
    upDownArrow 50 60 10 black

    let sp = bndWidgetHeight + 5

    Theme{..} <- theme
    background 90 0 240 (10 * sp) tBg
    {--
    toolButton 100 10 200 bndWidgetHeight
      [] NoFocus (Just Icon'Particles) (Just "Tool button")

    toolButton 100 (10 + sp) 200 bndWidgetHeight
      [] HasFocus (Just Icon'Speaker) (Just "Focus button")

--    toolButton 100 (10 + (2*sp)) 200 bndWidgetHeight
--      [] ActiveFocus (Just $ (2 * 256) + 2) "Active button"
--
    toolButton 100 (10 + (2*sp)) 200 bndWidgetHeight
      [] ActiveFocus (Just Icon'Physics) (Just "Active button")

    radioButton 100 (10 + (3*sp)) 200 bndWidgetHeight
      [] NoFocus (Just Icon'9_AA) (Just "Radio button")
  --}


  --(bAddHandler, bRunHandlers) <- h
  --(cAddHandler, cRunHandlers) <- h

  let elemHandler (Button as@Attrs{..}) = do
        (add, run) <- newAddHandler
        let nb = (Button (handler run as))
        return $ (nb, fromAddHandler add)

  let b = Button (ico Icon'Speaker $ label "Up" $ defA ()) -- (Just Icon'Speaker) "Count up"
      c = Button (defA () & ico Icon'Dot & label "Down") -- (Just Icon'Dot) "Count down"


  zz <- newTVarIO $ HBox [
           Rekt 30 100 100 21 b
         , Rekt 127 100 100 21 c
         ]

  (b', bMoment) <- elemHandler b
  (c', cMoment) <- elemHandler c

  -- bricklike
  -- State -> Event -> State
  -- State -> UI
        
        
  --register addHandler (putStrLn . show)

  {--
  let net :: MomentIO ()
      net = do
        --bMoment <- fromAddHandler bAddHandler
        --cMoment <- fromAddHandler cAddHandler
        (counter :: Behavior Int)
            <- accumB 0 $ unions
                [ (+1)       <$ bMoment
                , subtract 1       <$ cMoment
                ]
        c <- changes counter
        reactimate' $ fmap (\x -> print <$> x) c -- moment
        return ()

  cnet <- compile net
  actuate cnet
  --}

  w <- flip runReaderT (UIContext ctx (defTheme icons) (x, y)) 
    $ flip runStateT (defRS x y mb) $ do
       drawElem $
         HBox [
           Rekt 30 100 100 21 b'
         , Rekt 127 100 100 21 c'
         ]
  --print (show $ map fst $ hs b, show $ events $ snd w)

  --runHandlersX [(b'), (c')] $ events $ snd w
  return ()

--hs :: Elem -> [(Elem, Elem -> IO ())]
--hs b = [(b, print)]
--runHandlersX :: [Elem] -> [Elem] -> IO ()
--runHandlersX handlers evts = do
--  forM_ handlers $ \el@(Button as) -> do
--    when (el `elem` evts) $ mapM_ ($ aValue as) (aHandlers as)

runHandlersR :: [(AddHandler Elem, Elem -> IO ())] -> [Elem] -> IO ()
runHandlersR handlers evts = do
  forM_ handlers $ \(_, f) -> do
    unless (null evts) $ f (head evts)
    --when (el `elem` evts) $ f el

h :: IO (AddHandler Elem, Elem -> IO ())
h = newAddHandler

-- mouz
defRS x y mb = RS False False False False (round x, round y) mb []
--  (GLFW.MouseButton'1) -- ,MouseButtonState'Pressed) -- ,ModifierKeys {modifierKeysShift = False, modifierKeysControl = False, modifierKeysAlt = False, modifierKeysSuper = False, modifierKeysCapsLock = False, modifierKeysNumLock = True})
--  ] []


-- 0 0 -> A1 (topleft)
-- 1 0 -> A2 (topleft -> this)
iconIdFromXY x y = x + y `shiftL` 8
ii x y  = Just $ iconIdFromXY x y

-- source/blender/editors/include/UI_icons.h
-- DEF_ICON(NAME)
-- DEF_ICON_BLANK
--
--Icon'9_[

main' = genDefs
genDefs = do
  let f = "/home/srk/git/blender/source/blender/editors/include/UI_icons.h"
  defs <- filter (\x -> "DEF_ICON" `isPrefixOf` x) . lines <$> readFile f
  let uvs = map (\(d, i) -> (d, i `mod` 26, 29 - i `div` 26, isB d)) $ zip defs [0..]
      --noBlanks = filter (\(d, _, _) -> not $ "BLANK(" `isInfixOf` d) uvs
      isB d = "BLANK(" `isInfixOf` d

      casing = -- SYNTAX_ON to SyntaxOn
        filter (/='_')
        . snd
        . mapAccumL (\upcase c -> (c == '_', if upcase then toUpper c else c)) True 
        . map toLower
      pretty = map (\(d, u, v, b) -> (casing $ takeWhile (/=')') $ drop 1 $ dropWhile (/='(') d, u, v, b)) uvs
      cutoff = filter (\(_, _, v, _) -> v >= 0) pretty
      fixOver '[' = "AA"
      fixOver x = [x]
      fmtB True  _ u v = (show $ u + 1) ++ "_" ++ (fixOver $ chr $ (ord 'A') + v)
      fmtB False x _ _ = x

  putStrLn "module Icon (Icon(..), iconXY) where"
  putStrLn ""
  putStrLn "data Icon ="
  putStrLn $ "   Icon'" ++ ((\(x, _, _, _) -> x) $ head cutoff)
  forM_ (tail cutoff) $ \(x, u, v, b) -> putStrLn $ " | Icon'" ++ fmtB b x u v
  putStrLn " deriving (Eq, Show, Ord)"
  putStrLn ""
  putStrLn "iconXY :: Icon -> (Int, Int)"
  forM_ cutoff $ \(x, u, v, b) -> putStrLn $ "iconXY Icon'" ++ fmtB b x u v ++ " = (" ++ (show u) ++ ", " ++ (show v) ++ ")"

-- NVG monad, utils
--
somec = rgba 255 0 100 255

withFill paintAct act = do
  beginPath
  act
  paintAct
  fill

withFillColor bgColor = withFill (fillColor bgColor)

withStroke paintAct act = do
  beginPath
  act
  paintAct
  stroke

withStrokeColor bgColor = withStroke (strokeColor bgColor)

hodl op = ask >>= liftIO . op

beginPath = withCtx NanoVG.beginPath
closePath = withCtx NanoVG.closePath

moveTo x y = withCtx $ \c -> NanoVG.moveTo c x y
lineTo x y = withCtx $ \c -> NanoVG.lineTo c x y
arcTo x y j k r = withCtx $ \c -> NanoVG.arcTo c x y j k r
rect x y w h = withCtx $ \c -> NanoVG.rect c (CFloat x) (CFloat y) (CFloat w) (CFloat h)
rect' x y w h = withCtx $ \c -> NanoVG.rect c (cvt x) (cvt y) (cvt w) (cvt h)
  where cvt = fromIntegral

fillColor x = withCtx $ \c -> NanoVG.fillColor c x
fillPaint = withCtx . flip NanoVG.fillPaint
fill = withCtx NanoVG.fill

strokeWidth w = withCtx $ \c -> NanoVG.strokeWidth c w
strokeColor x = withCtx $ \c -> NanoVG.strokeColor c x

strokePaint = withCtx . flip NanoVG.strokePaint
stroke = withCtx NanoVG.stroke

boxGradient x y w h a b color1 color2 = withCtx $ \c ->
  NanoVG.boxGradient c x y w h a b color1 color2

boxGradientFloat x y w h a b color1 color2 = withCtx $ \c ->
  NanoVG.boxGradient c (cvt x) (cvt y) (cvt w) (cvt h) a b color1 color2
  where cvt = CFloat

linearGradient x y a b color1 color2 = withCtx $ \c ->
  NanoVG.linearGradient c x y a b color1 color2

linearGradient' x y a b color1 color2 = withCtx $ \c ->
  NanoVG.linearGradient c (cvt x) (cvt y) (cvt a) (cvt b) color1 color2
  where cvt = CFloat

imagePattern x y w h r img a = withCtx $ \c ->
  NanoVG.imagePattern c (cvt x) (cvt y) (cvt w) (cvt h) r img a
  where cvt = CFloat

lineCap = withCtx . flip NanoVG.lineCap
lineJoin = withCtx . flip NanoVG.lineJoin

fontFace = withCtx . flip NanoVG.fontFace
fontSize = withCtx . flip NanoVG.fontSize

text x y txt = withCtx $ \c -> NanoVG.text c (cvt x) (cvt y) txt
  where cvt = CFloat

textBox x y w txt = withCtx $ \c -> NanoVG.textBox c (cvt x) (cvt y) (cvt w) txt
  where cvt = CFloat
textAlign = withCtx . flip NanoVG.textAlign

cvtBounds (NanoVG.Bounds (NanoVG.V4 x0 y0 x1 y1)) = (x0, y0, x1, y1)

textBounds a b t = withCtx $ \c -> NanoVG.textBounds c a b t >>= return . cvtBounds
textBoxBounds a b w t = withCtx $ \c -> NanoVG.textBoxBounds c a b w t >>= return . cvtBounds


data Align = ALeft | ARight | ACenter
  deriving (Eq, Show, Ord)

-- blendish prims
--
roundBox x y w h cr0 cr1 cr2 cr3 = do
    beginPath
    moveTo x (y + h*0.5)
    arcTo x       y       (x + w) y       (min cr0 (d / 2))
    arcTo (x + w) y       (x + w) (y + h) (min cr1 (d / 2))
    arcTo (x + w) (y + h) x       (y + h) (min cr2 (d / 2))
    arcTo x       (y + h) x       y       (min cr3 (d / 2))
    closePath
  where
    d = min w h

background x y w h bgColor = withFillColor bgColor $ rect' x y w h

bevel x' y' w' h' bgColor = do
  let x = x' + 0.5
      y = y' + 0.5
      w = w' - 1
      h = h' - 1

      c1 = trans $ offsetColor bgColor bndBevelShade
      c2 = trans $ offsetColor bgColor (-bndBevelShade)

  strokeWidth 1
  withStrokeColor c1 $ do
    moveTo  x      (y + h)
    lineTo (x + w) (y + h)
    lineTo (x + w)  y

  withStrokeColor c2 $ do
    moveTo  x      (y + h)
    lineTo  x       y
    lineTo (x + w)  y

bevelInset x y w h cr2 cr3 = bevelInset'
  (cvt x) (cvt y) (cvt w) (cvt h) (cvt cr2) (cvt cr3)
  where cvt = CFloat

bevelInset' x y' w h cr2' cr3' bgColor = do
  let y = y' - 0.5
      d = min w h
      cr2 = min cr2' (d / 2)
      cr3 = min cr3' (d / 2)
      bevelColor = offsetColor bgColor bndInsetBevelShade

  withStroke
    (do
      strokeWidth 1
      pain <- linearGradient
          x (y + h - (max cr2 cr3) - 1)
          x (y + h - 1)
          (opaq bgColor)
          bgColor

      strokePaint pain
    )
    (do
      moveTo (x + w) (y + h - cr2)
      arcTo  (x + w) (y + h) x (y + h) cr2
      arcTo   x      (y + h) x  y      cr3
    )

dropShadow x y' w h' r feather alpha = do
  let y = y' + feather
      h = h' - feather

  withFill (do
    pain <- boxGradient
      (x - feather * 0.5) (y - feather * 0.5)
      (w + feather) (h + feather)
      (r + feather * 0.5)
      feather
      (rgba 0 0 0 (alpha ^ 2))
      (rgba 0 0 0 0)

    fillPaint pain
    )
    (do
      moveTo (x - feather)     (y - feather)
      lineTo  x                (y - feather)
      lineTo  x                (y + h - feather)

      arcTo   x                (y + h) (x + r) (y + h) r
      arcTo  (x + h)           (y + h) (x + w) (y + h - r) r

      lineTo (x + w)           (y - feather)
      lineTo (x + w + feather) (y - feather)
      lineTo (x + w + feather) (y + h + feather)
      lineTo (x - feather)     (y + h + feather)

      closePath
    )

innerBox x y w h cr0 cr1 cr2 cr3 shadeTop shadeDown = do
  withFill (do
    pain <- linearGradient
      x y
      (if h - 2 > w then x + w else x)
      (if h - 2 > w then y else y + h)
      shadeTop
      shadeDown

    fillPaint pain
    ) $ 
    roundBox
      (x + 1) (y + 1)
      (w - 2) (h - 3)
      (max 0 (cr0 - 1))
      (max 0 (cr1 - 1))
      (max 0 (cr2 - 1))
      (max 0 (cr3 - 1))

outlineBox x y w h cr0 cr1 cr2 cr3 color = do
  withStrokeColor color
    $ roundBox
      (x + 0.5) (y + 0.5)
      (w - 1) (h - 2)
      cr0 cr1 cr2 cr3


icon :: NanoVG.Image -> Float -> Float -> Icon -> UI ()
icon icons x y icon = do
  let
    (ix, iy) = iconXY icon
    u = fromIntegral (bndIconSheetXOff + ix * bndIconSheetGrid)
    v = fromIntegral (bndIconSheetYOff + iy * bndIconSheetGrid)
    res = bndIconSheetRes
  withFill (do
      pat <- imagePattern
        (x - u)
        (y - v)
        (fromIntegral bndIconSheetWidth)
        (fromIntegral bndIconSheetHeight)
        0 -- rot?
        icons
        1 -- ??
      fillPaint pat
    ) (rect x y (fromIntegral res) (fromIntegral res))

iconLabelValue icons x y w h mIconId color align font fontSize' mLabel mValue = do
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
      textAlign (S.fromList [if align == ALeft then NanoVG.AlignLeft else NanoVG.AlignCenter, NanoVG.AlignBaseline])
      textBox
        (x + fromIntegral pLeft)
        (y + h - fromIntegral bndTextPadDown)
        (w - fromIntegral bndPadRight - fromIntegral pLeft)
        label

    Just value -> do
      let sep = Data.Text.pack bndLabelSep

      labWidth <- textWidthF label
      sepWidth <- textWidthF sep
      valWidth <- textWidthF value

      textAlign (S.fromList [NanoVG.AlignLeft, NanoVG.AlignBaseline])
      xoff <- case align of
        ACenter -> do
          let width = labWidth + sepWidth + valWidth
          return $
              x
            + fromIntegral pLeft 
            + (((w - (fromIntegral $ bndPadRight - pLeft)) - width) * 0.5)
        _ -> return $ x + fromIntegral pLeft

      let yoff = y + h - fromIntegral bndTextPadDown

      text xoff yoff label
      text (xoff + labWidth) yoff sep
      text (xoff + labWidth + sepWidth) yoff value

      --withFillColor black $ do
      --  rect xoff yoff (labWidth) (10)

      --withFillColor white $ do
      --  rect (xoff + labWidth) yoff (sepWidth) (10)

      --withFillColor (trans red) $ do
      --  rect (xoff + labWidth + sepWidth) yoff (valWidth) (-10)

-- XXX
iconLabelCarret = undefined

-- check mark
check x y color = withStroke (do
  strokeWidth 2
  strokeColor color
  lineCap NanoVG.Butt
  lineJoin NanoVG.Miter
  )
  (do
    moveTo (x + 4)  (y + 5)
    lineTo (x + 7)  (y + 8)
    lineTo (x + 14) (y + 1)
  )

arrow x y s color = withFillColor color $ do
  moveTo x y
  lineTo (x - s) (y + s)
  lineTo (x - s) (y - s)
  closePath

upDownArrow x y s color = withFillColor color $ do
  moveTo x (y - 1)
  lineTo (x + 0.5 * w) (y - s - 1)
  lineTo (x + w) (y - 1)
  closePath
  moveTo x (y - 1)
  lineTo (x + 0.5 * w) (y + s + 1)
  lineTo (x + w) (y - 1)
  closePath
  where w = s * 1.1


-- THEMED

toolButton x y w h flags state mIcon mLabel = do
  let [cr0, cr1, cr2, cr3] = selectCorners bndToolRadius flags

  Theme{..} <- theme

  bevelInset x y w h (fromIntegral cr2) (fromIntegral cr3) tBg
  let (i1, i2) = innerColors state tTool True
  -- XXX
  --innerBox   x y w h cr0 cr1 cr2 cr3 i1 i2
  --outlineBox x y w h cr0 cr1 cr2 cr3 (trans (wtOutline tTool))

  let tc = textColor state tTool
  iconLabelValue tIcons x y w h mIcon tc ACenter tFont tFontSize mLabel Nothing

radioButton x y w h flags state mIcon mLabel = do
  let [cr0, cr1, cr2, cr3] = selectCorners bndToolRadius flags

  Theme{..} <- theme

  bevelInset x y w h (fromIntegral cr2) (fromIntegral cr3) tBg
  let (i1, i2) = innerColors state tRadio True
  -- XXX
  --innerBox   x y w h cr0 cr1 cr2 cr3 i1 i2
  --outlineBox x y w h cr0 cr1 cr2 cr3 (trans (wtOutline tRadio))

  let tc = textColor state tRadio
  iconLabelValue tIcons x y w h mIcon tc ACenter tFont tFontSize mLabel Nothing

-- UI

data Size = Fixed | Greedy
  deriving (Eq, Show, Ord)

data Attrs a = Attrs {
    aIcon :: Maybe Icon
  , aLabel :: Maybe Text
  , aValue :: a
  , aHandlers :: [a -> IO ()]
  }

instance (Eq x) => Eq (Attrs x) where
  (==) a b = aValue a == aValue b

instance (Ord x) => Ord (Attrs x) where
  (<=) a b = aValue a <= aValue b

instance (Show x) => Show (Attrs x) where
  show Attrs{..} = show (aIcon, aLabel, aValue)

defA a = Attrs Nothing Nothing a []

ico i x = x { aIcon = Just i }
label i x = x { aLabel = Just i }
value i x = x { aValue = i }
handler i x = x { aHandlers = i:(aHandlers x) }
(&) = flip ($)

--data Elem a = Attrs a 

data Elem =
    HBox [Elem]
  | VBox [Elem]
  | Button (Attrs ()) -- (Maybe Icon) Text
  | Radio (Attrs Bool)
  | Rekt Integer Integer Integer Integer Elem
  | Sized Size Elem
  | Dummy
  | Mutable (TVar Elem)
--  deriving (Eq, Show, Ord)

-- uniplate from Sized to Rekts 
-- h/vbox can set corners to rendering state

data RenderState = RS {
    firstElem :: Bool
  , lastElem :: Bool
  , inHBox :: Bool
  , inVBox :: Bool
  , mousePos :: (Integer, Integer)
  , mouseBtn :: [GLFW.MouseButton]
  , events :: [Elem]
  }
 -- deriving (Eq, Show, Ord)

drawElem (Rekt x y w h b@(Button Attrs{..})) = do
  RS{..} <- get
  let
      focused = let (mx, my) = mousePos in
           mx > x && mx < x + w
        && my > y && my < y + h
      clicked = focused && GLFW.MouseButton'1 `elem` mouseBtn

      state = case (clicked, focused) of
        (True, _) -> ActiveFocus
        (_, True) -> HasFocus
        _         -> NoFocus

  when (state == ActiveFocus) $ do
    modify (\x -> x { events = [b] })

  let corners = case inHBox of
        False -> []
        True -> case (firstElem, lastElem) of
          (True, _) -> [TopRight, DownRight]
          (_, True) -> [TopLeft, DownLeft]
          _         -> [TopRight, DownRight, TopLeft, DownLeft]

  lift $ toolButton
    (fromIntegral x) (fromIntegral y)
    (fromIntegral w) (fromIntegral h)
    corners
    state
    aIcon
    (Data.Text.unpack <$> aLabel)

drawElem (HBox es) = do
  modify (\x -> x { inHBox = True })
  --mapM_ drawElem es
  orderTrackingMapM_ drawElem es
  modify (\x -> x { inHBox = False })

orderTrackingMapM_ fn [] = return ()
orderTrackingMapM_ fn (x:xs) = do
  modify (\x -> x { firstElem = True })
  fn x
  modify (\x -> x { firstElem = False })
  mapM_ fn (Data.List.init xs)
  when (length xs >= 1) $ do
    modify (\x -> x { lastElem = True })
    fn $ last xs
    modify (\x -> x { lastElem = False })

{--
cubeMesh :: LC.Mesh
cubeMesh = Mesh
  { mAttributes   = M.fromList
      [ ("position",  A_V3F $ V.fromList vertecies)
      , ("uv",        A_V2F $ V.fromList uvs)
      ]
  , mPrimitive    = P_Triangles
  }
  where
  vertecies = [
      v3, v2, v1, v3, v1, v0
    , v4, v7, v6, v4, v6, v5
    , v0, v1, v7, v0, v7, v4
    , v5, v6, v2, v5, v2, v3
    , v2, v6, v7, v2, v7, v1
    , v5, v3, v0, v5, v0, v4
    ]
  uvs = concat $ replicate 6 [u1, u2, u3, u1, u3, u0]

  v0 = LC.V3 (-1) (-1) (-1)
  v1 = LC.V3 (-1)   1  (-1)
  v2 = LC.V3   1    1  (-1)
  v3 = LC.V3   1  (-1) (-1)
  v4 = LC.V3 (-1) (-1)   1
  v5 = LC.V3   1  (-1)   1
  v6 = LC.V3   1    1    1
  v7 = LC.V3 (-1)   1    1

  u0 = LC.V2 0 0
  u1 = LC.V2 1 0
  u2 = LC.V2 1 1
  u3 = LC.V2 0 1
--}

initWindow :: String -> Int -> Int -> IO Window
initWindow title width height = do
    GLFW.init

    GLFW.defaultWindowHints

    mapM_ GLFW.windowHint
      [ WindowHint'ContextVersionMajor 3
      , WindowHint'ContextVersionMinor 3
      , WindowHint'OpenGLProfile OpenGLProfile'Core
      , WindowHint'OpenGLForwardCompat True
      , WindowHint'Samples $ Just 16 -- MSAA 16
      , WindowHint'Resizable True
      , WindowHint'Decorated False
      , WindowHint'DoubleBuffer True
      ]

    -- not needed
    --glEnable GL_MULTISAMPLE

    Just win <- GLFW.createWindow width height title Nothing Nothing
    GLFW.makeContextCurrent $ Just win
    glewInit

    return win

{--
data Widget wt = Widget {
    wLocation :: (Integer, Integer)
  , wSize :: (Integer, Integer)
  , wRender :: wt -> UI ()
  }

data WT = Button | Radio

render :: Widget Radio -> IO

--instance Renderable Bool where
--  renderType = Radio

--}

class Render a where
  render :: a -> Elem
  --type Renderable a :: * -> *

instance Render () where
  render x = Button (defA x)

instance Render Bool where
  render b = Radio (defA b)
  --type Renderable Bool = Elem
