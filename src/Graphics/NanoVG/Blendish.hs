{-# LANGUAGE PackageImports, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- net
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.NanoVG.Blendish where

import Graphics.UI.GLFW (WindowHint(..), OpenGLProfile(..), Key(..), MouseButton(..))
import qualified Graphics.UI.GLFW as GLFW

import Data.Function ((&))
-- 2D
import qualified Data.Bits
import qualified Data.Set as S
import qualified Data.List


import NanoVG (Font, CreateFlags(..))
import qualified NanoVG
import           Foreign.C.Types

import Data.Text (Text)
import qualified Data.Text

-- loadDat
import           Control.Monad
--import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Maybe

import Control.Concurrent.STM hiding (check)-- .TVar 

-- ours
import Graphics.NanoVG.Blendish.Icon
--import Graphics.NanoVG.Blendish.IconModuleGenerator
import Graphics.NanoVG.Blendish.Context
import Graphics.NanoVG.Blendish.Monad
import Graphics.NanoVG.Blendish.Shorthand
import Graphics.NanoVG.Blendish.Types
import Graphics.NanoVG.Blendish.Theme
import Graphics.NanoVG.Blendish.Utils

import Graphics.GL.Core33
import Linear

foreign import ccall unsafe "initGlew"
  glewInit :: IO CInt

main :: IO ()
main = do
    -- load image and upload texture
    --Right img <- Juicy.readImage "logo.png"
    --
    win <- initWindow "nanovg-blendish" 1920 1080
    c@(NanoVG.Context _c') <- NanoVG.createGL3 (S.fromList [Antialias,StencilStrokes,Debug])
    --fbo' <- alloca (\ptr -> glGenFramebuffers 1 ptr >> peek ptr)

    mdata <- runMaybeT $ loadData c
    da <- case mdata of
      Nothing -> error "Unable to load data"
      Just x -> return x


    --GLFW.setMouseButtonCallback win . pure $
    --

    let loop = do
                -- update graphics input
                (w,h) <- GLFW.getWindowSize win >>= \(w,h) -> do
                  return (w, h)

                Just _t <- GLFW.getTime
                (mx, my) <- GLFW.getCursorPos win
                (fbW, fbH) <- GLFW.getFramebufferSize win
                let pxRatio = fromIntegral fbW / fromIntegral w

                --glViewport 0 0 (fromIntegral fbW) (fromIntegral fbH)
                -- 3d

                -- NANO
                glViewport 0 0 (fromIntegral fbW) (fromIntegral fbH)

                --glViewport 0 0 (fromIntegral w) (fromIntegral h)
                --glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT)
                --glClear (GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT)
                --beginFrame c (fromIntegral w) (fromIntegral h) pxRatio
                --glClear (GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT)

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

                let keyIsPressed k = fmap (==GLFW.KeyState'Pressed) $ GLFW.getKey win k
                escape <- keyIsPressed GLFW.Key'Escape
                q <- keyIsPressed Key'Q
                shouldClose <- GLFW.windowShouldClose win
                if (escape || q || shouldClose) then return () else loop

    loop
    GLFW.destroyWindow win
    GLFW.terminate


data NData = NData Font NanoVG.Image


loadData :: NanoVG.Context -> MaybeT IO NData
loadData c = do
  --normal <- MaybeT $ NanoVG.createFont c "sans" (NanoVG.FileName "Roboto-Regular.ttf")
  sans <- MaybeT $ NanoVG.createFont c "sans" (NanoVG.FileName "DejaVuSans.ttf")
  icons <- MaybeT $ NanoVG.createImage c (NanoVG.FileName "blender_icons16.png") 0
  pure (NData sans icons)

renderUI :: NanoVG.Context -> NData -> Double -> Double -> [MouseButton] -> IO ()
renderUI ctx (NData _ icons) x y mb = do
  --bg <- linearGradient c x y x (y+h) (rgba 0 16 192 0) (rgba 0 160 192 64)
  --NanoVG.beginPath c
  --NanoVG.rect c 10 10 100 300
  --NanoVG.fillColor c (rgba 255 255 255 32)
  --NanoVG.fill c

  NanoVG.fontSize ctx 148
  NanoVG.fontFace ctx "sans"

  NanoVG.globalAlpha ctx 0.9

  _w <- flip runReaderT (UIContext ctx (defTheme icons) (x, y)) $ do
    withFillColor (rgba 0 0 0 100) $ withCtx $ \c ->
      NanoVG.text c 20 250 "Lambdacube nanovg !!"

    dropShadow (V2 10 10) (V2 100 100) 10 10 10
    withFillColor gray $ roundBox (V2 10 10) (V2 100 100) 10 10 10 10

    background (V2 100 10) (V2 100 300) (rgba 255 255 255 32)

    check (V2 100 50) black
    arrow (V2 100 60) 10 black
    upDownArrow (V2 50 60) 10 black

    let sp = bndWidgetHeight + 5

    Theme{..} <- theme
    background (V2 0 0) (V2 1024 (100 * sp)) tBg -- (rgbaf 0.9 0.9 0.9 0.9) -- tBg
    toolButton (V2 100 10) (V2 200 21)
      [] NoFocus (Just Icon'Particles) (Just "Tool button")

    toolButton (V2 100 (10 + sp)) (V2 200 bndWidgetHeight)
      [] HasFocus (Just Icon'Speaker) (Just "Focus button")

    toolButton (V2 100 (10 + (2*sp))) (V2 200 bndWidgetHeight)
      [] ActiveFocus (Just Icon'Physics) (Just "Active button")

    radioButton (V2 100 (10 + (3*sp))) (V2 200 bndWidgetHeight)
      [] NoFocus (Just Icon'9_AA) (Just "Radio button")

  let b = Button (ico Icon'Speaker $ label "Up" $ defA ()) -- (Just Icon'Speaker) "Count up"
      c = Button (defA () & ico Icon'Dot & label "Down") -- (Just Icon'Dot) "Count down"


  _zz <- newTVarIO $ HBox [
           Rekt 30 100 100 21 b
         , Rekt 127 100 100 21 c
         ]


  -- bricklike
  -- State -> Event -> State
  -- State -> UI

  _w <- flip runReaderT (UIContext ctx (defTheme icons) (x, y)) 
    $ flip runStateT (defRS x y mb) $ do
       drawElem $
         HBox [
           Rekt 30 100 100 21 b
         , Rekt 127 100 100 21 c
         ]
  --print (show $ map fst $ hs b, show $ events $ snd w)

  return ()

-- mouz
defRS :: (RealFrac a1, RealFrac a2) => a1 -> a2 -> [MouseButton] -> RenderState
defRS x y mb = RS False False False False (round x, round y) mb []
--  (GLFW.MouseButton'1) -- ,MouseButtonState'Pressed) -- ,ModifierKeys {modifierKeysShift = False, modifierKeysControl = False, modifierKeysAlt = False, modifierKeysSuper = False, modifierKeysCapsLock = False, modifierKeysNumLock = True})
--  ] []


-- 0 0 -> A1 (topleft)
-- 1 0 -> A2 (topleft -> this)
iconIdFromXY :: Integer -> Integer -> Integer
iconIdFromXY x y = x + y `Data.Bits.shiftL` 8

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

defA :: a -> Attrs a
defA a = Attrs Nothing Nothing a []

ico :: Icon -> Attrs a -> Attrs a
ico i x = x { aIcon = Just i }

label :: Text -> Attrs a -> Attrs a
label i x = x { aLabel = Just i }

value :: a -> Attrs a -> Attrs a
value i x = x { aValue = i }

handler :: (a -> IO ()) -> Attrs a -> Attrs a
handler i x = x { aHandlers = i:(aHandlers x) }

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

drawElem :: Elem -> StateT RenderState (ReaderT UIContext IO) ()
drawElem (Rekt x y w h b@(Button Attrs{..})) = do
  RS{..} <- get
  let
      focused = let (mx, my) = mousePos in
           mx > x && mx < x + w
        && my > y && my < y + h
      clicked = focused && GLFW.MouseButton'1 `elem` mouseBtn

      state' = case (clicked, focused) of
        (True, _) -> ActiveFocus
        (_, True) -> HasFocus
        _         -> NoFocus

  when (state' == ActiveFocus) $ do
    modify (\x' -> x' { events = [b] })

  let corners = case inHBox of
        False -> []
        True -> case (firstElem, lastElem) of
          (True, _) -> [TopRight, DownRight]
          (_, True) -> [TopLeft, DownLeft]
          _         -> [TopRight, DownRight, TopLeft, DownLeft]

  lift $ toolButton
    (V2 (fromIntegral x) (fromIntegral y))
    (V2 (fromIntegral w) (fromIntegral h))
    corners
    state'
    aIcon
    (Data.Text.unpack <$> aLabel)

drawElem (HBox es) = do
  modify (\x -> x { inHBox = True })
  --mapM_ drawElem es
  orderTrackingMapM_ drawElem es
  modify (\x -> x { inHBox = False })
drawElem _ = error "Don't know how to render"

orderTrackingMapM_ :: Monad m => (a1 -> StateT RenderState m a2) -> [a1] -> StateT RenderState m ()
orderTrackingMapM_ _fn [] = return ()
orderTrackingMapM_ fn (a:xs) = do
  modify (\x -> x { firstElem = True })
  void $ fn a
  modify (\x -> x { firstElem = False })
  mapM_ fn (Data.List.init xs)
  when (length xs >= 1) $ do
    modify (\x -> x { lastElem = True })
    void $ fn $ last xs
    modify (\x -> x { lastElem = False })

initWindow :: String -> Int -> Int -> IO GLFW.Window
initWindow title width height = do
    void $ GLFW.init

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

    mWin <- GLFW.createWindow width height title Nothing Nothing
    case mWin of
      Nothing -> error "Can't createWindow"
      Just win -> do
        GLFW.makeContextCurrent $ Just win
        void $ glewInit
        return win
