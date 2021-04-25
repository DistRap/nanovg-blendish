{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.NanoVG.Blendish where

import Graphics.UI.GLFW (WindowHint(..), OpenGLProfile(..), Key(..), MouseButton(..))
import qualified Graphics.UI.GLFW as GLFW

import Data.Bits ((.|.))
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
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Maybe

import Control.Concurrent.STM hiding (check)-- .TVar 

-- ours
import Graphics.NanoVG.Blendish.Icon
import Graphics.NanoVG.Blendish.Context
import Graphics.NanoVG.Blendish.Demo
import Graphics.NanoVG.Blendish.Types
import Graphics.NanoVG.Blendish.Monad
import Graphics.NanoVG.Blendish.Theme (defTheme)
import Graphics.NanoVG.Blendish.Utils
import Paths_nanovg_blendish

import Graphics.GL.Core33
import Linear

foreign import ccall unsafe "initGlew"
  glewInit :: IO CInt

main :: IO ()
main = do
    win <- initWindow "nanovg-blendish" 1920 1080
    --c@(NanoVG.Context _c') <- NanoVG.createGL3 (S.fromList [Antialias,StencilStrokes,Debug])
    c@(NanoVG.Context _c') <- NanoVG.createGL3 (S.fromList [StencilStrokes,Debug])

    mdata <- runMaybeT $ loadData c
    da <- case mdata of
      Nothing -> error "Unable to load data"
      Just x -> return x

    let loop = do
                -- update graphics input
                (winW,winH) <- GLFW.getWindowSize win >>= \(w,h) -> do
                  return (w, h)

                Just _t <- GLFW.getTime
                (mx, my) <- GLFW.getCursorPos win
                (fbW, fbH) <- GLFW.getFramebufferSize win
                let pxRatio = fromIntegral fbW / fromIntegral winW

                -- NANO
                glViewport 0 0 (fromIntegral fbW) (fromIntegral fbH)
                glClearColor 0.3 0.3 0.32 1.0
                glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT)

                mb1 <- GLFW.getMouseButton win MouseButton'1
                let mb = case mb1 of
                            GLFW.MouseButtonState'Pressed -> [ MouseButton'1 ]
                            _ -> []

                NanoVG.beginFrame c (fromIntegral winW) (fromIntegral winH) (pxRatio * 1.0)
                renderUI c da mx my mb
                NanoVG.endFrame c

                GLFW.swapBuffers win
                GLFW.pollEvents

                let keyIsPressed k = (==GLFW.KeyState'Pressed) <$> GLFW.getKey win k
                escape <- keyIsPressed GLFW.Key'Escape
                q <- keyIsPressed Key'Q
                shouldClose <- GLFW.windowShouldClose win
                unless (escape || q || shouldClose) loop

    loop
    GLFW.destroyWindow win
    GLFW.terminate


data UIData = UIData Font NanoVG.Image

loadData :: NanoVG.Context -> MaybeT IO UIData
loadData c = do
  (fontFile, iconsFile) <- liftIO $
    (,) <$> getDataFileName "DejaVuSans.ttf"
        <*> getDataFileName "blender_icons16.png"
  sans  <- MaybeT $ NanoVG.createFont c "sans" (NanoVG.FileName $ Data.Text.pack fontFile)
  icons <- MaybeT $ NanoVG.createImage c (NanoVG.FileName $ Data.Text.pack iconsFile) 0
  pure (UIData sans icons)

renderUI :: NanoVG.Context -> UIData -> Double -> Double -> [MouseButton] -> IO ()
renderUI ctx (UIData _ icons) x y _mouseButtons = do
  NanoVG.fontSize ctx 18
  NanoVG.fontFace ctx "sans"

  NanoVG.globalAlpha ctx 1

  _w <- flip runReaderT (UIContext ctx (defTheme icons) (x, y)) $ do
    demoUI
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

label' :: Text -> Attrs a -> Attrs a
label' i x = x { aLabel = Just i }

value :: a -> Attrs a -> Attrs a
value i x = x { aValue = i }

handler :: (a -> IO ()) -> Attrs a -> Attrs a
handler i x = x { aHandlers = i : aHandlers x }

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

  let corners = if inHBox then
       (case  (firstElem, lastElem) of
          (True, _) -> (pure True) { topRight = False, downRight = False }
          (_, True) -> (pure True) { topLeft = False, downLeft = False }
          _         -> pure False
       ) else pure False

  lift $ toolButton
    (V2 (fromIntegral x) (fromIntegral y))
    (V2 (fromIntegral w) (fromIntegral h))
    corners
    state'
    aIcon
    aLabel

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
  unless (null xs) $ do
    modify (\x -> x { lastElem = True })
    void $ fn $ last xs
    modify (\x -> x { lastElem = False })

initWindow :: String -> Int -> Int -> IO GLFW.Window
initWindow title width height = do
    void GLFW.init

    GLFW.defaultWindowHints

    mapM_ GLFW.windowHint
      [ WindowHint'ContextVersionMajor 3
      , WindowHint'ContextVersionMinor 3
      , WindowHint'OpenGLProfile OpenGLProfile'Core
      , WindowHint'OpenGLForwardCompat True
      , WindowHint'Samples $ Just 4 -- MSAA 4
      , WindowHint'Resizable True
      , WindowHint'Decorated False
      , WindowHint'DoubleBuffer True
      ]

    mWin <- GLFW.createWindow width height title Nothing Nothing
    case mWin of
      Nothing -> error "Can't createWindow"
      Just win -> do
        GLFW.makeContextCurrent $ Just win
        void glewInit
        return win
