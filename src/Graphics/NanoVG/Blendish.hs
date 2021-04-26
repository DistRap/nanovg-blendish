{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.NanoVG.Blendish (
    blendish
  , blendishCfg
  , BlendishConfig(..)
  , main
  , simple
  , initWindow
  , loadData
  , module Graphics.NanoVG.Blendish.Icon
  , module Graphics.NanoVG.Blendish.Context
  , module Graphics.NanoVG.Blendish.Types
  , module Graphics.NanoVG.Blendish.Monad
  , module Graphics.NanoVG.Blendish.Monad.Primitives
  , module Graphics.NanoVG.Blendish.Theme
  , module Graphics.NanoVG.Blendish.Utils
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Bits ((.|.))
import Data.Default
-- TODO mouse buttons
import Foreign.C.Types
import Graphics.GL.Core33
import Graphics.UI.GLFW (Window, WindowHint(..), OpenGLProfile(..), Key(..), MouseButton(..))
import Linear (V2(..))
import NanoVG (Font, CreateFlags(..), Context, Image)

import qualified Data.Bool
import qualified Data.Set
import qualified Graphics.UI.GLFW as GLFW
import qualified NanoVG
import qualified Data.Text

import Graphics.NanoVG.Blendish.Icon
import Graphics.NanoVG.Blendish.Context
import Graphics.NanoVG.Blendish.Demo
import Graphics.NanoVG.Blendish.Types
import Graphics.NanoVG.Blendish.Monad
import Graphics.NanoVG.Blendish.Monad.Primitives
import Graphics.NanoVG.Blendish.Theme
import Graphics.NanoVG.Blendish.Utils

import Paths_nanovg_blendish

foreign import ccall unsafe "initGlew"
  glewInit :: IO CInt

data BlendishConfig = BlendishConfig {
    configNanovgAntialias :: Bool -- ^ Use NanoVGs built-in anti-aliasing
  , configNanovgDebug :: Bool -- ^ Enable NanoVG debug output
  , configScaling :: Float -- ^ Zoom factor
  } deriving Show

instance Default BlendishConfig where
  def = BlendishConfig {
          configNanovgAntialias = False
        , configNanovgDebug = False
        , configScaling = 1
        }

data UIData = UIData {
    uiDataSansFont :: Font
  , uiDataIcons :: Image
  }

-- | Initialize blendish (and its NanoVG context), wrapping
-- our `a -> Draw ()` function into `a -> IO ()` variant
-- which is then called from application main loop.
blendish
  :: Window
  -> (a -> Draw ())
  -> IO (a -> IO ())
blendish = blendishCfg def

-- | Principled variant of @blendish@, accepting @BlendishConfig@
blendishCfg
  :: BlendishConfig
  -> Window
  -> (a -> Draw ())
  -> IO (a -> IO ())
blendishCfg BlendishConfig{..} win drawAct = do
    nanovgContext<- NanoVG.createGL3
      $ Data.Set.fromList
      $ [StencilStrokes]
        ++ Data.Bool.bool [] (pure Antialias) configNanovgAntialias
        ++ Data.Bool.bool [] (pure Debug) configNanovgDebug

    NanoVG.fontFace nanovgContext "sans"
    NanoVG.globalAlpha nanovgContext 1

    mdata <- runMaybeT $ loadData nanovgContext
    uiData <- case mdata of
      Nothing -> error "Unable to load data"
      Just x -> return x

    let renderAct = \drawData -> do
          render nanovgContext (drawAct drawData) uiData win configScaling
    return renderAct


-- | Actual rendering function
render
  :: Context
  -> Draw ()
  -> UIData
  -> Window
  -> Float
  -> IO ()
render nanovgCtx drawAct uiData win zoom = do

  (winW, winH) <- GLFW.getWindowSize win
  (mx, my) <- GLFW.getCursorPos win
  (fbW, _fbH) <- GLFW.getFramebufferSize win
  let pxRatio = fromIntegral fbW / fromIntegral winW

  NanoVG.beginFrame nanovgCtx (fromIntegral winW / zoom) (fromIntegral winH / zoom) (pxRatio * zoom)

  runDraw
    (DrawContext
      nanovgCtx
      (defTheme (uiDataIcons uiData))
      (V2 mx my))
    drawAct

  NanoVG.endFrame nanovgCtx

-- | Demo rendering example
main :: IO ()
main = do
    win <- initWindow "nanovg-blendish" 1920 1080

    -- init blendish
    renderUI <- blendish win demoUI

    let loop = do
                -- update graphics input
                fbSize@(fbW, fbH) <- GLFW.getFramebufferSize win

                glViewport 0 0 (fromIntegral fbW) (fromIntegral fbH)
                glClearColor 0.3 0.3 0.32 1.0
                -- at least GL_DEPTH_BUFFER_BIT and GL_STENCIL_BUFFER_BIT are required
                glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT)

                -- call the rendering function with fbsize input
                renderUI fbSize

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

-- | Init GLFW Window and enable glew
initWindow :: String -> Int -> Int -> IO GLFW.Window
initWindow title width height = do
    void GLFW.init

    GLFW.defaultWindowHints

    mapM_ GLFW.windowHint
      [ WindowHint'ContextVersionMajor 3
      , WindowHint'ContextVersionMinor 3
      , WindowHint'OpenGLProfile OpenGLProfile'Core
      , WindowHint'OpenGLForwardCompat True
      -- It is possible to draw w/o GL AA
      -- relying on NanoVG AA (configNanovgAntialias = True)
      -- If NanoVG AA is enabled, this can't be enabled
      -- or you need to use another GL buffer where AA is disabled.
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

-- | Load default data files bundled with nanovg-blendish
loadData
  :: Context
  -> MaybeT IO UIData
loadData c = do
  (fontFile, iconsFile) <- liftIO $
    (,) <$> getDataFileName "DejaVuSans.ttf"
        <*> getDataFileName "blender_icons16.png"
  sans  <- MaybeT $ NanoVG.createFont c "sans" (NanoVG.FileName $ Data.Text.pack fontFile)
  icons <- MaybeT $ NanoVG.createImage c (NanoVG.FileName $ Data.Text.pack iconsFile) 0
  pure $ UIData {
            uiDataSansFont = sans
          , uiDataIcons = icons
          }

-- | Render @Draw ()@, shorthand for GHCI testing
-- > simple $ toolButton (V2 100 100) (V2 200 bndWidgetHeight) (pure True) HasFocus (Just Icon'Speaker) (Just "Simple")
simple :: Draw () -> IO ()
simple drawAct = do
    win <- initWindow "nanovg-blendish-simple" 1920 1080

    -- init blendish
    renderUI <- blendish win (const drawAct)

    let loop = do
                (fbW, fbH) <- GLFW.getFramebufferSize win
                glViewport 0 0 (fromIntegral fbW) (fromIntegral fbH)
                glClearColor 0.3 0.3 0.32 1.0
                -- at least GL_DEPTH_BUFFER_BIT and GL_STENCIL_BUFFER_BIT are required
                glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT)

                -- call the rendering function with fbsize input
                renderUI ()

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
