
module Graphics.NanoVG.Blendish.Context where

import NanoVG (Context)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad.IO.Class (liftIO)

import Graphics.NanoVG.Blendish.Theme

data UIContext = UIContext {
    ctxNvg   :: Context
  , ctxTheme :: Theme
  , ctxMouse :: (Double, Double)
  }

type Draw = ReaderT UIContext IO

theme :: Draw Theme
theme = ask >>= return . ctxTheme

withCtx :: (Context -> IO a) -> Draw a
withCtx op = ask >>= liftIO . op . ctxNvg
