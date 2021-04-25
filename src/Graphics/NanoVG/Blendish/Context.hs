
module Graphics.NanoVG.Blendish.Context where

import Linear (V2(..))
import NanoVG (Context)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Monad.IO.Class (liftIO)

import Graphics.NanoVG.Blendish.Theme

data DrawContext = DrawContext {
    ctxNvg   :: Context
  , ctxTheme :: Theme
  , ctxMouse :: V2 Double
  }

type Draw = ReaderT DrawContext IO

theme :: Draw Theme
theme = ctxTheme <$> ask

mouse :: Draw (V2 Double)
mouse = ctxMouse <$> ask

withCtx :: (Context -> IO a) -> Draw a
withCtx op = ask >>= liftIO . op . ctxNvg

-- | Run draw monad
runDraw :: DrawContext -> Draw a -> IO a
runDraw = flip runReaderT
