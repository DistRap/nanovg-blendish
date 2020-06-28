
module Graphics.NanoVG.Blendish.Types where

import NanoVG (Context)
import qualified NanoVG


data WidgetFocus = NoFocus | HasFocus | ActiveFocus
  deriving (Eq, Show, Ord, Enum)

data SharpCorner = TopLeft | TopRight | DownRight | DownLeft
  deriving (Eq, Show, Ord, Enum)

cornerOrdering = [TopLeft, TopRight, DownRight, DownLeft]

selectCorners :: Num a => a -> [SharpCorner] -> [a]
selectCorners r flags = map zeroWhenFlag cornerOrdering
  where zeroWhenFlag x = if x `elem` flags then 0 else r


