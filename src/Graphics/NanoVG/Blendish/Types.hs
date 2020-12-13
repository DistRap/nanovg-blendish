
module Graphics.NanoVG.Blendish.Types where

data WidgetFocus = NoFocus | HasFocus | ActiveFocus
  deriving (Eq, Show, Ord, Enum)

data SharpCorner = TopLeft | TopRight | DownRight | DownLeft
  deriving (Eq, Show, Ord, Enum)

cornerOrdering :: [SharpCorner]
cornerOrdering = [TopLeft, TopRight, DownRight, DownLeft]

selectCorners :: Float -> [SharpCorner] -> [Float]
selectCorners r flags = map zeroWhenFlag cornerOrdering
  where zeroWhenFlag x = if x `elem` flags then 0 else r


