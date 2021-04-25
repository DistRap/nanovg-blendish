{-# LANGUAGE DeriveFunctor #-}

module Graphics.NanoVG.Blendish.Types where

data WidgetFocus = NoFocus | HasFocus | ActiveFocus
  deriving (Eq, Show, Ord, Enum)

-- | Corner rounding
-- @pure True :: Corners Bool@ means all corners are rounded
--
-- @Corners Float@ is used to represent corner radius for each corner
data Corners a = Corners
  { topLeft   :: a
  , topRight  :: a
  , downRight :: a
  , downLeft  :: a
  }
  deriving (Eq, Show, Ord, Functor)

instance Applicative Corners where
  pure x = Corners x x x x
  Corners a b c d <*> Corners e f g h = Corners (a e) (b f) (c g) (d h)

selectCorners :: Float -> Corners Bool -> Corners Float
selectCorners r = fmap (\x -> if x then r else 0)

downCorners :: Corners a -> (a, a)
downCorners c = (downRight c, downLeft c)
