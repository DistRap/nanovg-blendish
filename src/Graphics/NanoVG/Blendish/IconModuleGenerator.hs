
module Graphics.NanoVG.Blendish.IconModuleGenerator where

import Control.Monad
import Data.List
import Data.Char (toLower, toUpper, ord, chr)

-- | Used to generate Graphics.NanoVG.Blendish.Icon
genDefs :: String -> IO ()
genDefs blenderGitPath = do
  -- worked on blender rev bb89cc51eca305195e530f6bc91433aea21e235e
  let f = blenderGitPath ++ "/source/blender/editors/include/UI_icons.h"
  defs <- filter (\x -> "DEF_ICON" `isPrefixOf` x) . lines <$> readFile f
  let uvs = map (\(d, i) -> (d, i `mod` 26, 29 - i `div` 26, isB d)) $ zip defs [0..]
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
