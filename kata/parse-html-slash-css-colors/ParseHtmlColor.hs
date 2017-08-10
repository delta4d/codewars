module Codewars.Lambda4fun.ParseHtmlColor where

import Codewars.Lambda4fun.ParseHtmlColor.PresetColors (presetColors)
import Data.Map.Strict (Map, fromList, (!))
import Numeric (readHex)
import Data.Char (toLower)

toHex :: String -> Int
toHex = fst . head . readHex

parseHtmlColor :: String -> Map Char Int
parseHtmlColor ['#',r,g,b] = parseHtmlColor ['#',r,r,g,g,b,b]
parseHtmlColor ['#',r1,r2,g1,g2,b1,b2] = fromList $ zip "rgb" $ map toHex [[r1,r2], [g1,g2], [b1,b2]]
parseHtmlColor name = parseHtmlColor (presetColors ! map toLower name)
