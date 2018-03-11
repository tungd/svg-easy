module Data.Aeson.Config where

import Data.Aeson
import RIO
import RIO.Char


dropPrefixOptions :: Options
dropPrefixOptions = defaultOptions { fieldLabelModifier = dropPrefix }

dropPrefix :: String -> String
dropPrefix [] = []
dropPrefix (x:xs)
  | isUpper x = toLower x : xs
  | otherwise = dropPrefix xs
