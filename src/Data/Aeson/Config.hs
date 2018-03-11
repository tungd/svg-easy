module Data.Aeson.Config where

import Data.Aeson
import RIO
import RIO.Char


dropLabelPrefixOptions :: Options
dropLabelPrefixOptions = defaultOptions { fieldLabelModifier = dropLabelPrefix }

dropLabelPrefix :: String -> String
dropLabelPrefix [] = []
dropLabelPrefix (x:xs)
  | isUpper x = toLower x : xs
  | otherwise = dropLabelPrefix xs
