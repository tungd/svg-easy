module SVGEase.IconSet
  ( IconSet(..)
  ) where

import RIO
import Data.Aeson

import Data.Aeson.Config


data IconSet = IconSet
  { isName        :: Text
  , isVersion     :: Text
  , isDescription :: Text
  , isLicense     :: Text
  , isHomepage    :: Text
  , isIconList    :: [Text]
  } deriving (Show, Generic)

instance ToJSON IconSet where
  toJSON = genericToJSON dropPrefixOptions

instance FromJSON IconSet where
  parseJSON = genericParseJSON dropPrefixOptions
