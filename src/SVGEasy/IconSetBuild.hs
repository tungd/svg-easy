module SVGEasy.IconSetBuild where

import Data.Aeson
import Graphics.Svg hiding (Text)
import RIO
import Web.FormUrlEncoded

import Data.Aeson.Config


data IconSpec = IconSpec
  { specName :: Text
  , specIconset :: Text
  } deriving (Show, Generic)

instance FromJSON IconSpec where
  parseJSON = genericParseJSON dropLabelPrefixOptions

type IconSetBuildSpec = Map Text IconSpec

instance FromForm IconSetBuildSpec where
  fromForm f = case parseUnique "spec" f of
    Left err   -> Left err
    Right spec -> mapLeft fromString (eitherDecode $ fromString spec)

buildSprite
  :: (HasLogFunc env)
  => FilePath -> IconSetBuildSpec -> RIO env Document
buildSprite = undefined

buildPreview
  :: (HasLogFunc env)
  => Document -> RIO env Text
buildPreview = undefined
