module SVGEasy.IconSetBuild where

import Data.Aeson
import Graphics.Svg hiding (Text)
import RIO
import RIO.FilePath
import RIO.Map
import Web.FormUrlEncoded

import qualified RIO.Text as T

import Data.Aeson.Config
import SVGEasy.IconSet


data IconSpec = IconSpec
  { specName :: Text
  , specIconset :: Text
  } deriving (Show, Generic)

instance FromJSON IconSpec where
  parseJSON = genericParseJSON dropLabelPrefixOptions

instance ToJSON IconSpec where
  toJSON = genericToJSON dropLabelPrefixOptions

type IconSetBuildSpec = Map Text IconSpec

instance FromForm IconSetBuildSpec where
  fromForm f = case parseUnique "spec" f of
    Left err   -> Left err
    Right spec -> mapLeft fromString (eitherDecode $ fromString spec)

buildSprite
  :: (HasLogFunc env)
  => FilePath -> IconSetBuildSpec -> RIO env Document
buildSprite _ spec = do
  (icons, _) <- foldM loadIcon ([], []) (svgFiles <$> assocs spec)
  pure $ emptyDocument & set elements icons

svgFiles :: (Text, IconSpec) -> FilePath
svgFiles (k, IconSpec{..}) =
  "resources" </> T.unpack specIconset </> T.unpack k <.> "svg"
