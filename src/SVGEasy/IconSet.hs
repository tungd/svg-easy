module SVGEasy.IconSet
  ( IconSet(..)
  , findIconSets
  , iconName
  ) where

import Data.Aeson
import RIO
import RIO.FilePath
import RIO.List
import System.FilePath.Find hiding (fold)

import qualified System.FilePath.Find as FP
import qualified RIO.ByteString.Lazy as BL

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
  toJSON = genericToJSON dropLabelPrefixOptions

instance FromJSON IconSet where
  parseJSON = genericParseJSON dropLabelPrefixOptions


findIconSets
  :: (HasLogFunc env)
  => FilePath -> RIO env [IconSet]
findIconSets fp = do
  iss <- liftIO $ FP.find always (filePath ~~? "**/set.json") fp
  foldM readIconSet [] iss

readIconSet
  :: (HasLogFunc env)
  => [IconSet] -> FilePath -> RIO env [IconSet]
readIconSet iss fp = decode <$> liftIO (BL.readFile fp) >>= \case
  Nothing -> pure iss
  Just is' -> do
    icons <- liftIO $ FP.find always (extension ==? ".svg") root
    pure (is' { isIconList = iconName root <$> icons } : iss)
  where
    root = takeDirectory fp

iconName :: FilePath -> FilePath -> Text
iconName root fp = fromString
  . intercalate "-" . splitDirectories
  . dropExtension
  $ dropPrefix (root <> "/") fp
