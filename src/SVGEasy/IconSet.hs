module SVGEasy.IconSet
  ( IconSet(..)
  , findIconSets
  ) where

import Data.Aeson
import Data.List
import RIO
import RIO.Directory
import System.FilePath

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
  toJSON = genericToJSON dropPrefixOptions

instance FromJSON IconSet where
  parseJSON = genericParseJSON dropPrefixOptions


findIconSets
  :: (HasLogFunc env)
  => FilePath -> RIO env [IconSet]
findIconSets fp = do
  fps <- liftIO $ getDirectoryContents fp
  is <- foldM (loadIconSet' fp) [] fps
  pure is

loadIconSet'
  :: (HasLogFunc env)
  => FilePath -> [IconSet] -> FilePath -> RIO env [IconSet]
loadIconSet' root iss fp
  | "." `isPrefixOf` fp = pure iss
  | otherwise = do
      is :: Maybe IconSet <- decode <$> liftIO (BL.readFile definition)
      case is of
        Nothing -> pure iss
        Just is' ->
          pure (is' : iss)
  where
    definition = root </> fp </> "set.json"
