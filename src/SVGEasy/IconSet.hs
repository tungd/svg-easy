module SVGEasy.IconSet
  ( IconSet(..)
  , IconSetMeta(..)
  , Icon(..)
  , loadIconSet
  ) where

import Data.Aeson
import Graphics.Svg hiding (Text)
import RIO
import RIO.FilePath
import System.FilePath.Find

import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Map as M
import qualified System.FilePath.Find as FP

import Data.Aeson.Config


data IconSetMeta = IconSetMeta
  { isId          :: Text
  , isName        :: Text
  , isVersion     :: Text
  , isDescription :: Text
  , isLicense     :: Text
  , isHomepage    :: Text
  , isIconList    :: [Icon]
  } deriving (Show, Generic)

instance ToJSON IconSetMeta where
  toJSON = genericToJSON dropLabelPrefixOptions

instance FromJSON IconSetMeta where
  parseJSON = genericParseJSON dropLabelPrefixOptions

data IconSet = IconSet
  { isMeta   :: IconSetMeta
  , isSprite :: Document
  } deriving (Show)

instance ToJSON IconSet where
  toJSON = toJSON . isMeta

newtype Icon = Icon { iName :: Text }
  deriving (Show, Generic)

instance ToJSON Icon where
  toJSON = genericToJSON dropLabelPrefixOptions

instance FromJSON Icon where
  parseJSON = genericParseJSON dropLabelPrefixOptions


loadIconSet
  :: (HasLogFunc env)
  => FilePath -> RIO env (Either String IconSet)
loadIconSet fp = eitherDecode <$> liftIO (BL.readFile fp) >>= \case
  Left e     -> pure (Left e)
  Right meta -> do
    svgs <- liftIO $ FP.find always (extension ==? ".svg") root
    (icons, names) <- foldM loadIcon ([], []) svgs
    pure . Right $ IconSet (meta { isIconList = names }) $ emptyDocument
      & set documentLocation fp
      & set elements icons
    where
      root = takeDirectory fp

loadIcon
  :: (HasLogFunc env)
  => ([Tree], [Icon]) -> FilePath -> RIO env ([Tree], [Icon])
loadIcon (els, ns) fp = liftIO (loadSvgFile fp) >>= \case
  Nothing  -> pure (els, ns)
  Just doc@Document{..} -> pure (symbol : els, name : ns)
    where
      symbol = SymbolTree (toSymbol doc)
      name = Icon $ fromString (takeBaseName _documentLocation)

toSymbol :: Document -> Symbol Tree
toSymbol Document{..} = defaultSvg
  & set (groupOfSymbol . groupDrawAttributes . attrId)
        (Just $ takeBaseName _documentLocation)
  & set (groupOfSymbol . groupViewBox) _viewBox
  & set (groupOfSymbol . groupChildren) _elements

emptyDocument :: Document
emptyDocument = Document Nothing Nothing Nothing [] M.empty "" [] ""
