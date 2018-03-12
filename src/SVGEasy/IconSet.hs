module SVGEasy.IconSet
  ( IconSet(..)
  , IconSetMeta(..)
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
  { isName        :: Text
  , isVersion     :: Text
  , isDescription :: Text
  , isLicense     :: Text
  , isHomepage    :: Text
  , isIconList    :: [Text]
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
  => ([Tree], [Text]) -> FilePath -> RIO env ([Tree], [Text])
loadIcon (els, ns) fp = liftIO (loadSvgFile fp) >>= \case
  Nothing  -> pure (els, ns)
  Just doc@Document{..} -> pure (symbol : els, name : ns)
    where
      symbol = SymbolTree (toSymbol doc)
      name = fromString (takeBaseName _documentLocation)

toSymbol :: Document -> Symbol Tree
toSymbol Document{..} = defaultSvg
  & over (groupOfSymbol . groupDrawAttributes)
         (set attrId $ Just $ takeBaseName _documentLocation)
  & over groupOfSymbol (set groupViewBox _viewBox)
  & over groupOfSymbol (set groupChildren _elements)

emptyDocument :: Document
emptyDocument = Document Nothing Nothing Nothing [] M.empty "" [] ""
