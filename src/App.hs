module App where

import Codec.Archive.Zip
import Data.Aeson
import Data.Has
import Data.Time.Clock.POSIX
import Graphics.Svg (Document, xmlOfDocument, saveXmlFile)
import Network.Wai.Handler.Warp
import Network.Wai.Application.Static
import Network.Wai.Web
import RIO
import RIO.FilePath
import RIO.Map
import System.FilePath.Find
import Text.Printf
import Text.XML.Light.Output
import Web.Routes
import Text.Heterocephalus

import qualified Data.Text.Lazy.Encoding as TL
import qualified RIO.ByteString.Lazy as BL
import qualified System.FilePath.Find as FP

import SVGEasy.IconSet
import SVGEasy.IconSetBuild


-- type DownloadIconSet =
--   ReqBody '[FormUrlEncoded] IconSetBuildSpec :> Download Post DownloadArchive

-- type SVGEasyAPI = "icon-sets" :> Get '[JSON] [IconSet]
--   :<|> "download" :> DownloadIconSet
--   :<|> "browser-support" :> Get '[HTML "browser-support"] Value
--   :<|> "tips-n-tricks" :> Get '[HTML "tips-n-tricks"] Value
--   :<|> Get '[HTML "index"] Value

-- newtype DownloadArchive = DownloadArchive { unArchive :: Archive }

-- instance MimeRender OctetStream DownloadArchive where
--   mimeRender _ = fromArchive . unArchive

data Sitemap = IconSets | Download | BrowserSupport | TipsNTricks
  deriving (Show, Generic)

instance PathInfo Sitemap


start
  :: (HasLogFunc env, Has [IconSet] env)
  => RIO env ()
start = do
  env <- ask
  liftIO $ runEnv 3000
    $ routeT "" (runRIO env) handler
    $ routeT "" (runRIO env) index
    $ staticApp (defaultWebAppSettings "public")


handler
  :: (HasLogFunc env, Has [IconSet] env)
  => Sitemap -> ApplicationT (RIO env)

handler = undefined


index :: Root -> ApplicationT (RIO env)
index _ _ = respond $(compileTextFile "templates/index.html")

head :: Html
head = $(compileTextFile "templates/head.html")

header :: Html
header = $(compileTextFile "templates/header.html")

  -- envPreviewTemplate <- compileMustacheFile "templates/preview.html"

  -- options <- logOptionsHandle stdout True
  -- withLogFunc options $ \envLogFunc -> do
  --   iss <- liftIO $ FP.find always (filePath ~~? "**/set.json") "resources"
  --   envIconSetList <- runRIO envLogFunc (foldM loadIconSet' [] iss)
  --   runEnv 4000
  --     $ staticPolicy (defaultIndex >-> addBase "public")
  --     $ serve api $ hoistServer api (runRIO Env{..}) app
  -- where
  --   api = Proxy :: Proxy SVGEasyAPI

loadIconSet'
  :: (HasLogFunc env)
  => [IconSet] -> FilePath -> RIO env [IconSet]
loadIconSet' isl meta = loadIconSet meta >>= \case
  Left err -> do
    logError (fromString $ printf "Error processing icon set %s" meta)
    logError (fromString err)
    pure isl
  Right is@IconSet{..} -> do
    logInfo (fromString $ printf "Writing sprite for icon set %s..." dest)
    liftIO $ saveXmlFile ("public" </> dest <.> "svg") isSprite
    pure (is : isl)
  where
    dest = takeBaseName (takeDirectory meta)

-- defaultIndex :: Policy
-- defaultIndex = policy $ \path ->
--   Just $ if path == "" then "index.html" else path

-- app :: ServerT SVGEasyAPI (RIO Env)
-- app = listIconSet :<|> buildIconSet
--   :<|> page "Browser support"
--   :<|> page "Tips and tricks"
--   :<|> page ""
--   where
--     page :: Monad m => String -> m Value
--     page title = pure (object [ "title" .= title ])

--     listIconSet = asks envIconSetList

--     buildIconSet spec = do
--       sprite <- buildSprite "svgicons.svg" spec
--       previewTemplate <- asks envPreviewTemplate
--       now <- round <$> liftIO getPOSIXTime
--       download "svgicons.zip" $ DownloadArchive $ emptyArchive
--         & addEntryToArchive (toEntry "svgicons.svg" now $ svgToByteString sprite)
--         & addEntryToArchive (toEntry "preview.html" now $ preview previewTemplate spec)

svgToByteString :: Document -> BL.ByteString
svgToByteString = fromString
  . ppcTopElement prettyConfigPP . xmlOfDocument

-- preview :: Template -> IconSetBuildSpec -> BL.ByteString
-- preview tmpl spec = TL.encodeUtf8
--   . renderMustache tmpl $ object [ "spec" .= elems spec ]
