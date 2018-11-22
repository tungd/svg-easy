module App where

import Codec.Archive.Zip
import Data.Aeson
import Data.Has
import Data.Time.Clock.POSIX
import Graphics.Svg (Document, xmlOfDocument, saveXmlFile)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import Network.Wai.Parse
import Network.Wai.Web
import RIO
import RIO.FilePath
import Text.Heterocephalus
import Text.Printf
import Text.XML.Light.Output
import Web.Routes

import qualified RIO.ByteString.Lazy as BL
import qualified Text.Blaze.Html.Renderer.Utf8 as Html

import SVGEasy.IconSet
import SVGEasy.IconSetBuild


data DownloadArchive = DownloadArchive
  { download :: ByteString
  , archive :: Archive
  }

instance ToResponse DownloadArchive where
  respond DownloadArchive{..} = pure
    $ responseBuilder ok200
      [ ( hContentType, "application/octet-stream" )
      , ( "Content-Disposition"
        , "attachment; filename=\"" <> download <> "\"")
      ]
    $ getUtf8Builder $ displayBytesUtf8 $ BL.toStrict $ fromArchive archive


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

handler IconSets _ = do
  iconsets :: [IconSet] <- asks getter
  respond (Json iconsets)

handler Download req = do
  now <- round <$> liftIO getPOSIXTime
  (params, _) <- liftIO $ parseRequestBody lbsBackEnd req
  case decode . BL.fromStrict =<< lookup "spec" params of
    Nothing ->
      respond ("Bad Request" :: Text, badRequest400)
    Just spec -> do
      sprite <- buildSprite "svgicons.svg" spec
      respond $ DownloadArchive "svgicons.zip" $ emptyArchive
        & addEntryToArchive (toEntry "svgicons.svg" now $ svgToByteString sprite)
        & addEntryToArchive (toEntry "preview.html" now $ Html.renderHtml $(compileTextFile "templates/preview.html"))

handler BrowserSupport _ = 
  respond $(compileTextFile "templates/browser-support.html")

handler TipsNTricks _ = 
  respond $(compileTextFile "templates/tips-n-tricks.html")


index :: Root -> ApplicationT (RIO env)
index _ _ = respond $(compileTextFile "templates/index.html")

head :: Text -> Html
head title = $(compileTextFile "templates/head.html")

header :: Html
header = $(compileTextFile "templates/header.html")

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


svgToByteString :: Document -> BL.ByteString
svgToByteString = fromString
  . ppcTopElement prettyConfigPP . xmlOfDocument
