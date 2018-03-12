module App where

import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Static
import RIO
import RIO.FilePath
import Servant
import System.FilePath.Find
import Text.Printf

import qualified Graphics.Svg as Svg
import qualified System.FilePath.Find as FP

import SVGEasy.IconSet


data Env = Env
  { envLogFunc :: LogFunc
  , envIconSetList :: [IconSet]
  }

instance HasLogFunc Env where
  logFuncL = lens envLogFunc (\e f -> e { envLogFunc = f })


type SVGEasyAPI = "icon-sets" :> Get '[JSON] [IconSet]


start :: IO ()
start = do
  options <- logOptionsHandle stdout True
  withLogFunc options $ \envLogFunc -> do
    iss <- liftIO $ FP.find always (filePath ~~? "**/set.json") "resources"
    envIconSetList <- runRIO envLogFunc (foldM loadIconSet' [] iss)
    runEnv 4000
      $ staticPolicy (defaultIndex >-> addBase "public")
      $ staticPolicy (addBase "resources")
      $ serve api $ enter (nt Env{..}) app
  where
    api = Proxy :: Proxy SVGEasyAPI
    nt env = NT (Servant.Handler . runRIO env)

loadIconSet'
  :: HasLogFunc env =>
     [IconSet] -> FilePath -> RIO env [IconSet]
loadIconSet' isl meta = loadIconSet meta >>= \case
  Left err -> do
    logError (fromString $ printf "Error processing icon set %s" meta)
    logError (fromString err)
    pure isl
  Right is@IconSet{..} -> do
    logInfo (fromString $ printf "Writing sprite for icon set %s..." dest)
    liftIO $ Svg.saveXmlFile ("public" </> dest <.> "svg") isSprite
    pure (is : isl)
  where
    dest = takeBaseName (takeDirectory meta)

defaultIndex :: Policy
defaultIndex = policy $ \path ->
  Just $ if path == "" then "index.html" else path

app :: ServerT SVGEasyAPI (RIO Env)
app = listIconSet
  where
    listIconSet = asks envIconSetList
