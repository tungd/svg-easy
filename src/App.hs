module App where

import RIO
import Network.Wai.Middleware.Static
import Network.Wai.Handler.Warp
import Servant

import SVGEasy.IconSet


data Env = Env
  { envLogFunc :: LogFunc
  , envIconSetList :: [IconSet]
  }

instance HasLogFunc Env where
  logFuncL = lens envLogFunc (\e f -> e { envLogFunc = f })


type SVGEasyAPI = Raw


start :: IO ()
start = do
  options <- logOptionsHandle stdout True
  withLogFunc options $ \envLogFunc -> runEnv 4000
    $ staticPolicy (defaultIndex >-> addBase "public")
    $ serve api $ enter (nt Env{..}) app
  where
    api = Proxy :: Proxy SVGEasyAPI
    nt env = NT (Servant.Handler . runRIO env)

defaultIndex :: Policy
defaultIndex = policy $ \path ->
  Just $ if path == "" then "index.html" else path

app :: (HasLogFunc env) => ServerT SVGEasyAPI (RIO env)
app = undefined
