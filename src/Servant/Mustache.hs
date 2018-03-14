module Servant.Mustache
  ( Tpl
  , TplText
  , HTML
  , XML
  , htmlErr
  ) where

import Data.Aeson
import GHC.TypeLits
import Network.HTTP.Media ((//), (/:))
import Network.HTTP.Types
import RIO
import Servant
import System.FilePath.Find
import System.IO.Unsafe
import Text.Microstache

import qualified Data.Text.Lazy.Encoding as TL
import qualified RIO.Map as M
import qualified System.FilePath.Find as FP


data Tpl (ct :: *) (file :: Symbol)

instance Accept ct => Accept (Tpl ct file) where
  contentType _ = contentType (Proxy :: Proxy ct)

instance (KnownSymbol file, Accept ct, ToJSON val)
  => MimeRender (Tpl ct file) val where
  mimeRender _ val = TL.encodeUtf8 $ unsafePerformIO $ do
    -- TODO: we can also cache this and just try to switch the template over
    -- Let's just see what the performance is like then decide
    tmpl <- compileMustacheDir' name "templates"
    pure $ renderMustache tmpl $ toJSON val
    where
      name = fromString $ symbolVal (Proxy :: Proxy file)


data TplText (file :: Symbol)

instance Accept (TplText file) where
  contentType _ = "*" // "*"

instance (KnownSymbol file, ToJSON val)
  => MimeRender (TplText file) val where
  mimeRender _ = mimeRender (Proxy :: Proxy (Tpl (TplText file) file))


data HTML (file :: Symbol)

instance Accept (HTML file) where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance (KnownSymbol file, ToJSON val)
  => MimeRender (HTML file) val where
  mimeRender _ = mimeRender (Proxy :: Proxy (Tpl (HTML file) file))


data XML (file :: Symbol)

instance Accept (XML file) where
  contentType _ = "text" // "xml" /: ("charset", "utf-8")

instance (KnownSymbol file, ToJSON val)
  => MimeRender (XML file) val where
  mimeRender _ = mimeRender (Proxy :: Proxy (Tpl (XML file) file))


htmlErr
  :: (MonadIO m, ToJSON val)
  => ServantErr -> PName -> val -> m a
htmlErr err@ServantErr{..} name val = do
  tmpl <- compileMustacheDir' name "templates"
  throwIO err
    { errHeaders = errHeaders <> [(hContentType, "text/html;charset=utf-8")]
    , errBody = TL.encodeUtf8 $ renderMustache tmpl $ toJSON val
    }

compileMustacheDir' :: MonadIO m => PName -> FilePath -> m Template
compileMustacheDir' pname path =
  liftIO $ FP.find always (fileType ==? RegularFile) path >>=
  fmap selectKey . foldM f (Template undefined M.empty)
  where
    selectKey t = t { templateActual = pname }
    f (Template _ old) fp = do
      Template _ new <- compileMustacheFile fp
      return (Template undefined (M.union new old))
