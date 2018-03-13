module Servant.Download where

import RIO
import Servant
import Text.Printf


type Download verb content =
  verb '[OctetStream] (Headers '[Header "Content-Disposition" String] content)

download
  :: (Monad m)
  => String -> content -> m (Headers '[Header "Content-Disposition" String] content)
download name content = pure (addHeader name' content)
  where name' = printf "attachment; filename=\"%s\"" name
