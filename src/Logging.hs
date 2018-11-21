{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Logging where

import RIO
import Data.Has

instance {-# OVERLAPPING #-}
  (Has LogFunc env) => HasLogFunc env where
  logFuncL = hasLens
