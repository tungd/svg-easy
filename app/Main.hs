module Main where

import RIO
import System.FilePath.Find 

import qualified System.FilePath.Find as FP

import App
import Logging ()


main :: IO ()
main = do
  (logger, closeLogger) <- newLogFunc =<< logOptionsHandle stdout True

  iconsets <- runRIO (logger, True) $ do
    iss <- liftIO $ FP.find always (filePath ~~? "**/set.json") "resources"
    foldM loadIconSet' [] iss

  runRIO (logger, iconsets) start
  closeLogger
