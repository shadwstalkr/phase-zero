module Main where

import Control.Monad.Error
import System.Environment
import System.IO
import System.IO.Temp

import CollectVariables
import Extract
import Render

main = do
  (package:[]) <- getArgs

  withSystemTempDirectory "phasezero" $ \path -> do
         extractedPath <- extractPackage package path
         variables <- runErrorT $ collectVariables extractedPath
         case variables of
           Left err -> hPutStrLn stderr . show $ err
           Right var -> renderTemplates extractedPath var
