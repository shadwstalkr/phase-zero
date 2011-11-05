module Main where

import Control.Monad
import System.Environment
import System.IO.Temp

import Extract
import Render

main = do
  (package:[]) <- getArgs

  withSystemTempDirectory "phasezero" (renderTemplates <=< extractPackage package)
