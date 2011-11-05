{-
Copyright Alex Midgley 2011

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Alex Midgley nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

module Render where

import Control.Arrow
import Control.Monad.Trans
import qualified Data.Enumerator as E
import Data.Enumerator (($$), (=$))
import qualified Data.Enumerator.List as EL
import qualified Data.Map as Map
import qualified Data.Text.Lazy as Txt
import qualified Data.Text.Lazy.IO as TxtIo
import System.Directory
import System.FilePath
import Text.StringTemplate

import CollectVariables

renderTemplates :: FilePath -> Variables -> IO ()
renderTemplates templatePath vars = 
    E.run_ $  EL.unfoldM listFiles [base]
           $$ EL.mapM (runKleisli $ makePath &&& renderFile)
           =$ EL.mapM (runKleisli . first . Kleisli $ makeDir)
           =$ EL.mapM_ writeTemplate
    where
      base = templatePath </> "src"
      makePath = arr (replacePath base vars)
      renderFile = Kleisli (renderTemplate vars)

listFiles :: [FilePath] -> IO (Maybe (FilePath, [FilePath]))
listFiles [] = return Nothing
listFiles (path:paths)
    | (takeFileName path) `elem` [".", ".."] = listFiles paths
    | otherwise               = do
  isDir <- doesDirectoryExist path
  if isDir
    then do
      files <- map (path </>) `fmap` getDirectoryContents path
      listFiles $ files ++ paths
    else return $ Just (path, paths)

replacePath :: FilePath -> Variables -> FilePath -> FilePath
replacePath base vars = makeRelative basePath . concat . map replaceVar . splitPath
    where
      basePath = addTrailingPathSeparator base
      replaceVar = Txt.unpack . renderStringTmpl vars

renderTemplate :: Variables -> FilePath -> IO Txt.Text
renderTemplate vars = fmap (renderStringTmpl vars) . readFile

writeTemplate :: (FilePath, Txt.Text) -> IO ()
writeTemplate = uncurry TxtIo.writeFile

makeDir path = createDirectoryIfMissing True (takeDirectory path) >> return path

renderStringTmpl :: Variables -> String -> Txt.Text
renderStringTmpl vars = render . setManyAttrib (Map.toList vars) . newSTMP
