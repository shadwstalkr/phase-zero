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
