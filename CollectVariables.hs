{-# LANGUAGE FlexibleContexts #-}

module CollectVariables (Variables, collectVariables) where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans
import qualified Data.ConfigFile as CP
import qualified Data.Map as Map
import qualified Data.Text as Txt
import System.Directory
import System.FilePath
import System.IO

type Variables = Map.Map String Txt.Text

collectVariables :: (MonadError CP.CPError m, MonadIO m)
                 => FilePath
                 -> m Variables
collectVariables packagePath = liftM2 Map.union (queryUser varPath) defaultVars
    where
      varPath = packagePath </> "variables.conf"

defaultVars :: MonadIO m => m Variables
defaultVars = return Map.empty

queryUser :: (MonadError CP.CPError m, MonadIO m) => FilePath -> m Variables
queryUser varPath = do
  exists <- liftIO $ doesFileExist varPath
  if exists then doQueries else return Map.empty

    where
      doQueries = do
        varConf <- join $ liftIO $ CP.readfile CP.emptyCP varPath
        items <- liftM concat . mapM (CP.items varConf) $ CP.sections varConf
        liftIO $ foldM query Map.empty items
        
query :: Variables -> (String, String) -> IO Variables
query vars (name, prompt) = do
  putStr $ prompt ++ "? "
  hFlush stdout
  value <- fmap Txt.pack getLine
  return $ Map.insert name value vars
