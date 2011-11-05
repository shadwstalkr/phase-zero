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

{-# LANGUAGE FlexibleContexts #-}

module CollectVariables (Variables, collectVariables) where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans
import qualified Data.ConfigFile as CP
import qualified Data.Map as Map
import qualified Data.Text as Txt
import qualified Data.Text.IO as TxtIo
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
  value <- TxtIo.getLine
  return $ Map.insert name value vars
