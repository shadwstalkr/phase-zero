module Extract where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.BZip as BZip
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BS
import Data.Char
import System.Directory
import System.FilePath

extractPackage :: FilePath -> FilePath -> IO FilePath
extractPackage package extractPath = do
  -- If the package is just a directory, leave it alone
  isDir <- doesDirectoryExist package
  if isDir
    then return package
    else runExtractionPipeline >> return extractPath

    where
      runExtractionPipeline = extractionPipeline package extractPath

extractionPipeline :: FilePath -> FilePath -> IO ()
extractionPipeline package destination =
    BS.readFile package >>= buildPipeline (segments package)

    where 
      segments [] = []
      segments str =
          let (rest, ext) = splitExtension str
          in ext : segments rest

      buildPipeline fileSegments =
          let (compression, fileSegments') = parseCompression fileSegments
              (archive, _) = parseArchive fileSegments'
          in archive . compression

      parseCompression :: [String] -> (BS.ByteString -> BS.ByteString, [String])
      parseCompression [] = (id, [])
      parseCompression (seg:rest) = case map toLower seg of
                                      ".gz"  -> (GZip.decompress, rest)
                                      ".bz2" -> (BZip.decompress, rest)
                                      _     -> (id, seg:rest)

      parseArchive :: [String] -> (BS.ByteString -> IO (), [String])
      parseArchive [] = (const $ return (), [])
      parseArchive (seg:rest) = case map toLower seg of
                                  ".tar" -> (Tar.unpack destination . Tar.read, rest)
                                  _     -> (const $ return (), seg:rest)
