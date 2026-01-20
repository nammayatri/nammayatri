{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module GCP.GCS.Flow
  ( get'',
    put'',
    delete'',
    putRaw'',
    mockGet,
    mockPut,
    mockDelete,
    mockPutRaw,
    generateUploadUrl'',
    generateDownloadUrl'',
    mockGenerateUploadUrl,
    mockGenerateDownloadUrl,
    headRequest'',
    mockHeadRequest,
  )
where

import AWS.S3.Types (EntityTag (..), ObjectStatus (..))
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as DL (last)
import Data.String.Conversions
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import EulerHS.Prelude hiding (decodeUtf8, lines, show)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error
import Kernel.Utils.Common
import System.Directory (removeFile)
import qualified System.Directory as Dir
import System.FilePath.Posix as Path
import qualified System.IO as IO
import qualified System.IO.Temp as Temp
import System.Process
import Prelude (lines, read)

get'' ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  Text ->
  String ->
  m Text
get'' bucketName path = withLogTag "GCS" $ do
  liftIO $
    Temp.withSystemTempFile "gcs-download-" $ \tmpPath tmpHandle -> do
      IO.hClose tmpHandle -- Close handle so gsutil can write to it
      callProcess "gsutil" ["cp", "gs://" <> T.unpack bucketName <> "/" <> path, tmpPath]
      readFile tmpPath

put'' ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  Text ->
  String ->
  Text ->
  m ()
put'' bucketName path img = withLogTag "GCS" $ do
  liftIO $
    Temp.withSystemTempFile "gcs-upload-" $ \tmpPath tmpHandle -> do
      IO.hPutStr tmpHandle (T.unpack img)
      IO.hFlush tmpHandle
      IO.hClose tmpHandle
      callProcess "gsutil" ["cp", tmpPath, "gs://" <> T.unpack bucketName <> "/" <> path]

putRaw'' ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  Text ->
  String ->
  BS.ByteString ->
  String ->
  m ()
putRaw'' bucketName path bs _contentType = withLogTag "GCS" $ do
  liftIO $
    Temp.withSystemTempFile "gcs-upload-raw-" $ \tmpPath tmpHandle -> do
      BS.hPut tmpHandle bs
      IO.hFlush tmpHandle
      IO.hClose tmpHandle
      callProcess "gsutil" ["-h", "Content-Type:" <> _contentType, "cp", tmpPath, "gs://" <> T.unpack bucketName <> "/" <> path]

delete'' ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  Text ->
  String ->
  m ()
delete'' bucketName path = withLogTag "GCS" $ do
  liftIO $ callProcess "gsutil" ["rm", "gs://" <> T.unpack bucketName <> "/" <> path]

mockPut ::
  (MonadIO m, Log m) =>
  String ->
  Text ->
  String ->
  Text ->
  m ()
mockPut baseDirectory bucketName path img =
  withLogTag "GCS" $
    liftIO $ do
      let fullPath'Name = getFullPathMock baseDirectory bucketName path
          fullPath = Path.takeDirectory fullPath'Name
      Dir.createDirectoryIfMissing True fullPath
      T.writeFile fullPath'Name img

mockPutRaw ::
  (MonadIO m, Log m) =>
  String ->
  Text ->
  String ->
  ByteString ->
  String ->
  m ()
mockPutRaw baseDirectory bucketName path img _ =
  withLogTag "GCS" $
    liftIO $ do
      let fullPath'Name = getFullPathMock baseDirectory bucketName path
          fullPath = Path.takeDirectory fullPath'Name
      Dir.createDirectoryIfMissing True fullPath
      BS.writeFile fullPath'Name img

mockGet ::
  (MonadIO m, Log m) =>
  String ->
  Text ->
  String ->
  m Text
mockGet baseDirectory bucketName path =
  withLogTag "GCS" $
    liftIO $ do
      let fullPath'Name = getFullPathMock baseDirectory bucketName path
      T.readFile fullPath'Name

mockDelete ::
  (MonadIO m, Log m) =>
  String ->
  Text ->
  String ->
  m ()
mockDelete baseDirectory bucketName path =
  withLogTag "GCS" $
    liftIO $ do
      let fullPath'Name = getFullPathMock baseDirectory bucketName path
      liftIO $ removeFile fullPath'Name

getFullPathMock :: String -> Text -> String -> String
getFullPathMock baseDirectory bucketName path = baseDirectory <> "/" <> cs bucketName <> "/" <> path

-- | Generate a pre-signed URL for uploading
generateUploadUrl'' ::
  ( MonadFlow m
  ) =>
  Text ->
  String ->
  Seconds ->
  m Text
generateUploadUrl'' bucketName path expires = withLogTag "GCS" $ do
  -- GCS signed URL generation using gsutil
  let expiresStr = show (toInteger expires) <> "s"
      gsUri = "gs://" <> T.unpack bucketName <> "/" <> path
  result <- liftIO $ readProcess "gsutil" ["signurl", "-d", expiresStr, "-m", "PUT", gsUri] ""
  -- Parse table output: skip header line and extract URL from last column of data row
  let resultLines = T.lines $ T.pack result
      dataLines = drop 1 resultLines -- Skip header
      url = case dataLines of
        (dataLine : _) -> T.strip $ DL.last $ T.words dataLine
        [] -> ""
  return url

-- | Generate a pre-signed URL for downloading
generateDownloadUrl'' ::
  ( MonadFlow m
  ) =>
  Text ->
  String ->
  Seconds ->
  m Text
generateDownloadUrl'' bucketName path expires = withLogTag "GCS" $ do
  -- GCS signed URL generation using gsutil
  let expiresStr = show (toInteger expires) <> "s"
      gsUri = "gs://" <> T.unpack bucketName <> "/" <> path
  result <- liftIO $ readProcess "gsutil" ["signurl", "-d", expiresStr, "-m", "GET", gsUri] ""
  -- Parse table output: skip header line and extract URL from last column of data row
  let resultLines = T.lines $ T.pack result
      dataLines = drop 1 resultLines -- Skip header
      url = case dataLines of
        (dataLine : _) -> T.strip $ DL.last $ T.words dataLine
        [] -> ""
  return url

mockGenerateUploadUrl ::
  (MonadIO m, Log m) =>
  String ->
  Text ->
  String ->
  Seconds ->
  m Text
mockGenerateUploadUrl baseDirectory bucketName path _expires = withLogTag "GCS" $ do
  let fullPath = getFullPathMock baseDirectory bucketName path
  pure $ "file://" <> T.pack fullPath

mockGenerateDownloadUrl ::
  (MonadIO m, Log m) =>
  String ->
  Text ->
  String ->
  Seconds ->
  m Text
mockGenerateDownloadUrl baseDirectory bucketName path _expires = withLogTag "GCS" $ do
  let fullPath = getFullPathMock baseDirectory bucketName path
  pure $ "file://" <> T.pack fullPath

headRequest'' ::
  ( MonadFlow m,
    MonadThrow m
  ) =>
  Text ->
  String ->
  m ObjectStatus
headRequest'' bucketName path = withLogTag "GCS" $ do
  -- GCS stat using gsutil
  let gsUri = "gs://" <> T.unpack bucketName <> "/" <> path
  result <- liftIO $ readProcess "gsutil" ["stat", gsUri] ""
  let lines' = lines result
      fileSizeInBytes = extractFileSize lines'
      entityTag = EntityTag $ extractETag lines'
  pure $ ObjectStatus {fileSizeInBytes, entityTag}
  where
    extractFileSize :: [String] -> Integer
    extractFileSize ls =
      maybe 0 (read . dropWhile (== ' ')) $
        find (isPrefixOf "Content-Length:") ls >>= \line -> Just $ drop 1 $ dropWhile (/= ':') line
    extractETag :: [String] -> Text
    extractETag ls =
      maybe "\"\"" (T.strip . T.pack . dropWhile (== ' ')) $
        find (isPrefixOf "ETag:") ls >>= \line -> Just $ drop 1 $ dropWhile (/= ':') line

mockHeadRequest ::
  (MonadIO m, Log m, MonadThrow m) =>
  String ->
  Text ->
  String ->
  m ObjectStatus
mockHeadRequest baseDirectory bucketName path = withLogTag "GCS" $ do
  let fullPath'Name = getFullPathMock baseDirectory bucketName path
  fileExists <- liftIO $ Dir.doesFileExist fullPath'Name
  unless fileExists $ throwError (InvalidRequest $ "File does not exist: " <> T.pack fullPath'Name)
  fileContent <- liftIO $ BL.readFile fullPath'Name
  let fileSizeInBytes = toInteger $ BL.length fileContent
      md5Digest = MD5.hashlazy fileContent
      md5Hex = B16.encode md5Digest
      entityTag = EntityTag $ "\"" <> T.decodeUtf8 md5Hex <> "\""
  pure $ ObjectStatus {fileSizeInBytes, entityTag}
