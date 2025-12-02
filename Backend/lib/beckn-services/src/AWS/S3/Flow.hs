{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module AWS.S3.Flow
  ( get',
    put',
    get'',
    put'',
    delete',
    delete'',
    mockGet,
    mockPut,
    mockDelete,
    putRaw'',
    mockPutRaw,
    generateUploadUrl',
    generateDownloadUrl',
    mockGenerateUploadUrl,
    mockGenerateDownloadUrl,
    headRequest',
    mockHeadRequest,
  )
where

import AWS.S3.Error
import AWS.S3.Types
import AWS.S3.Utils
import qualified Amazonka
import qualified Amazonka.S3 as Amazonka
import qualified Amazonka.S3.HeadObject as Amazonka
import Control.Lens.Getter ((^.))
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as DL (last)
import Data.String.Conversions
import qualified Data.Text as T
import Data.Text.Encoding as T
import qualified Data.Text.IO as T
import EulerHS.Prelude hiding (decodeUtf8, get, put, show, (^.))
import qualified EulerHS.Types as ET
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error
import Kernel.Utils.Common
import Servant hiding (GET, PUT, throwError)
import Servant.Client
import System.Directory (removeFile)
import qualified System.Directory as Dir
import System.FilePath.Posix as Path
import qualified System.Posix.Files as Posix
import qualified System.Posix.IO as Posix
import System.Process

type S3GetAPI = Get '[S3ImageData] Text

type S3PutAPI =
  ReqBody '[S3ImageData] Text
    :> Put '[S3OctetStream] Text

type S3DeleteAPI = Delete '[S3OctetStream] Text

s3GetAPI :: Proxy S3GetAPI
s3GetAPI = Proxy

s3PutAPI :: Proxy S3PutAPI
s3PutAPI = Proxy

s3DeleteAPI :: Proxy S3DeleteAPI
s3DeleteAPI = Proxy

url :: String -> String -> BaseUrl
url path host =
  BaseUrl
    { baseUrlScheme = Https,
      baseUrlHost = host,
      baseUrlPort = 443,
      baseUrlPath = path
    }

s3Host :: Text -> String
s3Host bN = T.unpack bN <> ".s3.amazonaws.com"

get' ::
  ( CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  Text ->
  String ->
  m Text
get' bucketName path = do
  withLogTag "S3" $ do
    let host = s3Host bucketName
    callS3API
      (url path host)
      (ET.client s3GetAPI)
      "GetS3"
      s3GetAPI

put' ::
  ( CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  Text ->
  String ->
  Text ->
  m Text
put' bucketName path img = do
  withLogTag "S3" $ do
    let host = s3Host bucketName
    callS3API
      (url path host)
      (ET.client s3PutAPI img)
      "PutS3"
      s3PutAPI

delete' ::
  ( CoreMetrics m,
    MonadFlow m,
    HasRequestId r,
    MonadReader r m
  ) =>
  Text ->
  String ->
  m Text
delete' bucketName path = do
  withLogTag "S3" $ do
    let host = s3Host bucketName
    callS3API
      (url path host)
      (ET.client s3DeleteAPI)
      "DeleteS3"
      s3DeleteAPI

callS3API :: CallAPI m r api a
callS3API =
  callApiUnwrappingApiError
    (identity @S3Error)
    (Just $ ET.ManagerSelector s3AuthManagerKey)
    (Just "S3_NOT_AVAILABLE")
    Nothing

get'' ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  Text ->
  String ->
  m Text
get'' bucketName path = withLogTag "S3" $ do
  let tmpPath = getTmpPath path
  let cmd = "aws s3api get-object --bucket " <> T.unpack bucketName <> " --key " <> path <> " " <> tmpPath
  liftIO $ callCommand cmd
  result <- liftIO $ readFile tmpPath
  liftIO $ removeFile tmpPath
  return result

put'' ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  Text ->
  String ->
  Text ->
  m ()
put'' bucketName path img = withLogTag "S3" $ do
  let tmpPath = getTmpPath path
  liftIO $ writeFile_ tmpPath img
  let cmd = "aws s3api put-object --bucket " <> T.unpack bucketName <> " --key " <> path <> " --body " <> tmpPath
  liftIO $ callCommand cmd
  liftIO $ removeFile tmpPath
  where
    writeFile_ path_ img_ = do
      fd <- Posix.createFile path_ Posix.accessModes
      _ <- Posix.fdWrite fd (T.unpack img_)
      Posix.closeFd fd

putRaw'' ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  Text ->
  String ->
  BS.ByteString ->
  String ->
  m ()
putRaw'' bucketName path bs _contentType = withLogTag "S3" $ do
  let tmpPath = getTmpPath path
  liftIO $ BS.writeFile tmpPath bs
  let cmd = "aws s3api put-object --bucket " <> T.unpack bucketName <> " --key " <> path <> " --body " <> tmpPath <> " --content-type " <> _contentType
  liftIO $ callCommand cmd
  liftIO $ removeFile tmpPath

delete'' ::
  ( CoreMetrics m,
    MonadFlow m
  ) =>
  Text ->
  String ->
  m ()
delete'' bucketName path = withLogTag "S3" $ do
  let cmd = "aws s3api delete-object --bucket " <> T.unpack bucketName <> " --key " <> path
  liftIO $ callCommand cmd

getTmpPath :: String -> String
getTmpPath = (<>) "/tmp/" . T.unpack . DL.last . T.split (== '/') . T.pack

mockPut ::
  (MonadIO m, Log m) =>
  String ->
  Text ->
  String ->
  Text ->
  m ()
mockPut baseDirectory bucketName path img =
  withLogTag "S3" $
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
  withLogTag "S3" $
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
  withLogTag "S3" $
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
  withLogTag "S3" $
    liftIO $ do
      let fullPath'Name = getFullPathMock baseDirectory bucketName path
      liftIO $ removeFile fullPath'Name

getFullPathMock :: String -> Text -> String -> String
getFullPathMock baseDirectory bucketName path = baseDirectory <> "/" <> cs bucketName <> "/" <> path

-- | Generate a pre-signed URL for uploading a video
generateUploadUrl' ::
  ( MonadFlow m
  ) =>
  Text ->
  String ->
  Seconds ->
  m Text
generateUploadUrl' = generateUrl' PUT

-- | Generate a pre-signed URL for downloading a video
generateDownloadUrl' ::
  ( MonadFlow m
  ) =>
  Text ->
  String ->
  Seconds ->
  m Text
generateDownloadUrl' = generateUrl' GET

data Method = GET | PUT

generateUrl' ::
  ( MonadFlow m
  ) =>
  Method ->
  Text ->
  String ->
  Seconds ->
  m Text
generateUrl' method bucketName path expires = withLogTag "S3" $ do
  env <- Amazonka.newEnv Amazonka.discover
  now <- getCurrentTime
  let bucketName' = Amazonka.BucketName bucketName
      path' = Amazonka.ObjectKey $ T.pack path
      expires' = fromInteger $ toInteger expires
  bsUrl <- case method of
    GET -> Amazonka.presignURL env now expires' $ Amazonka.newGetObject bucketName' path'
    PUT -> Amazonka.presignURL env now expires' $ Amazonka.newPutObject bucketName' path' ""
  T.decodeUtf8' bsUrl & fromEitherM (\err -> InternalError $ "Unable to decode url: " <> show bsUrl <> "error: " <> show err)

mockGenerateUploadUrl ::
  (MonadIO m, Log m) =>
  String ->
  Text ->
  String ->
  Seconds ->
  m Text
mockGenerateUploadUrl baseDirectory bucketName path _expires = withLogTag "S3" $ do
  let fullPath = getFullPathMock baseDirectory bucketName path
  pure $ "file://" <> T.pack fullPath

mockGenerateDownloadUrl ::
  (MonadIO m, Log m) =>
  String ->
  Text ->
  String ->
  Seconds ->
  m Text
mockGenerateDownloadUrl baseDirectory bucketName path _expires = withLogTag "S3" $ do
  let fullPath = getFullPathMock baseDirectory bucketName path
  pure $ "file://" <> T.pack fullPath

headRequest' ::
  ( MonadFlow m
  ) =>
  Text ->
  String ->
  m ObjectStatus
headRequest' bucketName path = withLogTag "S3" $ do
  env <- Amazonka.newEnv Amazonka.discover
  let bucketName' = Amazonka.BucketName bucketName
      path' = Amazonka.ObjectKey $ T.pack path
  res <- liftIO . Amazonka.runResourceT $ Amazonka.send env (Amazonka.newHeadObject bucketName' path')
  fileSizeInBytes <- case res ^. Amazonka.headObjectResponse_contentLength of
    Just size -> pure size
    Nothing -> throwError (InvalidRequest "Content length was not found")
  entityTag <- case res ^. Amazonka.headObjectResponse_eTag of
    Just (Amazonka.ETag t) -> pure . EntityTag $ T.decodeUtf8 t
    Nothing -> throwError (InvalidRequest "Entity tag was not found")
  pure $ ObjectStatus {fileSizeInBytes, entityTag}

mockHeadRequest ::
  (MonadIO m, Log m, MonadThrow m) =>
  String ->
  Text ->
  String ->
  m ObjectStatus
mockHeadRequest baseDirectory bucketName path = withLogTag "S3" $ do
  let fullPath'Name = getFullPathMock baseDirectory bucketName path
  fileExists <- liftIO $ Dir.doesFileExist fullPath'Name
  unless fileExists $ throwError (InvalidRequest $ "File does not exist: " <> T.pack fullPath'Name)
  fileContent <- liftIO $ BL.readFile fullPath'Name
  let fileSizeInBytes = toInteger $ BL.length fileContent
      md5Digest = MD5.hashlazy fileContent
      md5Hex = B16.encode md5Digest
      entityTag = EntityTag $ "\"" <> T.decodeUtf8 md5Hex <> "\""
  pure $ ObjectStatus {fileSizeInBytes, entityTag}
