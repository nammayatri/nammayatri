{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module AWS.S3.Flow (get', put', get'', put'', mockGet, mockPut) where

import AWS.S3.Error
import AWS.S3.Types
import AWS.S3.Utils
import qualified Data.List as DL (last)
import Data.String.Conversions
import qualified Data.Text as T
import qualified Data.Text.IO as T
import EulerHS.Prelude hiding (decodeUtf8, get, put, show)
import qualified EulerHS.Types as ET
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Utils.Common
import Servant
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

s3GetAPI :: Proxy S3GetAPI
s3GetAPI = Proxy

s3PutAPI :: Proxy S3PutAPI
s3PutAPI = Proxy

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
    MonadFlow m
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
    MonadFlow m
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

callS3API :: CallAPI env api a
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

getFullPathMock :: String -> Text -> String -> String
getFullPathMock baseDirectory bucketName path = baseDirectory <> "/" <> cs bucketName <> "/" <> path
