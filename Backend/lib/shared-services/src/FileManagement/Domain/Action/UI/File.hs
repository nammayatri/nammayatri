module FileManagement.Domain.Action.UI.File where

import qualified AWS.S3 as S3
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude (withFile)
import EulerHS.Types (base64Encode)
import FileManagement.Common.UI.File
import GHC.IO.Handle (hFileSize)
import GHC.IO.IOMode (IOMode (..))
import qualified IssueManagement.Domain.Action.UI.Issue as IssueAction
import qualified IssueManagement.Domain.Types.MediaFile as D
import IssueManagement.Storage.BeamFlow
import qualified IssueManagement.Storage.Queries.MediaFile as QMF
import IssueManagement.Tools.Error
import Kernel.Prelude
import Kernel.Utils.Common

fileUploadToS3 ::
  ( BeamFlow m r,
    MonadTime m,
    MonadReader r m,
    HasField "s3Env" r (S3.S3Env m)
  ) =>
  Int ->
  Text ->
  FileUploadReq ->
  m FileUploadRes
fileUploadToS3 mediaFileSizeUpperLimit mediaFileUrlPattern FileUploadReq {..} = do
  let cfg = domainConfig domain
  contentType <- IssueAction.validateContentType fileType reqContentType
  fileSize <- L.runIO $ withFile file ReadMode hFileSize
  when (fileSize > fromIntegral mediaFileSizeUpperLimit) $
    throwError $ FileSizeExceededError (show fileSize)
  mediaFile <- L.runIO $ base64Encode <$> BS.readFile file
  filePath <- S3.createFilePath cfg.s3Prefix "" fileType contentType
  let url =
        cfg.routeSegment <&> \seg ->
          mediaFileUrlPattern
            & T.replace "<DOMAIN>" seg
            & T.replace "<FILE_PATH>" filePath
  uploadRes <- IssueAction.createMediaEntry (fromMaybe filePath url) fileType filePath
  fork "S3 Put File Media" $ do
    uploadOk <-
      (S3.put (T.unpack filePath) mediaFile >> pure True)
        `catch` \(e :: SomeException) -> do
          logError $ "S3 upload failed for file media " <> uploadRes.fileId.getId <> ": " <> show e
          pure False
    QMF.updateStatusById (if uploadOk then D.COMPLETED else D.FAILED) uploadRes.fileId
  return $
    FileUploadRes
      { fileId = uploadRes.fileId,
        filePath = filePath,
        url = url,
        status = D.PENDING
      }

fetchFileMedia ::
  ( MonadReader r m,
    HasField "s3Env" r (S3.S3Env m)
  ) =>
  Text ->
  m Text
fetchFileMedia filePath = S3.get (T.unpack filePath)
