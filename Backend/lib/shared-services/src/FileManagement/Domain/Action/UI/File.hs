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
  Text ->
  FileUploadReq ->
  m FileUploadRes
fileUploadToS3 mediaFileSizeUpperLimit mediaFileUrlPattern uploaderId FileUploadReq {..} = do
  let cfg = domainConfig domain
  contentType <- IssueAction.validateContentType fileType reqContentType
  when (cfg.storeRawBytes && fileType /= S3.Image) $
    throwError $ FileFormatNotSupported "only images are accepted"
  fileSize <- L.runIO $ withFile file ReadMode hFileSize
  when (fileSize > fromIntegral mediaFileSizeUpperLimit) $
    throwError $ FileSizeExceededError (show fileSize)
  rawFile <- L.runIO $ BS.readFile file
  when (cfg.scopeToUploader && fileType == S3.Image && not (looksLikeImage rawFile)) $
    throwError $ FileFormatNotSupported "not an image"
  let mediaFile = base64Encode rawFile
  pathIdentifier <- if cfg.scopeToUploader then (\guid -> "/" <> uploaderId <> "/" <> guid) <$> generateGUIDText else pure ""
  filePath <- S3.createFilePath cfg.s3Prefix pathIdentifier fileType contentType
  let url =
        cfg.routeSegment <&> \seg ->
          mediaFileUrlPattern
            & T.replace "<DOMAIN>" seg
            & T.replace "<FILE_PATH>" filePath
  uploadRes <- IssueAction.createMediaEntry (fromMaybe filePath url) fileType filePath
  uploadOk <-
    (storeFile cfg filePath contentType rawFile mediaFile >> pure True)
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

-- A domain that stores raw bytes keeps the real file in the store, so a signed link to it serves a picture. The other domains keep base64 text.
storeFile ::
  ( MonadReader r m,
    HasField "s3Env" r (S3.S3Env m)
  ) =>
  DomainConfig ->
  Text ->
  Text ->
  BS.ByteString ->
  Text ->
  m ()
storeFile cfg filePath extension rawFile base64File
  | cfg.storeRawBytes = S3.putRaw (T.unpack filePath) rawFile (T.unpack (imageMimeOfExtension extension))
  | otherwise = S3.put (T.unpack filePath) base64File

-- The content type stored with a raw file comes from the validated extension and never from the caller's header, because the store passes it to a command line.
imageMimeOfExtension :: Text -> Text
imageMimeOfExtension extension = case extension of
  "png" -> "image/png"
  "jpg" -> "image/jpeg"
  "gif" -> "image/gif"
  "webp" -> "image/webp"
  "heic" -> "image/heic"
  "heif" -> "image/heif"
  _ -> "application/octet-stream"

fetchFileMedia ::
  ( MonadReader r m,
    HasField "s3Env" r (S3.S3Env m)
  ) =>
  Text ->
  m Text
fetchFileMedia filePath = S3.get (T.unpack filePath)

-- A stored file of a domain that stores raw bytes, as the base64 text the apps expect.
readStoredFileAsBase64 ::
  ( MonadReader r m,
    HasField "s3Env" r (S3.S3Env m)
  ) =>
  Text ->
  m Text
readStoredFileAsBase64 filePath = base64Encode <$> S3.getRaw (T.unpack filePath)

-- The read for a path the server itself stored: a missing or unreadable file is answered as not found instead of as a server error.
fetchStoredFileOrNotFound ::
  ( MonadFlow m,
    MonadReader r m,
    HasField "s3Env" r (S3.S3Env m)
  ) =>
  Text ->
  m Text
fetchStoredFileOrNotFound filePath =
  readStoredFileAsBase64 filePath
    `catch` \(e :: SomeException) -> do
      logError $ "S3 read failed for " <> filePath <> ": " <> show e
      throwError $ FileDoesNotExist filePath

-- Same read for a path a caller sent: an empty or absolute path is not a file of ours.
fetchFileMediaOrNotFound ::
  ( MonadFlow m,
    MonadReader r m,
    HasField "s3Env" r (S3.S3Env m)
  ) =>
  Text ->
  m Text
fetchFileMediaOrNotFound filePath = do
  when (T.null filePath || "/" `T.isPrefixOf` filePath) $
    throwError $ FileDoesNotExist filePath
  fetchStoredFileOrNotFound filePath
