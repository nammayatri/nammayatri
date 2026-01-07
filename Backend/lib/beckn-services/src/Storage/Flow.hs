{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Flow where

import qualified AWS.S3.Flow as S3
import qualified AWS.S3.Types as S3Types
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Time.Format.ISO8601 (iso8601Show)
import EulerHS.Prelude hiding (id)
import qualified GCP.GCS.Flow as GCS
import qualified GCP.GCS.Types as GCSTypes
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error
import Kernel.Utils.Common
import Storage.Types (FileType (..), ObjectStatus (..), StorageConfig (..), StorageProvider (..))

-- | Unified put operation that can write to S3, GCS, or both based on config
-- Writes to primary first, then to secondary if configured
put ::
  ( CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasField "storageConfig" r StorageConfig
  ) =>
  String ->
  Text ->
  m ()
put path file_ = do
  storageConfig <- asks (.storageConfig)
  let primaryProvider = storageConfig.primaryStorage
      mbSecondaryProvider = storageConfig.secondaryStorage
      writeToBoth = storageConfig.enableMultiCloudWrite && isJust mbSecondaryProvider

  -- Write to primary first
  putToProvider primaryProvider path file_ >>= \case
    Right _ -> pure ()
    Left err -> throwError $ InternalError $ "Failed to write to primary storage: " <> show err

  -- Write to secondary if configured
  when writeToBoth $ do
    whenJust mbSecondaryProvider $ \secondaryProvider -> do
      putToProvider secondaryProvider path file_ >>= \case
        Right _ -> pure ()
        Left err -> logError $ "Failed to write to secondary storage: " <> show err

-- | Unified putRaw operation
-- Writes to primary first, then to secondary if configured
putRaw ::
  ( CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasField "storageConfig" r StorageConfig
  ) =>
  String ->
  BS.ByteString ->
  String ->
  m ()
putRaw path file_ contentType_ = do
  storageConfig <- asks (.storageConfig)
  let primaryProvider = storageConfig.primaryStorage
      mbSecondaryProvider = storageConfig.secondaryStorage
      writeToBoth = storageConfig.enableMultiCloudWrite && isJust mbSecondaryProvider

  -- Write to primary first
  putRawToProvider primaryProvider path file_ contentType_ >>= \case
    Right _ -> pure ()
    Left err -> throwError $ InternalError $ "Failed to write to primary storage: " <> show err

  -- Write to secondary if configured
  when writeToBoth $ do
    whenJust mbSecondaryProvider $ \secondaryProvider -> do
      putRawToProvider secondaryProvider path file_ contentType_ >>= \case
        Right _ -> pure ()
        Left err -> logError $ "Failed to write to secondary storage: " <> show err

-- | Unified get operation (reads from primary only)
get ::
  ( CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasField "storageConfig" r StorageConfig
  ) =>
  String ->
  m Text
get path = do
  storageConfig <- asks (.storageConfig)
  let primaryProvider = storageConfig.primaryStorage
  getFromProvider primaryProvider path

-- | Unified delete operation (deletes from both if multi-cloud is enabled)
-- Deletes from primary first, then from secondary if configured
delete ::
  ( CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasField "storageConfig" r StorageConfig
  ) =>
  String ->
  m ()
delete path = do
  storageConfig <- asks (.storageConfig)
  let primaryProvider = storageConfig.primaryStorage
      mbSecondaryProvider = storageConfig.secondaryStorage
      deleteFromBoth = storageConfig.enableMultiCloudWrite && isJust mbSecondaryProvider

  -- Delete from primary first
  deleteFromProvider primaryProvider path >>= \case
    Right _ -> pure ()
    Left err -> throwError $ InternalError $ "Failed to delete from primary storage: " <> show err

  -- Delete from secondary if configured
  when deleteFromBoth $ do
    whenJust mbSecondaryProvider $ \secondaryProvider -> do
      deleteFromProvider secondaryProvider path >>= \case
        Right _ -> pure ()
        Left err -> logError $ "Failed to delete from secondary storage: " <> show err

-- | Unified headRequest operation (reads from primary only)
headRequest ::
  ( CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasField "storageConfig" r StorageConfig
  ) =>
  String ->
  m ObjectStatus
headRequest path = do
  storageConfig <- asks (.storageConfig)
  let primaryProvider = storageConfig.primaryStorage
  headRequestFromProvider primaryProvider path

-- | Unified generateUploadUrl (from primary only)
generateUploadUrl ::
  ( CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasField "storageConfig" r StorageConfig
  ) =>
  String ->
  Seconds ->
  m Text
generateUploadUrl path expires = do
  storageConfig <- asks (.storageConfig)
  let primaryProvider = storageConfig.primaryStorage
  generateUploadUrlFromProvider primaryProvider path expires

-- | Unified generateDownloadUrl (from primary only)
generateDownloadUrl ::
  ( CoreMetrics m,
    MonadFlow m,
    MonadReader r m,
    HasField "storageConfig" r StorageConfig
  ) =>
  String ->
  Seconds ->
  m Text
generateDownloadUrl path expires = do
  storageConfig <- asks (.storageConfig)
  let primaryProvider = storageConfig.primaryStorage
  generateDownloadUrlFromProvider primaryProvider path expires

-- Internal helper functions

putToProvider :: (CoreMetrics m, MonadFlow m) => StorageProvider -> String -> Text -> m (Either SomeException ())
putToProvider provider path file_ = do
  result <- try $ case provider of
    StorageS3 (S3Types.S3AwsConf config) -> S3.put'' config.bucketName path file_
    StorageS3 (S3Types.S3MockConf config) -> S3.mockPut config.baseLocalDirectory config.bucketName path file_
    StorageGCS (GCSTypes.GCSConf config) -> GCS.put'' config.bucketName path file_
    StorageGCS (GCSTypes.GCSMockConf config) -> GCS.mockPut config.baseLocalDirectory config.bucketName path file_
  return $ case result of
    Right _ -> Right ()
    Left err -> Left err

putRawToProvider :: (CoreMetrics m, MonadFlow m) => StorageProvider -> String -> BS.ByteString -> String -> m (Either SomeException ())
putRawToProvider provider path file_ contentType_ = do
  result <- try $ case provider of
    StorageS3 (S3Types.S3AwsConf config) -> S3.putRaw'' config.bucketName path file_ contentType_
    StorageS3 (S3Types.S3MockConf config) -> S3.mockPutRaw config.baseLocalDirectory config.bucketName path file_ contentType_
    StorageGCS (GCSTypes.GCSConf config) -> GCS.putRaw'' config.bucketName path file_ contentType_
    StorageGCS (GCSTypes.GCSMockConf config) -> GCS.mockPutRaw config.baseLocalDirectory config.bucketName path file_ contentType_
  return $ case result of
    Right _ -> Right ()
    Left err -> Left err

getFromProvider :: (CoreMetrics m, MonadFlow m) => StorageProvider -> String -> m Text
getFromProvider provider path = case provider of
  StorageS3 (S3Types.S3AwsConf config) -> S3.get'' config.bucketName path
  StorageS3 (S3Types.S3MockConf config) -> S3.mockGet config.baseLocalDirectory config.bucketName path
  StorageGCS (GCSTypes.GCSConf config) -> GCS.get'' config.bucketName path
  StorageGCS (GCSTypes.GCSMockConf config) -> GCS.mockGet config.baseLocalDirectory config.bucketName path

deleteFromProvider :: (CoreMetrics m, MonadFlow m) => StorageProvider -> String -> m (Either SomeException ())
deleteFromProvider provider path = do
  result <- try $ case provider of
    StorageS3 (S3Types.S3AwsConf config) -> S3.delete'' config.bucketName path
    StorageS3 (S3Types.S3MockConf config) -> S3.mockDelete config.baseLocalDirectory config.bucketName path
    StorageGCS (GCSTypes.GCSConf config) -> GCS.delete'' config.bucketName path
    StorageGCS (GCSTypes.GCSMockConf config) -> GCS.mockDelete config.baseLocalDirectory config.bucketName path
  return $ case result of
    Right _ -> Right ()
    Left err -> Left err

headRequestFromProvider :: (CoreMetrics m, MonadFlow m) => StorageProvider -> String -> m ObjectStatus
headRequestFromProvider provider path = case provider of
  StorageS3 (S3Types.S3AwsConf config) -> S3.headRequest' config.bucketName path
  StorageS3 (S3Types.S3MockConf config) -> S3.mockHeadRequest config.baseLocalDirectory config.bucketName path
  StorageGCS (GCSTypes.GCSConf config) -> GCS.headRequest'' config.bucketName path
  StorageGCS (GCSTypes.GCSMockConf config) -> GCS.mockHeadRequest config.baseLocalDirectory config.bucketName path

generateUploadUrlFromProvider :: (CoreMetrics m, MonadFlow m) => StorageProvider -> String -> Seconds -> m Text
generateUploadUrlFromProvider provider path expires = case provider of
  StorageS3 (S3Types.S3AwsConf config) -> S3.generateUploadUrl' config.bucketName path expires
  StorageS3 (S3Types.S3MockConf config) -> S3.mockGenerateUploadUrl config.baseLocalDirectory config.bucketName path expires
  StorageGCS (GCSTypes.GCSConf config) -> GCS.generateUploadUrl'' config.bucketName path expires
  StorageGCS (GCSTypes.GCSMockConf config) -> GCS.mockGenerateUploadUrl config.baseLocalDirectory config.bucketName path expires

generateDownloadUrlFromProvider :: (CoreMetrics m, MonadFlow m) => StorageProvider -> String -> Seconds -> m Text
generateDownloadUrlFromProvider provider path expires = case provider of
  StorageS3 (S3Types.S3AwsConf config) -> S3.generateDownloadUrl' config.bucketName path expires
  StorageS3 (S3Types.S3MockConf config) -> S3.mockGenerateDownloadUrl config.baseLocalDirectory config.bucketName path expires
  StorageGCS (GCSTypes.GCSConf config) -> GCS.generateDownloadUrl'' config.bucketName path expires
  StorageGCS (GCSTypes.GCSMockConf config) -> GCS.mockGenerateDownloadUrl config.baseLocalDirectory config.bucketName path expires

-- | Extract path prefix from a storage provider
getPathPrefixFromProvider :: StorageProvider -> Text
getPathPrefixFromProvider provider = case provider of
  StorageS3 (S3Types.S3AwsConf config) -> config.pathPrefix
  StorageS3 (S3Types.S3MockConf config) -> config.pathPrefix
  StorageGCS (GCSTypes.GCSConf config) -> config.pathPrefix
  StorageGCS (GCSTypes.GCSMockConf config) -> config.pathPrefix

-- | Unified createFilePath operation (uses primary storage's path prefix)
createFilePath ::
  ( MonadTime m,
    MonadReader r m,
    HasField "storageConfig" r StorageConfig
  ) =>
  Text ->
  Text ->
  FileType ->
  Text ->
  m Text
createFilePath domain identifier fileType validatedFileExtention = do
  storageConfig <- asks (.storageConfig)
  let pathPrefix = getPathPrefixFromProvider storageConfig.primaryStorage
  now <- getCurrentTime
  let fileName = T.replace (T.singleton ':') (T.singleton '-') (T.pack $ iso8601Show now)
  return
    ( pathPrefix <> domain <> identifier <> "/"
        <> show fileType
        <> "/"
        <> fileName
        <> validatedFileExtention
    )

-- | Unified createFilePublicPath operation (uses primary storage's path prefix)
-- Unlike createFilePath, this uses the provided filename directly without timestamp
createFilePublicPath ::
  ( MonadReader r m,
    HasField "storageConfig" r StorageConfig
  ) =>
  Text ->
  Text ->
  Text ->
  Text ->
  m Text
createFilePublicPath domain identifier filename validatedFileExtention = do
  storageConfig <- asks (.storageConfig)
  let pathPrefix = getPathPrefixFromProvider storageConfig.primaryStorage
  return
    ( pathPrefix <> "/" <> domain <> "/"
        <> identifier
        <> "/"
        <> filename
        <> validatedFileExtention
    )
