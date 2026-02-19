module Storage.Flow
  ( buildStorageEnvIO,
    module Storage.Types,
  )
where

import qualified AWS.S3.Init as S3Init
import AWS.S3.Types
import qualified Data.Text as T
import qualified Storage.GCS.Flow as GcsFlow
import Storage.Types
import EulerHS.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Version (CloudType (..))
import Kernel.Utils.App (lookupCloudType)
import Kernel.Utils.Common
import Kernel.Utils.IOLogging (HasLog, LoggerEnv)
import Kernel.Storage.Esqueleto.Logger (runLoggerIO)


mkAwsStorageEnv ::
  (MonadFlow m, CoreMetrics m) =>
  StorageServiceConfig ->
  Either Text (StorageEnv m)
mkAwsStorageEnv cfg =
  case cfg.awsConfig of
    Just s3Conf -> Right $ S3Init.buildS3Env s3Conf
    Nothing -> Left "awsConfig is missing — cannot route to AWS S3"

mkGcsStorageEnv ::
  (MonadFlow m, CoreMetrics m, MonadReader r m, HasLog r) =>
  GcsFlow.TokenCache ->
  StorageServiceConfig ->
  Either Text (StorageEnv m)
mkGcsStorageEnv tokenCache cfg =
  case cfg.gcsConfig of
    Nothing -> Left "gcsConfig is missing — cannot route to GCS"
    Just (GCSGcpConf g) ->
      Right $
        S3Env
          { pathPrefix = g.pathPrefix,
            getH = GcsFlow.get tokenCache g.bucketName,
            putH = GcsFlow.put tokenCache g.bucketName,
            putRawH = GcsFlow.putRaw tokenCache g.bucketName,
            deleteH = GcsFlow.delete tokenCache g.bucketName,
            generateUploadUrlH = GcsFlow.generateUploadUrl g.bucketName,
            generateDownloadUrlH = GcsFlow.generateDownloadUrl g.bucketName,
            headRequestH = GcsFlow.headRequest tokenCache g.bucketName
          }

buildStorageEnvIO ::
  forall m r.
  (MonadFlow m, CoreMetrics m, MonadReader r m, HasLog r) =>
  LoggerEnv ->
  StorageServiceConfig ->
  IO (StorageEnv m)
buildStorageEnvIO loggerEnv cfg = runLoggerIO loggerEnv $ do
  cloudType <-
    if cfg.isForcedAWS
      then do
        logInfo "[Storage] isForcedAWS=True → routing to AWS S3"
        pure AWS
      else do
        ct <- liftIO lookupCloudType
        logInfo $ "[Storage] Cloud detection → " <> show ct
        pure ct
  case cloudType of
    AWS -> do
      logInfo "[Storage] Routing to AWS S3"
      case mkAwsStorageEnv cfg of
        Left err -> do
          logInfo $ "[Storage] " <> err
          liftIO $ fail $ T.unpack err
        Right env -> pure env
    GCP -> do
      logInfo "[Storage] Routing to GCS"
      tokenCache <- liftIO GcsFlow.newTokenCache
      case mkGcsStorageEnv tokenCache cfg of
        Left err -> do
          logInfo $ "[Storage] " <> err
          liftIO $ fail $ T.unpack err
        Right env -> pure env
    _ -> do
      logInfo "[Storage] Unknown cloud type or unsupported configuration"
      liftIO $ fail "[Storage] Unknown cloud type"
