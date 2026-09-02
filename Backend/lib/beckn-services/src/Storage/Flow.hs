module Storage.Flow
  ( buildStorageEnvIO,
    module Storage.Types,
  )
where

import qualified AWS.S3.Init as S3Init
import AWS.S3.Types
import qualified Data.Text as T
import EulerHS.Prelude hiding (show)
import Kernel.Storage.Esqueleto.Logger (runLoggerIO)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Version (CloudType (..))
import Kernel.Utils.App (lookupCloudType)
import Kernel.Utils.Common
import Kernel.Utils.IOLogging (HasLog, LoggerEnv)
import qualified Storage.CloudRouter as CloudRouter
import qualified Storage.GCS.Flow as GcsFlow
import Storage.Types
import System.Environment (lookupEnv)

mkGcsStorageEnv ::
  (MonadFlow m, CoreMetrics m, MonadReader r m, HasLog r) =>
  GcsFlow.TokenCache ->
  GCSGcpConfig ->
  S3Env m
mkGcsStorageEnv tokenCache g =
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

lookupEnvFlag :: String -> IO Bool
lookupEnvFlag name = fromMaybe False . (>>= readMaybe) <$> lookupEnv name

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
  awsEnv <- case cfg.awsConfig of
    Nothing -> do
      let err = "awsConfig is missing — S3 is mandatory for cloud-aware storage routing" :: Text
      logInfo $ "[Storage] " <> err
      liftIO $ fail $ T.unpack err
    Just s3Conf -> pure $ S3Init.buildS3Env s3Conf
  mbGcsEnv <- case cfg.gcsConfig of
    Nothing -> do
      logInfo "[Storage] gcsConfig is missing — GCS dual-write/recache/GCP-native reads are disabled"
      pure Nothing
    Just (GCSGcpConf g) -> do
      tokenCache <- liftIO GcsFlow.newTokenCache
      pure $ Just (mkGcsStorageEnv tokenCache g)
  dualWriteEnabled <- liftIO $ lookupEnvFlag "ENABLE_GCS_DUAL_WRITE"
  recacheEnabled <- liftIO $ lookupEnvFlag "ENABLE_GCS_RECACHE_ON_FALLBACK"
  logInfo $
    "[Storage] cloudType=" <> show cloudType
      <> " dualWrite="
      <> show dualWriteEnabled
      <> " recache="
      <> show recacheEnabled
      <> " gcsConfigured="
      <> show (isJust mbGcsEnv)
  pure $ CloudRouter.buildCloudAwareEnv cloudType dualWriteEnabled recacheEnabled awsEnv mbGcsEnv
