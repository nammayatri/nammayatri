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
import Kernel.Utils.Common hiding (logInfo, logWarning)
import Kernel.Utils.IOLogging (HasLog, LoggerEnv)
import Kernel.Utils.Logging (logInfo, logWarning)
import qualified Storage.CloudRouter as CloudRouter
import qualified Storage.GCS.Flow as GcsFlow
import Storage.Types
import System.Environment (lookupEnv)

mkGcsStorageEnv ::
  (MonadFlow m, CoreMetrics m, MonadReader r m, HasLog r) =>
  GcsFlow.GcsClient ->
  GCSGcpConfig ->
  S3Env m
mkGcsStorageEnv client g =
  S3Env
    { pathPrefix = g.pathPrefix,
      getH = GcsFlow.get client g.bucketName,
      putH = GcsFlow.put client g.bucketName,
      putRawH = GcsFlow.putRaw client g.bucketName,
      getRawH = GcsFlow.getRaw client g.bucketName,
      deleteH = GcsFlow.delete client g.bucketName,
      generateUploadUrlH = GcsFlow.generateUploadUrl client g.bucketName,
      generateDownloadUrlH = GcsFlow.generateDownloadUrl client g.bucketName,
      headRequestH = GcsFlow.headRequest client g.bucketName
    }

lookupEnvFlag :: (MonadIO m, Log m) => String -> m Bool
lookupEnvFlag name = do
  mbVal <- liftIO $ lookupEnv name
  case T.toLower . T.strip . T.pack <$> mbVal of
    Nothing -> pure False
    Just v
      | v `elem` ["true", "1", "yes", "y", "on"] -> pure True
      | v `elem` ["false", "0", "no", "n", "off", ""] -> pure False
      | otherwise -> do
        logWarning $ "[Storage] Unrecognised value '" <> v <> "' for " <> T.pack name <> ", treating as false"
        pure False

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
      client <- liftIO $ GcsFlow.newGcsClient g.metadataServerUrl g.gcsBaseUrl g.gcsUploadBaseUrl g.iamCredentialsBaseUrl
      when (g.pathPrefix /= awsEnv.pathPrefix) $
        logWarning $
          "[Storage] gcsConfig.pathPrefix ('" <> g.pathPrefix <> "') differs from awsConfig.pathPrefix ('" <> awsEnv.pathPrefix
            <> "'); object keys are generated with the AWS prefix and sent to GCS unchanged, so the GCS prefix is ignored"
      pure $ Just (mkGcsStorageEnv client g)
  dualWriteEnabled <- lookupEnvFlag "ENABLE_GCS_DUAL_WRITE"
  recacheEnabled <- lookupEnvFlag "ENABLE_GCS_RECACHE_ON_FALLBACK"
  logInfo $
    "[Storage] cloudType=" <> show cloudType
      <> " dualWrite="
      <> show dualWriteEnabled
      <> " recache="
      <> show recacheEnabled
      <> " gcsConfigured="
      <> show (isJust mbGcsEnv)
  pure $ CloudRouter.buildCloudAwareEnv cloudType dualWriteEnabled recacheEnabled awsEnv mbGcsEnv
