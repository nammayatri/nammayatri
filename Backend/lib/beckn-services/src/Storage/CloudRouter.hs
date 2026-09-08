module Storage.CloudRouter
  ( buildCloudAwareEnv,
  )
where

import AWS.S3.Types
import qualified Data.Text as T
import EulerHS.Prelude hiding (show)
import Kernel.Types.Version (CloudType (..))
import Kernel.Utils.Common

buildCloudAwareEnv ::
  forall m.
  MonadFlow m =>
  CloudType ->
  Bool ->
  Bool ->
  S3Env m ->
  Maybe (S3Env m) ->
  S3Env m
buildCloudAwareEnv cloudType dualWriteEnabled recacheEnabled awsEnv mbGcsEnv =
  S3Env
    { pathPrefix = awsEnv.pathPrefix,
      getH = cloudAwareGet,
      putH = cloudAwarePut,
      putRawH = cloudAwarePutRaw,
      deleteH = cloudAwareDelete,
      generateUploadUrlH = awsEnv.generateUploadUrlH,
      generateDownloadUrlH = cloudAwareGenerateDownloadUrl,
      headRequestH = cloudAwareHeadRequest
    }
  where
    readsFromGcsFirst = cloudType == GCP

    cloudAwareGet path = case (readsFromGcsFirst, mbGcsEnv) of
      (True, Just gcsEnv) -> do
        result <- withTryCatch "cloudRouter:gcsGet" (getH gcsEnv path)
        case result of
          Right content -> pure content
          Left err -> do
            logWarning $ "[CloudStorage] GCS get failed for " <> T.pack path <> ", falling back to S3: " <> show err
            content <- getH awsEnv path
            recacheInGcs gcsEnv path content
            pure content
      _ -> getH awsEnv path

    recacheInGcs gcsEnv path content =
      when recacheEnabled $
        fork ("cloudRouter:gcsRecache:" <> T.pack path) $ do
          result <- withTryCatch "cloudRouter:gcsRecache" (putH gcsEnv path content)
          case result of
            Right () -> logInfo $ "[CloudStorage] GCS recache succeeded for " <> T.pack path
            Left err -> logWarning $ "[CloudStorage] GCS recache failed for " <> T.pack path <> ": " <> show err

    dualWriteToGcs :: Text -> String -> (S3Env m -> m ()) -> m ()
    dualWriteToGcs label path action =
      when dualWriteEnabled $
        case mbGcsEnv of
          Nothing -> pure ()
          Just gcsEnv ->
            fork ("cloudRouter:gcsDualWrite:" <> T.pack path) $ do
              result <- withTryCatch ("cloudRouter:gcsDualWrite:" <> label) (action gcsEnv)
              case result of
                Right () -> logInfo $ "[CloudStorage] GCS dual-write (" <> label <> ") succeeded for " <> T.pack path
                Left err -> logWarning $ "[CloudStorage] GCS dual-write (" <> label <> ") failed for " <> T.pack path <> ": " <> show err

    cloudAwarePut path content = do
      putH awsEnv path content
      dualWriteToGcs "put" path (\gcsEnv -> putH gcsEnv path content)

    cloudAwarePutRaw path bs contentType_ = do
      putRawH awsEnv path bs contentType_
      dualWriteToGcs "putRaw" path (\gcsEnv -> putRawH gcsEnv path bs contentType_)

    cloudAwareDelete path = do
      deleteH awsEnv path
      dualWriteToGcs "delete" path (\gcsEnv -> deleteH gcsEnv path)

    cloudAwareHeadRequest path = case (readsFromGcsFirst, mbGcsEnv) of
      (True, Just gcsEnv) -> do
        result <- withTryCatch "cloudRouter:gcsHead" (headRequestH gcsEnv path)
        case result of
          Right status -> pure status
          Left err -> do
            logWarning $ "[CloudStorage] GCS head failed for " <> T.pack path <> ", falling back to S3: " <> show err
            headRequestH awsEnv path
      _ -> headRequestH awsEnv path

    cloudAwareGenerateDownloadUrl path expires = case (readsFromGcsFirst, mbGcsEnv) of
      (True, Just gcsEnv) -> do
        result <- withTryCatch "cloudRouter:gcsHeadForUrl" (headRequestH gcsEnv path)
        case result of
          Right _ -> generateDownloadUrlH gcsEnv path expires
          Left err -> do
            logWarning $ "[CloudStorage] Object not found in GCS for " <> T.pack path <> ", generating S3 download URL instead: " <> show err
            generateDownloadUrlH awsEnv path expires
      _ -> generateDownloadUrlH awsEnv path expires
