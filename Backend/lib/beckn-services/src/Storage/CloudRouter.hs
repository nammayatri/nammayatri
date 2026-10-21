module Storage.CloudRouter
  ( buildCloudAwareEnv,
  )
where

import AWS.S3.Types
import qualified Data.Text as T
import EulerHS.Prelude hiding (show)
import Kernel.Types.Version (CloudType (..))
import Kernel.Utils.Common hiding (logInfo, logWarning)
import Kernel.Utils.Logging (logInfo, logWarning)

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
      getRawH = cloudAwareGetRaw,
      deleteH = cloudAwareDelete,
      generateUploadUrlH = awsEnv.generateUploadUrlH,
      generateDownloadUrlH = cloudAwareGenerateDownloadUrl,
      headRequestH = cloudAwareHeadRequest
    }
  where
    preferGcs = cloudType == GCP

    cloudAwareGet path = case (preferGcs, mbGcsEnv) of
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

    cloudAwareGetRaw path = case (preferGcs, mbGcsEnv) of
      (True, Just gcsEnv) -> do
        result <- withTryCatch "cloudRouter:gcsGetRaw" (getRawH gcsEnv path)
        case result of
          Right content -> pure content
          Left err -> do
            logWarning $ "[CloudStorage] GCS getRaw failed for " <> T.pack path <> ", falling back to S3: " <> show err
            content <- getRawH awsEnv path
            recacheRawInGcs gcsEnv path content
            pure content
      _ -> getRawH awsEnv path

    recacheInGcs gcsEnv path content =
      when recacheEnabled $
        fork ("cloudRouter:gcsRecache:" <> T.pack path) $ do
          result <- withTryCatch "cloudRouter:gcsRecache" (putH gcsEnv path content)
          case result of
            Right () -> logInfo $ "[CloudStorage] GCS recache succeeded for " <> T.pack path
            Left err -> logWarning $ "[CloudStorage] GCS recache failed for " <> T.pack path <> ": " <> show err

    recacheRawInGcs gcsEnv path content =
      when recacheEnabled $
        fork ("cloudRouter:gcsRecacheRaw:" <> T.pack path) $ do
          result <- withTryCatch "cloudRouter:gcsRecacheRaw" (putRawH gcsEnv path content "application/octet-stream")
          case result of
            Right () -> logInfo $ "[CloudStorage] GCS raw recache succeeded for " <> T.pack path
            Left err -> logWarning $ "[CloudStorage] GCS raw recache failed for " <> T.pack path <> ": " <> show err

    dualWriteToGcs :: Text -> String -> (S3Env m -> m ()) -> m ()
    dualWriteToGcs label path action =
      when dualWriteEnabled $
        case mbGcsEnv of
          Nothing -> pure ()
          Just gcsEnv ->
            do
              result <- withTryCatch ("cloudRouter:gcsDualWrite:" <> label) (action gcsEnv)
              case result of
                Right () -> logInfo $ "[CloudStorage] GCS dual-write (" <> label <> ") succeeded for " <> T.pack path
                Left err -> logWarning $ "[CloudStorage] GCS dual-write (" <> label <> ") failed for " <> T.pack path <> ": " <> show err

    writeBothClouds :: Text -> String -> (S3Env m -> m ()) -> m ()
    writeBothClouds label path action = case mbGcsEnv of
      Just gcsEnv | dualWriteEnabled && preferGcs -> do
        gcsResult <- withTryCatch ("cloudRouter:gcsWrite:" <> label) (action gcsEnv)
        awsResult <- withTryCatch ("cloudRouter:awsWrite:" <> label) (action awsEnv)
        case (gcsResult, awsResult) of
          (Right (), Right ()) -> logInfo $ "[CloudStorage] GCS and S3 write (" <> label <> ") succeeded for " <> T.pack path
          (Left gcsErr, Right ()) -> logWarning $ "[CloudStorage] GCS write (" <> label <> ") failed for " <> T.pack path <> ", S3 succeeded: " <> show gcsErr
          (Right (), Left awsErr) -> logWarning $ "[CloudStorage] S3 write (" <> label <> ") failed for " <> T.pack path <> ", GCS succeeded: " <> show awsErr
          (Left gcsErr, Left awsErr) -> do
            logWarning $ "[CloudStorage] GCS write (" <> label <> ") failed for " <> T.pack path <> ": " <> show gcsErr
            throwM awsErr
      _ -> do
        action awsEnv
        dualWriteToGcs label path action

    cloudAwarePut path content = writeBothClouds "put" path (\env -> putH env path content)

    cloudAwarePutRaw path bs contentType_ = writeBothClouds "putRaw" path (\env -> putRawH env path bs contentType_)

    cloudAwareDelete path = writeBothClouds "delete" path (\env -> deleteH env path)

    cloudAwareHeadRequest path = case (preferGcs, mbGcsEnv) of
      (True, Just gcsEnv) -> do
        result <- withTryCatch "cloudRouter:gcsHead" (headRequestH gcsEnv path)
        case result of
          Right status -> pure status
          Left err -> do
            logWarning $ "[CloudStorage] GCS head failed for " <> T.pack path <> ", falling back to S3: " <> show err
            headRequestH awsEnv path
      _ -> headRequestH awsEnv path

    cloudAwareGenerateDownloadUrl path expires = case (preferGcs, mbGcsEnv) of
      (True, Just gcsEnv) -> do
        result <- withTryCatch "cloudRouter:gcsHeadForUrl" (headRequestH gcsEnv path)
        case result of
          Right _ -> do
            urlResult <- withTryCatch "cloudRouter:gcsSignUrl" (generateDownloadUrlH gcsEnv path expires)
            case urlResult of
              Right url -> pure url
              Left err -> do
                logWarning $ "[CloudStorage] GCS signed URL generation failed for " <> T.pack path <> ", generating S3 download URL instead: " <> show err
                generateDownloadUrlH awsEnv path expires
          Left err -> do
            logWarning $ "[CloudStorage] Object not found in GCS for " <> T.pack path <> ", generating S3 download URL instead: " <> show err
            generateDownloadUrlH awsEnv path expires
      _ -> generateDownloadUrlH awsEnv path expires
