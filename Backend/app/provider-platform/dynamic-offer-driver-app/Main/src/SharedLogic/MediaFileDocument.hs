-- Inspection media on the shared MediaFile table via a presigned flow (RC coupling + creator auth dropped; the
-- RC<->file link now happens per-request at inspection submit, not at upload):
--   uploadLink : create a MediaFile (PENDING), hand out a presigned S3 URL, schedule the cleanup job
--   client     : PUTs the bytes directly to S3, then calls confirm
--   confirm    : headRequest + size check; record the file hash (eTag) and mark CONFIRMED
--   submit     : finalizeInspectionMedia re-validates (must be CONFIRMED, still present, in-size, hash unchanged)
--                and marks COMPLETED -> confirm is mandatory and the file is re-checked before it is kept
--   cleanup job: COMPLETED (used by a submitted inspection) is kept; anything else (never confirmed, or confirmed
--                but never submitted) was abandoned -> delete the S3 object + the row
module SharedLogic.MediaFileDocument
  ( mediaFileDocumentUploadLink,
    mediaFileDocumentConfirm,
    mediaFileDocumentDelete,
    mediaFileDocumentDownloadLink,
    mediaFileDocumentComplete,
    finalizeInspectionMedia,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.MediaFileDocument as Common
import AWS.S3 as S3
import qualified Data.Text as T
import qualified Domain.Types.DocumentAuditLog as DAL
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Environment
import qualified IssueManagement.Domain.Types.MediaFile as DMF
import qualified IssueManagement.Storage.Queries.MediaFile as QMF
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import qualified SharedLogic.DriverOnboarding.Audit as Audit
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.Beam.IssueManagement ()
import Storage.Beam.SchedulerJob ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import Tools.Error

-- Create the MediaFile (PENDING), hand out a presigned upload URL, and schedule the completion job.
mediaFileDocumentUploadLink ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.UploadMediaFileDocumentReq ->
  Flow Common.MediaFileDocumentResp
mediaFileDocumentUploadLink merchantShortId opCity _requestorId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOpCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  fileExtension <- validateContentType req.fileType req.reqContentType
  fileId <- generateGUID
  filePath <- S3.createFilePath "/inspection-media/" fileId.getId req.fileType fileExtension
  uploadUrl <- S3.generateUploadUrl (T.unpack filePath) merchant.mediaFileDocumentLinkExpires
  now <- getCurrentTime
  let fileUrl =
        transporterConfig.mediaFileUrlPattern
          & T.replace "<DOMAIN>" "inspection"
          & T.replace "<FILE_PATH>" filePath
  QMF.create
    DMF.MediaFile
      { id = fileId,
        _type = req.fileType,
        url = fileUrl,
        s3FilePath = Just filePath,
        status = Just DMF.PENDING,
        fileHash = Nothing,
        createdAt = now,
        updatedAt = now
      }
  -- Fire the cleanup job well after the upload window. A real inspection (upload -> confirm -> fill -> submit)
  -- finishes long before then, so by the time it runs an un-submitted file = abandoned and is deleted from S3.
  let uploadLinkExpiredIn = fromIntegral merchant.mediaFileDocumentLinkExpires + 86400 -- ~1 day buffer
  createJobIn @_ @'MediaFileDocumentComplete (Just merchant.id) (Just merchantOpCityId) uploadLinkExpiredIn $
    MediaFileDocumentCompleteJobData {mediaFileId = fileId}
  -- Map onto the old response shape: mediaFileLink = presigned upload URL, mediaFileDocumentId = the MediaFile id.
  pure $
    Common.MediaFileDocumentResp
      { mediaFileLink = Just uploadUrl,
        mediaFileDocumentId = cast fileId,
        mediaFileDocumentStatus = Common.PENDING
      }

-- Verify the presigned upload landed + is within size; record its hash and mark CONFIRMED.
mediaFileDocumentConfirm ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.MediaFileDocumentReq ->
  Flow APISuccess
mediaFileDocumentConfirm merchantShortId opCity requestorId req = do
  let fileId = cast req.mediaFileDocumentId :: Id DMF.MediaFile
  externalServiceRateLimitOptions <- asks (.externalServiceRateLimitOptions)
  checkSlidingWindowLimitWithOptions (makeConfirmMediaHitsCountKey fileId) externalServiceRateLimitOptions
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  mediaFile <- QMF.findById fileId >>= fromMaybeM (InvalidRequest "MediaFile does not exist")
  -- Document audit: the dashboard actor confirming the upload. Role comes forwarded from the dashboard
  -- (audit-gated); when it isn't forwarded the audit degrades to SYSTEM rather than guessing a role.
  let auditActor = Audit.dashboardActorOrSystem (Just requestorId) req.requestorRole

  -- if status is PENDING, FAILED or CONFIRMED, we allow multiple reloads within rate limits, unless link will be expired
  when (mediaFile.status == Just DMF.DELETED) $
    throwError (InvalidRequest "MediaFile was deleted")
  when (mediaFile.status == Just DMF.COMPLETED) $
    throwError (InvalidRequest "MediaFile upload was already completed")

  s3Path <- mediaFile.s3FilePath & fromMaybeM (InvalidRequest "MediaFile has no s3 path")
  s3ObjectStatus <- catch (S3.headRequest (T.unpack s3Path)) $ \(err :: SomeException) -> do
    logError $ "File was not found: fileId: " <> fileId.getId <> " err: " <> show err
    QMF.updateStatusAndHashById DMF.FAILED Nothing fileId
    auditMediaFileStatus auditActor fileId (show <$> mediaFile.status) (Just "FAILED") DAL.STATUS_CHANGED (Just "File was not found") merchant.id merchantOpCityId
    throwError $ InvalidRequest "File was not found"

  let fileSizeInBytes = s3ObjectStatus.fileSizeInBytes
  let fileHash = S3.eTagToHash s3ObjectStatus.entityTag
  checkVideoFileSize (Just merchantOpCityId) fileSizeInBytes fileId >>= \case
    Right () -> pure ()
    Left errMessage -> do
      QMF.updateStatusAndHashById DMF.FAILED Nothing fileId
      auditMediaFileStatus auditActor fileId (show <$> mediaFile.status) (Just "FAILED") DAL.STATUS_CHANGED (Just errMessage) merchant.id merchantOpCityId
      catch (S3.delete (T.unpack s3Path)) $ \(err :: SomeException) -> logWarning $ "Unable to delete oversize file: " <> show err
      throwError $ InvalidRequest errMessage

  QMF.updateStatusAndHashById DMF.CONFIRMED (Just fileHash) fileId
  auditMediaFileStatus auditActor fileId Nothing (Just "CONFIRMED") DAL.UPLOADED Nothing merchant.id merchantOpCityId
  pure Success
  where
    makeConfirmMediaHitsCountKey :: Id DMF.MediaFile -> Text
    makeConfirmMediaHitsCountKey mediaFileId = "ConfirmMediaFile:Hits:" <> mediaFileId.getId <> ":hitsCount"

checkVideoFileSize ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r) =>
  Maybe (Id DMOC.MerchantOperatingCity) ->
  Integer ->
  Id DMF.MediaFile ->
  m (Either Text ())
checkVideoFileSize mbMerchantOpCityId fileSizeInBytes fileId = do
  mbTransporterConfig <- forM mbMerchantOpCityId $ \merchantOpCityId -> getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOpCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let maxSizeInMB = fromMaybe 500 $ mbTransporterConfig >>= (.maxAllowedVideoDocSizeInMB)
      maxSizeInBytes = toInteger maxSizeInMB * 1024 * 1024
  if fileSizeInBytes > maxSizeInBytes
    then do
      let errMessage = "File size " <> show fileSizeInBytes <> " bytes exceeds maximum limit of " <> show maxSizeInBytes <> " bytes (" <> show maxSizeInMB <> "MB)"
      logError $ "Upload failed: fileId: " <> fileId.getId <> " err: " <> errMessage
      pure $ Left errMessage
    else pure $ Right ()

finalizeInspectionMedia :: Id DMOC.MerchantOperatingCity -> Id DMF.MediaFile -> Flow ()
finalizeInspectionMedia merchantOpCityId fileId = do
  mediaFile <- QMF.findById fileId >>= fromMaybeM (InvalidRequest "Inspection media does not exist")
  unless (mediaFile.status `elem` [Just DMF.CONFIRMED, Just DMF.COMPLETED]) $
    throwError (InvalidRequest "Inspection media was not confirmed; please re-upload and confirm before submitting")
  s3Path <- mediaFile.s3FilePath & fromMaybeM (InvalidRequest "Inspection media has no s3 path")
  s3ObjectStatus <- catch (S3.headRequest (T.unpack s3Path)) $ \(err :: SomeException) -> do
    logError $ "Inspection media not found in S3 at submit: fileId: " <> fileId.getId <> " err: " <> show err
    QMF.updateStatusAndHashById DMF.FAILED Nothing fileId
    throwError (InvalidRequest "Inspection media was not found in S3")
  checkVideoFileSize (Just merchantOpCityId) s3ObjectStatus.fileSizeInBytes fileId >>= \case
    Right () -> pure ()
    Left errMessage -> do
      QMF.updateStatusAndHashById DMF.FAILED Nothing fileId
      catch (S3.delete (T.unpack s3Path)) $ \(err :: SomeException) -> logWarning $ "Unable to delete oversize media: " <> show err
      throwError (InvalidRequest errMessage)
  unless (Just (S3.eTagToHash s3ObjectStatus.entityTag) == mediaFile.fileHash) $
    throwError (InvalidRequest "Inspection media changed after confirmation; please re-upload and confirm")
  QMF.updateStatusById DMF.COMPLETED fileId
  -- Document audit: inspection media finalized (kept) at submit. No requestor is threaded through this shared
  -- finalization step, so it attributes to SYSTEM. merchantId is read (audit-gated) off the city, as the
  -- generic MediaFile row carries no merchant/city.
  mbMoc <- Audit.fetchForAuditByCity merchantOpCityId (CQMOC.findById merchantOpCityId)
  whenJust mbMoc $ \moc ->
    auditMediaFileStatus Audit.systemActor fileId (Just "CONFIRMED") (Just "COMPLETED") DAL.STATUS_CHANGED Nothing moc.merchantId merchantOpCityId

-- Delete a MediaFile. Within the upload window, soft-delete (status DELETED); after it, hard delete. Then S3 delete.
mediaFileDocumentDelete ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.MediaFileDocumentReq ->
  Flow APISuccess
mediaFileDocumentDelete merchantShortId opCity requestorId req = do
  let fileId = cast req.mediaFileDocumentId :: Id DMF.MediaFile
  mediaFile <- QMF.findById fileId >>= fromMaybeM (InvalidRequest "MediaFile does not exist")
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  let uploadLinkExpiredIn = fromIntegral merchant.mediaFileDocumentLinkExpires + 300 -- 5 minutes buffer
  now <- getCurrentTime
  if now > addUTCTime uploadLinkExpiredIn mediaFile.createdAt
    then QMF.deleteById fileId
    else QMF.updateStatusById DMF.DELETED fileId -- upload may be in progress, so only soft delete within the window
    -- Document audit: media-file deleted by the dashboard actor (role forwarded from the dashboard; SYSTEM when absent).
  auditMediaFileDelete (Audit.dashboardActorOrSystem (Just requestorId) req.requestorRole) fileId merchant.id merchantOpCityId
  whenJust mediaFile.s3FilePath $ \s3Path ->
    catch (S3.delete (T.unpack s3Path)) $ \(err :: SomeException) ->
      logWarning $ "Unable to delete file from S3: fileId: " <> fileId.getId <> " err: " <> show err
  pure Success

-- Resolve a MediaFile id to a presigned download URL.
mediaFileDocumentDownloadLink ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Text ->
  Flow Common.MediaFileDocumentResp
mediaFileDocumentDownloadLink merchantShortId _opCity fileId _requestorId = do
  merchant <- findMerchantByShortId merchantShortId
  mediaFile <- QMF.findById (Id fileId) >>= fromMaybeM (InvalidRequest "MediaFile does not exist")
  -- Same as the original flow: only hand out a download link once the file is actually uploaded (CONFIRMED/COMPLETED).
  let fileWasUploaded = mediaFile.status `elem` [Just DMF.CONFIRMED, Just DMF.COMPLETED]
  mbUrl <-
    if fileWasUploaded
      then forM mediaFile.s3FilePath $ \s3Path -> S3.generateDownloadUrl (T.unpack s3Path) merchant.mediaFileDocumentLinkExpires
      else pure Nothing
  -- Reuse the old response shape: mediaFileLink = presigned download URL, mediaFileDocumentStatus drives the
  -- dashboard's "uploaded yet?" polling.
  pure $
    Common.MediaFileDocumentResp
      { mediaFileLink = mbUrl,
        mediaFileDocumentId = cast mediaFile.id,
        mediaFileDocumentStatus = castMediaFileStatus mediaFile.status
      }

-- Cleanup job (fires well after the upload window). A file that a submitted inspection actually used is marked
-- COMPLETED (at submit) and kept; anything else -- never confirmed, or confirmed but never submitted -- was
-- abandoned, so it is deleted from S3 + the row.
mediaFileDocumentComplete ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r, HasField "s3Env" r (S3Env m), Audit.AuditFlow m r) =>
  Job 'MediaFileDocumentComplete ->
  m ExecutionResult
mediaFileDocumentComplete Job {id, jobInfo, merchantId = mbMerchantId, merchantOperatingCityId = mbMerchantOpCityId} = withLogTag ("JobId-" <> id.getId) $ do
  let mediaFileId = jobInfo.jobData.mediaFileId
  QMF.findById mediaFileId >>= \case
    Nothing -> logInfo $ "MediaFile: " <> mediaFileId.getId <> " already removed; nothing to clean"
    Just mediaFile ->
      case mediaFile.status of
        Just DMF.COMPLETED -> logInfo $ "MediaFile: " <> mediaFileId.getId <> " is used by an inspection; keeping"
        _ -> do
          logInfo $ "Removing abandoned MediaFile: " <> mediaFileId.getId <> " (status: " <> show mediaFile.status <> ")"
          QMF.deleteById mediaFileId
          -- Document audit: abandoned media-file removed by the cleanup scheduler. merchant/city come off the
          -- Job (the generic MediaFile row carries neither); skipped if the Job lacks them (row would be gate-dropped anyway).
          whenJust ((,) <$> mbMerchantId <*> mbMerchantOpCityId) $ \(mId, cId) ->
            auditMediaFileDelete Audit.systemScheduler mediaFileId mId cId
          whenJust mediaFile.s3FilePath $ \s3Path ->
            catch (S3.delete (T.unpack s3Path)) $ \(err :: SomeException) ->
              logWarning $ "Unable to delete abandoned file from S3: fileId: " <> mediaFileId.getId <> " err: " <> show err
  pure Complete

-- ─── Document audit (VehicleVideo / inspection media on the shared MediaFile table) ───
-- The generic MediaFile model dropped the RC↔file coupling the old audit keyed on (rcId /
-- mediaFileDocumentType), so the row is keyed on the MediaFile id itself: entity = VEHICLE + fileId,
-- documentType = "VehicleVideo", refType = MEDIA_FILE_DOCUMENT.
auditMediaFileStatus :: Audit.AuditFlow m r => Audit.Requestor -> Id DMF.MediaFile -> Maybe Text -> Maybe Text -> DAL.AuditAction -> Maybe Text -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m ()
auditMediaFileStatus requestor fileId =
  Audit.auditDocStatus requestor DAL.VEHICLE fileId.getId "VehicleVideo" DAL.MEDIA_FILE_DOCUMENT (Just fileId.getId)

auditMediaFileDelete :: Audit.AuditFlow m r => Audit.Requestor -> Id DMF.MediaFile -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m ()
auditMediaFileDelete requestor fileId =
  Audit.auditDelete requestor DAL.VEHICLE fileId.getId "VehicleVideo" DAL.MEDIA_FILE_DOCUMENT (Just fileId.getId) Nothing

-- Map the MediaFile upload status onto the dashboard-facing status enum (kept identical to the old flow).
castMediaFileStatus :: Maybe DMF.MediaFileUploadStatus -> Common.MediaFileDocumentStatus
castMediaFileStatus = \case
  Just DMF.PENDING -> Common.PENDING
  Just DMF.DELETED -> Common.DELETED
  Just DMF.FAILED -> Common.FAILED
  Just DMF.CONFIRMED -> Common.CONFIRMED
  Just DMF.COMPLETED -> Common.COMPLETED
  Nothing -> Common.PENDING

-- Video formats match the original media_file_document flow; Image formats are added for the new
-- driver-screenshot requirement (the old flow was video-only).
validateContentType :: S3.FileType -> Text -> Flow Text
validateContentType fileType reqContentType =
  case fileType of
    S3.Video -> case reqContentType of
      "video/mp4" -> pure "mp4"
      "video/x-msvideo" -> pure "avi"
      "video/mpeg" -> pure "mpeg"
      _ -> throwError $ FileFormatNotSupported reqContentType
    S3.Image -> case reqContentType of
      "image/png" -> pure "png"
      "image/jpeg" -> pure "jpg"
      _ -> throwError $ FileFormatNotSupported reqContentType
    _ -> throwError $ FileFormatNotSupported reqContentType
