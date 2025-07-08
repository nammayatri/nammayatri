module SharedLogic.MediaFileDocument
  ( mediaFileDocumentUploadLink,
    mediaFileDocumentConfirm,
    mediaFileDocumentDelete,
    mediaFileDocumentDownloadLink,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.DriverRegistration as Common
import AWS.S3 as S3
import qualified "dashboard-helper-api" Dashboard.Common.MediaFileDocument as CommonMFD
import qualified Data.Text as T
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Domain.Types.Common as DCommon
import qualified Domain.Types.MediaFileDocument as DMFD
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.VehicleRegistrationCertificate as DRC
import Environment
import EulerHS.Prelude hiding (elem, find, foldl', length, map, whenJust)
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.Cac.TransporterConfig as CCT
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.MediaFileDocument as QMFD
import qualified Storage.Queries.VehicleRegistrationCertificate as QRC
import Tools.Error

mediaFileDocumentUploadLink ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.UploadMediaFileDocumentReq ->
  Flow Common.MediaFileDocumentResp
mediaFileDocumentUploadLink merchantShortId opCity requestorId req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.getMerchantOpCity merchant (Just opCity)
  fileExtension <- validateContentType

  rc <- QRC.findLastVehicleRCWrapper req.rcNumber >>= fromMaybeM (RCNotFound req.rcNumber)
  let mediaFileDocumentType = castMediaFileDocumentType req.mediaFileDocumentType
  -- if file was deleted by operator, then allow to create new one
  existingMediaFiles <- filter (\mfd -> mfd.status /= DMFD.DELETED) <$> QMFD.findAllByMerchantOpCityIdAndRcIdAndType merchantOpCity.id rc.id mediaFileDocumentType

  when (length existingMediaFiles > 1) $ do
    logError $
      "Duplicated MediaFileDocuments found: merchantOpCityId: " <> merchantOpCity.id.getId
        <> "; rcId: "
        <> rc.id.getId
        <> "; mediaFileDocumentType: "
        <> show mediaFileDocumentType
        <> ": "
        <> show (length existingMediaFiles)
        <> " times"

  mediaFileDocument <- case listToMaybe existingMediaFiles of
    Nothing -> do
      mediaPath <- createMediaPathByRcId merchantOpCity.id rc.id mediaFileDocumentType fileExtension
      mediaFileLink <- S3.generateUploadUrl (T.unpack mediaPath) merchant.mediaFileDocumentLinkExpires
      newMediaFileDocument <- buildMediaFileDocument merchantOpCity (Id @DP.Person requestorId) mediaPath rc.id mediaFileLink req
      QMFD.create newMediaFileDocument
      pure newMediaFileDocument
    Just existingMediaFile -> do
      if existingMediaFile.creatorId == Id @DP.Person requestorId
        then pure existingMediaFile
        else pure existingMediaFile{uploadLink = Nothing} -- we can show upload link only to creator to avoid uploading by two users in the same time
  pure $
    Common.MediaFileDocumentResp
      { mediaFileLink = mediaFileDocument.uploadLink,
        mediaFileDocumentId = cast @DMFD.MediaFileDocument @CommonMFD.MediaFileDocument mediaFileDocument.id,
        mediaFileDocumentStatus = castMediaFileDocumentStatus mediaFileDocument.status
      }
  where
    validateContentType = do
      case req.fileType of
        S3.Video -> case req.reqContentType of
          "video/mp4" -> pure "mp4"
          "video/x-msvideo" -> pure "avi"
          "video/mpeg" -> pure "mpeg"
          _ -> throwError $ FileFormatNotSupported req.reqContentType
        _ -> throwError $ FileFormatNotSupported req.reqContentType

castMediaFileDocumentType :: Common.MediaFileDocumentType -> DCommon.MediaFileDocumentType
castMediaFileDocumentType = \case
  Common.VehicleVideo -> DCommon.VehicleVideo

castMediaFileDocumentStatus :: DMFD.MediaFileDocumentStatus -> CommonMFD.MediaFileDocumentStatus
castMediaFileDocumentStatus = \case
  DMFD.PENDING -> CommonMFD.PENDING
  DMFD.DELETED -> CommonMFD.DELETED
  DMFD.FAILED -> CommonMFD.FAILED
  DMFD.CONFIRMED -> CommonMFD.CONFIRMED
  DMFD.COMPLETED -> CommonMFD.COMPLETED

createMediaPathByRcId ::
  (MonadTime m, MonadReader r m, HasField "s3Env" r (S3Env m)) =>
  Id DMOC.MerchantOperatingCity ->
  Id DRC.VehicleRegistrationCertificate ->
  DCommon.MediaFileDocumentType ->
  Text ->
  m Text
createMediaPathByRcId merchantOpCityId rcId mediaFileDocumentType extension = do
  pathPrefix <- asks (.s3Env.pathPrefix)
  now <- getCurrentTime
  let fileName = T.replace (T.singleton ':') (T.singleton '-') (T.pack $ iso8601Show now)
  return
    ( pathPrefix <> "/driver-onboarding/mocId-" <> merchantOpCityId.getId
        <> "/rcId-"
        <> rcId.getId
        <> "/media/"
        <> show mediaFileDocumentType
        <> "/"
        <> fileName
        <> "."
        <> extension
    )

buildMediaFileDocument ::
  MonadFlow m =>
  DMOC.MerchantOperatingCity ->
  Id DP.Person ->
  Text ->
  Id DRC.VehicleRegistrationCertificate ->
  Text ->
  Common.UploadMediaFileDocumentReq ->
  m DMFD.MediaFileDocument
buildMediaFileDocument merchantOpCity creatorId s3Path rcId mediaFileLink Common.UploadMediaFileDocumentReq {..} = do
  uuid <- generateGUID
  now <- getCurrentTime
  pure
    DMFD.MediaFileDocument
      { id = Id uuid,
        creatorId,
        mediaFileDocumentType = castMediaFileDocumentType mediaFileDocumentType,
        merchantId = merchantOpCity.merchantId,
        merchantOperatingCityId = merchantOpCity.id,
        rcId,
        s3Path,
        uploadLink = Just mediaFileLink,
        status = DMFD.PENDING, -- TODO migrate CONFIRMED ==> COMPLETED
        fileHash = Nothing,
        createdAt = now,
        updatedAt = now
      }

mediaFileDocumentConfirm ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.MediaFileDocumentReq ->
  Flow APISuccess
mediaFileDocumentConfirm merchantShortId opCity requestorId req = do
  let mediaFileDocumentId = cast @CommonMFD.MediaFileDocument @DMFD.MediaFileDocument req.mediaFileDocumentId
  externalServiceRateLimitOptions <- asks (.externalServiceRateLimitOptions)
  checkSlidingWindowLimitWithOptions (makeConfirmMediaDocHitsCountKey mediaFileDocumentId) externalServiceRateLimitOptions
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just opCity)
  mediaFileDocument <- QMFD.findByPrimaryKey mediaFileDocumentId >>= fromMaybeM (InvalidRequest "MediaFileDocument does not exist")
  unless (Id @DP.Person requestorId == mediaFileDocument.creatorId) $ throwError AccessDenied

  -- if status is PENDING, FAILED or CONFIRMED, we allow multiple reloads within rate limits, unless link will be expired
  when (mediaFileDocument.status == DMFD.DELETED) $
    throwError (InvalidRequest "MediaFileDocument was deleted")
  when (mediaFileDocument.status == DMFD.COMPLETED) $
    throwError (InvalidRequest "MediaFileDocument download was already completed")

  let s3Path = T.unpack mediaFileDocument.s3Path
  s3ObjectStatus <- catch (S3.headRequest s3Path) $ \(err :: SomeException) -> do
    logError $ "File was not found: mediaFileDocumentId: " <> mediaFileDocumentId.getId <> "err: " <> show err
    QMFD.updateStatus DMFD.FAILED Nothing mediaFileDocumentId
    throwError $ InvalidRequest "File was not found"

  let fileSizeInBytes = s3ObjectStatus.fileSizeInBytes
  let fileHash = S3.eTagToHash s3ObjectStatus.entityTag
  transporterConfig <- CCT.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let maxSizeInMB = fromMaybe 500 transporterConfig.maxAllowedVideoDocSizeInMB
      maxSizeInBytes = toInteger maxSizeInMB * 1024 * 1024
  when (fileSizeInBytes > maxSizeInBytes) $ do
    let message = "Video size " <> show fileSizeInBytes <> " bytes exceeds maximum limit of " <> show maxSizeInBytes <> " bytes (" <> show maxSizeInMB <> "MB)"
    logError $ "Video upload failed: mediaFileDocumentId: " <> mediaFileDocumentId.getId <> "err: " <> show message
    QMFD.updateStatus DMFD.FAILED Nothing mediaFileDocumentId
    S3.delete s3Path
    throwError $ InvalidRequest message

  QMFD.updateStatus DMFD.CONFIRMED (Just fileHash) mediaFileDocumentId
  pure Success
  where
    makeConfirmMediaDocHitsCountKey :: Id DMFD.MediaFileDocument -> Text
    makeConfirmMediaDocHitsCountKey mediaFileDocumentId = "ConfirmMediaDoc:MediaDocHits:" <> mediaFileDocumentId.getId <> ":hitsCount"

mediaFileDocumentDelete ::
  ShortId DM.Merchant ->
  Context.City ->
  Text ->
  Common.MediaFileDocumentReq ->
  Flow APISuccess
mediaFileDocumentDelete _merchantShortId _opCity _requestorId req = do
  let mediaFileDocumentId = cast @CommonMFD.MediaFileDocument @DMFD.MediaFileDocument req.mediaFileDocumentId
  mediaFileDocument <- QMFD.findByPrimaryKey mediaFileDocumentId >>= fromMaybeM (InvalidRequest "MediaFileDocument does not exist")
  QMFD.updateStatus DMFD.DELETED Nothing mediaFileDocumentId
  let s3Path = T.unpack mediaFileDocument.s3Path
  catch (S3.delete s3Path) $ \(err :: SomeException) -> do
    logError $ "Unable to delete file: mediaFileDocumentId: " <> mediaFileDocumentId.getId <> "err: " <> show err
  pure Success

mediaFileDocumentDownloadLink ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.MediaFileDocumentType ->
  Text ->
  Text ->
  Flow Common.MediaFileDocumentResp
mediaFileDocumentDownloadLink merchantShortId opCity mediaFileDocumentType' rcNumber _requestorId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.getMerchantOpCity merchant (Just opCity)
  rc <- QRC.findLastVehicleRCWrapper rcNumber >>= fromMaybeM (RCNotFound rcNumber)
  let mediaFileDocumentType = castMediaFileDocumentType mediaFileDocumentType'
  mediaFileDocuments <- filter (\mfd -> mfd.status /= DMFD.DELETED) <$> QMFD.findAllByMerchantOpCityIdAndRcIdAndType merchantOpCity.id rc.id mediaFileDocumentType

  when (length mediaFileDocuments > 1) $ do
    logError $
      "Duplicated MediaFileDocuments found: merchantOpCityId: " <> merchantOpCity.id.getId
        <> "; rcId: "
        <> rc.id.getId
        <> "; mediaFileDocumentType: "
        <> show mediaFileDocumentType
        <> ": "
        <> show (length mediaFileDocuments)
        <> " times"

  mediaFileDocument <- listToMaybe mediaFileDocuments & fromMaybeM (InvalidRequest "MediaFileDocument does not exist")

  mbMediaFileLink <- case mediaFileDocument.status of
    mediaFileDocumentStatus | mediaFileDocumentStatus `elem` [DMFD.CONFIRMED, DMFD.COMPLETED] -> do
      Just <$> S3.generateDownloadUrl (T.unpack mediaFileDocument.s3Path) merchant.mediaFileDocumentLinkExpires
    _ -> pure Nothing -- DMFD.PENDING, DMFD.FAILED
  pure $
    Common.MediaFileDocumentResp
      { mediaFileLink = mbMediaFileLink,
        mediaFileDocumentId = cast @DMFD.MediaFileDocument @CommonMFD.MediaFileDocument mediaFileDocument.id,
        mediaFileDocumentStatus = castMediaFileDocumentStatus mediaFileDocument.status
      }
