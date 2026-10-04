-- | Emailing one TDS certificate: used by the ScheduledTDSDistribution job (dashboard batches) and by the
-- dashboard's single-recipient retry. Every attempt is an email_delivery row whose id rides on the message as
-- the "emailDeliveryId" tag, so delivery and bounce events can later be matched back to the record.
module SharedLogic.TdsDistribution.Delivery
  ( sendRecordCertificate,
    TdsEmailSettings (..),
    getTdsEmailSettings,
    latestPdfFile,
    refreshBatchCompletion,
    isPendingStatus,
  )
where

import qualified AWS.S3 as S3
import Control.Applicative ((<|>))
import Data.List (sortOn)
import Data.Ord (Down (..))
import qualified Data.Text as T
import qualified Domain.Types.EmailDelivery as DED
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.TDSDistributionBatch as DTB
import qualified Domain.Types.TDSDistributionPdfFile as DTF
import Domain.Types.TDSDistributionRecord
import qualified Email.Flow as Email
import Email.Types (EmailServiceConfig)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified SharedLogic.EmailDelivery as EmailDelivery
import qualified SharedLogic.TdsDistribution as STD
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.EmailDelivery as QEmailDelivery
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.TDSDistributionBatch as QBatch
import qualified Storage.Queries.TDSDistributionPdfFile as QPdfFile
import qualified Storage.Queries.TDSDistributionRecord as QRecord

-- | Automatic attempts for a transient send failure before the record is marked FAILED.
maxAutoAttempts :: Int
maxAutoAttempts = 3

-- | Statuses that still have a send coming.
isPendingStatus :: TDSDistributionStatus -> Bool
isPendingStatus status = status `elem` [PENDING, SENDING]

data TdsEmailSettings = TdsEmailSettings
  { fromEmail :: Text,
    -- | SES configuration set publishing this city's delivery events; Nothing sends untracked by SES
    configurationSet :: Maybe Text
  }

getTdsEmailSettings :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m TdsEmailSettings
getTdsEmailSettings merchantOpCityId = do
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  fromEmail <- case transporterConfig.tdsFromEmail of
    Just fromEmail -> pure fromEmail
    Nothing -> do
      logWarning "tdsFromEmail not configured in TransporterConfig; using fallback noreply-tds@nammayatri.in"
      pure "noreply-tds@nammayatri.in"
  pure TdsEmailSettings {fromEmail, configurationSet = transporterConfig.tdsEmailConfigurationSet}

-- | The certificate to send for a record: the most recently uploaded PDF linked to it.
latestPdfFile :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Id TDSDistributionRecord -> m (Maybe DTF.TDSDistributionPdfFile)
latestPdfFile recordId = do
  files <- QPdfFile.findAllByTdsDistributionRecordId (Just recordId)
  pure $ listToMaybe $ sortOn (Down . (.createdAt)) files

-- | Email the record's certificate once and return the updated record.
--
-- The address is, in order: the override passed in (a dashboard retry), the address a previous retry saved on
-- the record, the person's profile email. A transient failure goes back to PENDING for the job to pick up
-- again when 'autoRetry' is set and attempts remain; otherwise the record ends FAILED with a reason.
-- Returns the record unchanged when another send for it is already in progress.
sendRecordCertificate ::
  ( CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    HasField "s3Env" r (S3.S3Env m),
    HasField "emailServiceConfig" r EmailServiceConfig
  ) =>
  TdsEmailSettings ->
  Maybe Text ->
  Maybe Text ->
  Bool ->
  TDSDistributionRecord ->
  m TDSDistributionRecord
sendRecordCertificate settings mbOverrideEmail mbTriggeredBy autoRetry staleRecord = do
  let lockKey = "TdsDistribution:Record:Send:" <> staleRecord.id.getId
  gotLock <- Redis.tryLockRedis lockKey 120
  if not gotLock
    then do
      logInfo $ "TDS record " <> staleRecord.id.getId <> " is already being sent; skipping"
      pure staleRecord
    else do
      result <- try @_ @SomeException $ do
        record <- QRecord.findById staleRecord.id >>= fromMaybeM (InvalidRequest "TDS record not found")
        sendLocked record
      Redis.unlockRedis lockKey
      case result of
        Right record -> pure record
        Left err -> throwError (InternalError $ "TDS certificate send failed for record " <> staleRecord.id.getId <> ": " <> show err)
  where
    sendLocked record = do
      now <- getCurrentTime
      mbPdfFile <- latestPdfFile record.id
      mbPerson <- maybe (pure Nothing) QPerson.findById record.driverId
      let mbEmail = mfilter (not . T.null) . fmap T.strip $ mbOverrideEmail <|> record.emailAddress <|> (mbPerson >>= (.email))
      case (mbPdfFile, mbEmail) of
        (Nothing, _) -> do
          logWarning $ "No PDF file for TDS record " <> record.id.getId
          save record {status = MISSING_FILE, updatedAt = now}
        (_, Nothing) ->
          save record {status = FAILED, failureReason = Just MISSING_EMAIL, updatedAt = now}
        (Just pdfFile, Just email) -> sendAttempt record pdfFile mbPerson email now

    sendAttempt record pdfFile mbPerson email now = do
      deliveryId <- generateGUID
      QEmailDelivery.create
        DED.EmailDelivery
          { id = deliveryId,
            merchantId = record.merchantId,
            merchantOperatingCityId = record.merchantOperatingCityId,
            ownerType = DED.TDS_RECORD,
            ownerId = record.id.getId,
            toAddress = email,
            provider = Nothing,
            providerMessageId = Nothing,
            status = DED.SENDING,
            failureReason = Nothing,
            bounceType = Nothing,
            bounceSubType = Nothing,
            triggeredBy = mbTriggeredBy,
            sentAt = Nothing,
            deliveredAt = Nothing,
            lastEventAt = Nothing,
            createdAt = now,
            updatedAt = now
          }
      let attempting =
            record
              { status = SENDING,
                failureReason = Nothing,
                emailAddress = mbOverrideEmail <|> record.emailAddress,
                latestEmailDeliveryId = Just deliveryId,
                attemptCount = Just (fromMaybe 0 record.attemptCount + 1),
                lastAttemptAt = Just now,
                updatedAt = now
              }
      QRecord.updateByPrimaryKey attempting
      downloadUrl <- S3.generateDownloadUrl (T.unpack pdfFile.s3FilePath) (Seconds 300)
      let (subject, body) = certificateEmail attempting pdfFile.recipientType (STD.personDisplayName <$> mbPerson)
      sendResult <-
        try @_ @SomeException $
          EmailDelivery.sendEmail
            EmailDelivery.EmailRequest
              { from = settings.fromEmail,
                to = [email],
                subject,
                body,
                bodyFormat = Email.Text,
                attachments = [EmailDelivery.EmailAttachmentRef {url = downloadUrl, filename = pdfFile.fileName, contentType = Just "application/pdf"}],
                options = Email.EmailSendOptions {configurationSet = settings.configurationSet, tags = [("emailDeliveryId", deliveryId.getId)]}
              }
      sentAt <- getCurrentTime
      case sendResult of
        Right mbProviderResult -> do
          -- A delivery event can land before this point (SES delivers in seconds); never move a row back to SENT.
          mbDelivery <- QEmailDelivery.findById deliveryId
          let provider = toDeliveryProvider . (.provider) <$> mbProviderResult
              providerMessageId = mbProviderResult >>= (.messageId)
          case mbDelivery of
            Just delivery
              | delivery.status == DED.SENDING -> QEmailDelivery.updateSent DED.SENT provider providerMessageId (Just sentAt) deliveryId
            _ -> QEmailDelivery.updateProviderMessage provider providerMessageId (Just sentAt) deliveryId
          current <- QRecord.findById attempting.id
          case current of
            Just fresh | fresh.status /= SENDING -> pure fresh
            _ -> save attempting {status = SENT, updatedAt = sentAt}
        Left err -> do
          let errText = show err
              reason = classifySendFailure errText
              retryable = autoRetry && reason `elem` [TIMEOUT, SEND_ERROR] && attempting.retryCount + 1 < maxAutoAttempts
          logError $ "TDS certificate send failed for record " <> record.id.getId <> ": " <> errText
          QEmailDelivery.updateFailed DED.FAILED (Just errText) deliveryId
          save $
            if retryable
              then attempting {status = PENDING, retryCount = attempting.retryCount + 1, failureReason = Just reason, updatedAt = sentAt}
              else attempting {status = FAILED, failureReason = Just reason, updatedAt = sentAt}

    save record = record <$ QRecord.updateByPrimaryKey record

    toDeliveryProvider = \case
      Email.SES -> DED.SES
      Email.SENDGRID -> DED.SENDGRID

-- | Send-time failures; bounces (mailbox full, address not found) arrive later as provider events.
classifySendFailure :: Text -> TDSFailureReason
classifySendFailure err
  | "Attachment exceeds" `T.isInfixOf` err = ATTACHMENT_TOO_LARGE
  | "MessageRejected" `T.isInfixOf` err = REJECTED
  | any (`T.isInfixOf` lowered) ["timeout", "timed out", "throttl"] = TIMEOUT
  | otherwise = SEND_ERROR
  where
    lowered = T.toLower err

certificateEmail :: TDSDistributionRecord -> Maybe DTF.TDSRecipientType -> Maybe Text -> (Text, Text)
certificateEmail record mbRecipientType mbName = (subject, body)
  where
    period = case record.financialYear of
      Just financialYear -> record.quarter <> " FY " <> financialYear
      Nothing -> record.quarter <> " AY " <> record.assessmentYear
    subject = "TDS certificate (Form 16A) - " <> period
    greeting = fromMaybe (if mbRecipientType == Just DTF.FLEET_OWNER then "Fleet Owner" else "Driver") mbName
    body =
      "Dear "
        <> greeting
        <> ",\n\n"
        <> "Please find attached your TDS certificate (Form 16A) for "
        <> period
        <> ".\n\n"
        <> "This is a system-generated email. Please do not reply.\n\n"
        <> "Regards,\nNammayatri"

-- | Mark a SENDING batch COMPLETED once none of its records still has a send coming.
refreshBatchCompletion :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Id DTB.TDSDistributionBatch -> m ()
refreshBatchCompletion batchId = do
  mbBatch <- QBatch.findById batchId
  whenJust mbBatch $ \batch -> when (batch.status == DTB.SENDING) $ do
    records <- QRecord.findAllByBatchId (Just batchId)
    unless (any (isPendingStatus . (.status)) records) $ do
      now <- getCurrentTime
      QBatch.updateStatusAndCompletedAt DTB.COMPLETED (Just now) batchId
