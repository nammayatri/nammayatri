{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.TDSDistribution.ScheduledTDSDistribution where

import qualified AWS.S3 as S3
import Data.List.Split (chunksOf)
import qualified Data.Map as M
import qualified Data.Text as T
import Domain.Types.TDSDistributionBatch (TDSDistributionBatch)
import Domain.Types.TDSDistributionPdfFile (TDSDistributionPdfFile)
import Domain.Types.TDSDistributionRecord
import Domain.Utils (mapConcurrently)
import qualified Email.Flow as Email
import Email.Types (EmailServiceConfig)
import Kernel.Prelude
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import qualified SharedLogic.EmailDelivery as EmailDelivery
import qualified SharedLogic.TdsDistribution.Delivery as Delivery
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.TDSDistributionPdfFile as QPdfFile
import qualified Storage.Queries.TDSDistributionRecord as QTDS
import qualified Storage.Queries.TDSDistributionRecordExtra as QTDSExtra

-- | Reschedule interval: 24 hours
tdsRescheduleInterval :: NominalDiffTime
tdsRescheduleInterval = 24 * 60 * 60

-- | Default batch size
defaultBatchSize :: Int
defaultBatchSize = 1000

-- | Maximum retry attempts before marking as FAILED
tdsMaxRetries :: Int
tdsMaxRetries = 3

-- | Records sent per run of a dashboard batch; the job reschedules itself until the batch has none pending.
batchPageSize :: Int
batchPageSize = 50

-- | Certificates emailed in parallel within a page.
batchSendParallelism :: Int
batchSendParallelism = 10

-- | Main scheduler job handler for TDS certificate distribution.
-- With a batchId (started by the dashboard's confirm / retry): emails that upload's pending records page by page,
-- then marks the batch completed. Without one: the daily sweep of legacy PENDING records (12:00 AM IST).
scheduledTDSDistribution ::
  ( CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    HasField "s3Env" r (S3.S3Env m),
    HasField "emailServiceConfig" r EmailServiceConfig,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "blackListedJobs" r [Text]
  ) =>
  Job 'ScheduledTDSDistribution ->
  m ExecutionResult
scheduledTDSDistribution Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
  settings <- Delivery.getTdsEmailSettings jobData.merchantOperatingCityId
  case jobData.batchId of
    Just batchId -> sendBatchPage settings batchId
    Nothing -> legacySweep settings.fromEmail jobData

sendBatchPage ::
  ( CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    HasField "s3Env" r (S3.S3Env m),
    HasField "emailServiceConfig" r EmailServiceConfig
  ) =>
  Delivery.TdsEmailSettings ->
  Id TDSDistributionBatch ->
  m ExecutionResult
sendBatchPage settings batchId = withLogTag ("TdsBatch-" <> batchId.getId) do
  records <- QTDSExtra.findAllByBatchIdAndStatusesWithLimit batchId [PENDING] batchPageSize
  if null records
    then do
      Delivery.refreshBatchCompletion batchId
      logInfo "TDS batch has no pending records left"
      pure Complete
    else do
      logInfo $ "Sending " <> show (length records) <> " TDS certificates"
      forM_ (chunksOf batchSendParallelism records) $ \chunk ->
        void . flip mapConcurrently chunk $ \record -> do
          result <- try @_ @SomeException $ Delivery.sendRecordCertificate settings Nothing Nothing True record
          case result of
            Left err -> logError $ "TDS certificate send failed for record " <> record.id.getId <> ": " <> show err
            Right _ -> pure ()
      now <- getCurrentTime
      pure $ ReSchedule (addUTCTime 2 now)

legacySweep ::
  ( CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    HasField "s3Env" r (S3.S3Env m),
    HasField "emailServiceConfig" r EmailServiceConfig,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "blackListedJobs" r [Text]
  ) =>
  Text ->
  ScheduledTDSDistributionJobData ->
  m ExecutionResult
legacySweep fromEmail jobData = do
  let merchantId = jobData.merchantId
      opCityId = jobData.merchantOperatingCityId
      batchSize = fromMaybe defaultBatchSize jobData.batchSize

  logInfo $ "Starting TDS Distribution job for merchant: " <> merchantId.getId

  -- Fetch PENDING records scoped to this merchant operating city; dashboard batches are sent by their own job
  records <- filter (isNothing . (.batchId)) <$> QTDSExtra.findAllByStatusWithLimit (Just batchSize) Nothing opCityId PENDING
  logInfo $ "Found " <> show (length records) <> " PENDING TDS records"

  -- Process each record
  forM_ records $ \record -> do
    result <- try @_ @SomeException $ processRecord fromEmail record
    case result of
      Left e -> do
        logError $ "Error processing TDS record " <> record.id.getId <> ": " <> show e
        let newCount = record.retryCount + 1
        if newCount >= tdsMaxRetries
          then QTDS.updateStatusAndRetryCount FAILED newCount record.id
          else QTDS.updateStatusAndRetryCount PENDING newCount record.id
      Right () -> pure ()

  -- Reschedule for next day
  createJobIn @_ @'ScheduledTDSDistribution (Just merchantId) (Just opCityId) tdsRescheduleInterval $
    ScheduledTDSDistributionJobData
      { merchantId = merchantId,
        merchantOperatingCityId = opCityId,
        batchSize = jobData.batchSize,
        batchId = Nothing
      }

  logInfo "TDS Distribution job completed successfully"
  return Complete

-- | Process a single TDS distribution record
processRecord ::
  ( CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    HasField "s3Env" r (S3.S3Env m),
    HasField "emailServiceConfig" r EmailServiceConfig
  ) =>
  Text ->
  TDSDistributionRecord ->
  m ()
processRecord fromEmail record = do
  -- Step 1: Look up PDF files from tds_distribution_pdf_file table
  pdfFiles <- QPdfFile.findAllByTdsDistributionRecordId (Just record.id)
  case pdfFiles of
    [] -> do
      logWarning $ "No PDF file found for TDS record " <> record.id.getId
      QTDS.updateStatus MISSING_FILE record.id
    _ -> do
      -- Step 2: Resolve driver email from person table
      driverEmail <- getDriverEmail record
      case driverEmail of
        Nothing -> do
          logWarning $ "No email found for TDS record " <> record.id.getId
          QTDS.updateStatus FAILED record.id
        Just email -> do
          -- Step 3: Download each PDF from S3 and send email
          when (length pdfFiles > 1) $
            logInfo $ "Multiple PDF files (" <> show (length pdfFiles) <> ") found for TDS record " <> record.id.getId <> "; sending all"
          forM_ pdfFiles $ \pdfFile ->
            sendTDSCertificate fromEmail record pdfFile email
          QTDS.updateStatus SENT record.id

-- | Get driver email: use record's emailAddress if present, otherwise look up from person table
getDriverEmail ::
  ( CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r
  ) =>
  TDSDistributionRecord ->
  m (Maybe Text)
getDriverEmail record = case record.emailAddress of
  Just email -> pure (Just email)
  Nothing -> case record.driverId of
    Nothing -> pure Nothing
    Just driverId -> do
      mbPerson <- QPerson.findById driverId
      pure $ mbPerson >>= (.email)

-- | Email the PDF to the recipient; the shared email path downloads it from a short-lived pre-signed URL.
sendTDSCertificate ::
  ( CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    HasField "s3Env" r (S3.S3Env m),
    HasField "emailServiceConfig" r EmailServiceConfig
  ) =>
  Text ->
  TDSDistributionRecord ->
  TDSDistributionPdfFile ->
  Text ->
  m ()
sendTDSCertificate fromEmail record pdfFile recipientEmail = do
  logInfo $ "Sending TDS certificate to " <> recipientEmail <> " for " <> record.quarter <> " " <> record.assessmentYear

  downloadUrl <- S3.generateDownloadUrl (T.unpack pdfFile.s3FilePath) (Seconds 300)
  let subject = "TDS Certificate for " <> record.quarter <> " - " <> record.assessmentYear
      body =
        "Dear Driver,\n\n"
          <> "Please find attached your TDS Certificate for "
          <> record.quarter
          <> " of Assessment Year "
          <> record.assessmentYear
          <> ".\n\n"
          <> "This is a system-generated email. Please do not reply.\n\n"
          <> "Regards,\nNammayatri"
  void $
    EmailDelivery.sendEmail
      EmailDelivery.EmailRequest
        { from = fromEmail,
          to = [recipientEmail],
          subject,
          body,
          bodyFormat = Email.Text,
          attachments = [EmailDelivery.EmailAttachmentRef {url = downloadUrl, filename = pdfFile.fileName, contentType = Just "application/pdf"}],
          options = Email.noEmailSendOptions
        }

  logInfo $ "Successfully sent TDS certificate to " <> recipientEmail
