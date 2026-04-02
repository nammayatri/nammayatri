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
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import qualified Data.Text as T
import Domain.Types.TDSDistributionPdfFile (TDSDistributionPdfFile)
import Domain.Types.TDSDistributionRecord
import qualified Email.Flow as Email
import Email.Types (EmailServiceConfig)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import SharedLogic.Allocator
import Storage.Beam.SchedulerJob ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.TDSDistributionPdfFile as QPdfFile
import qualified Storage.Queries.TDSDistributionRecord as QTDS
import qualified Storage.Queries.TDSDistributionRecordExtra as QTDSExtra
import System.Directory (removeFile)
import System.IO.Temp (emptySystemTempFile)

-- | Reschedule interval: 24 hours
tdsRescheduleInterval :: NominalDiffTime
tdsRescheduleInterval = 24 * 60 * 60

-- | Default batch size
defaultBatchSize :: Int
defaultBatchSize = 1000

-- | Maximum retry attempts before marking as FAILED
tdsMaxRetries :: Int
tdsMaxRetries = 3

-- | Main scheduler job handler for TDS certificate distribution
-- Runs daily at 12:00 AM IST, fetches PENDING records, sends PDFs via email
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
      merchantId = jobData.merchantId
      opCityId = jobData.merchantOperatingCityId
      batchSize = fromMaybe defaultBatchSize jobData.batchSize

  logInfo $ "Starting TDS Distribution job for merchant: " <> merchantId.getId

  -- Fetch tdsFromEmail from TransporterConfig
  transporterConfig <- SCTC.findByMerchantOpCityId opCityId Nothing >>= fromMaybeM (TransporterConfigNotFound opCityId.getId)
  fromEmail <- case transporterConfig.tdsFromEmail of
    Just e -> pure e
    Nothing -> do
      logWarning "tdsFromEmail not configured in TransporterConfig; using fallback noreply-tds@nammayatri.in"
      pure "noreply-tds@nammayatri.in"

  -- Fetch PENDING records scoped to this merchant operating city
  records <- QTDSExtra.findAllByStatusWithLimit (Just batchSize) Nothing opCityId PENDING
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
        batchSize = jobData.batchSize
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

-- | Download PDF from S3 via pre-signed URL and send as email attachment
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

  -- Generate pre-signed URL and download PDF as binary
  let s3Path = T.unpack pdfFile.s3FilePath
  downloadUrl <- S3.generateDownloadUrl s3Path (Seconds 300)
  tempFile <- liftIO $ emptySystemTempFile "tds_certificate_.pdf"
  liftIO $ do
    manager <- TLS.newTlsManager
    request <- HTTP.parseRequest (T.unpack downloadUrl)
    response <- HTTP.httpLbs request manager
    LBS.writeFile tempFile (HTTP.responseBody response)

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
      attachmentName = pdfFile.fileName

  emailServiceConfig <- asks (.emailServiceConfig)
  liftIO $
    Email.sendEmailWithAttachment
      emailServiceConfig
      fromEmail
      [recipientEmail]
      subject
      body
      tempFile
      attachmentName
      `finally` removeFile tempFile

  logInfo $ "Successfully sent TDS certificate to " <> recipientEmail
