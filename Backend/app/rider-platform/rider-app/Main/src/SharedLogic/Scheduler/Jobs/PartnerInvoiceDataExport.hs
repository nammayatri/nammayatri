{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.PartnerInvoiceDataExport where

import Control.Exception (try)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Char (isAlphaNum)
import qualified Domain.Types.PartnerInvoiceDataLog as DPIL
import Environment (SFTPConfig (..))
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude hiding (hPutStr)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Utils.Common hiding (HasField)
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.JobScheduler
import Storage.Beam.SchedulerJob ()
import qualified Storage.Queries.PartnerInvoiceDataLog as QPartnerInvoiceDataLog
import System.Directory (removeFile)
import System.IO (hClose, hPutStr, openTempFile)
import System.Process (callProcess)
import Control.Exception (try)

-- | Data structure for JSON export
data ExportRecord = ExportRecord
  { bookingId :: Text,
    personId :: Text,
    requestedAt :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data ExportPayload = ExportPayload
  { exportDate :: UTCTime,
    records :: [ExportRecord]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- | The scheduled job handler for partner invoice data SFTP export
partnerInvoiceDataExportJob ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    SchedulerFlow r,
    EsqDBReplicaFlow m r,
    ServiceFlow m r,
    JobCreator r m,
    HasField "sftpConfig" r SFTPConfig
  ) =>
  Job 'PartnerInvoiceDataExport ->
  m ExecutionResult
partnerInvoiceDataExportJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantId' = jobData.merchantId
      merchantOperatingCityId' = jobData.merchantOperatingCityId
      scheduleItself = jobData.scheduleItself

  now <- getCurrentTime
  -- Get logs from the last 24 hours that haven't been exported yet
  let yesterday = addUTCTime (intToNominalDiffTime (-86400)) now

  unexportedLogs <- QPartnerInvoiceDataLog.findUnexportedSince yesterday

  logInfo $ "Found " <> show (length unexportedLogs) <> " unexported partner invoice data logs"

  -- Generate JSON content
  let exportRecords = map toExportRecord unexportedLogs
      exportPayload =
        ExportPayload
          { exportDate = now,
            records = exportRecords
          }
      jsonContent = TE.decodeUtf8 $ BL.toStrict $ encode exportPayload
      fileName = "partner_invoice_data_" <> T.pack (show now) <> ".json"

  -- Upload via SFTP if there are records
  unless (null unexportedLogs) $ do
    uploadOk <- uploadToSFTP fileName jsonContent
    -- Mark records as exported only if upload was successful
    when uploadOk $ do
      forM_ unexportedLogs $ \logEntry -> do
        QPartnerInvoiceDataLog.markAsExported (Just now) logEntry.id
      logInfo $ "Exported " <> show (length unexportedLogs) <> " records via SFTP"

  -- Schedule next run (24 hours from now = 86400 seconds)
  when scheduleItself $ do
    let newJobData =
          PartnerInvoiceDataExportJobData
            { merchantId = merchantId',
              merchantOperatingCityId = merchantOperatingCityId',
              scheduleItself = True
            }
    createJobIn @_ @'PartnerInvoiceDataExport (Just merchantId') (Just merchantOperatingCityId') (intToNominalDiffTime 86400) newJobData
    logInfo "Scheduled next partner invoice data export job to run in 24 hours"

  return Complete

-- | Convert a PartnerInvoiceDataLog to an ExportRecord
toExportRecord :: DPIL.PartnerInvoiceDataLog -> ExportRecord
toExportRecord logEntry =
  ExportRecord
    { bookingId = logEntry.bookingId.getId,
      personId = logEntry.personId.getId,
      requestedAt = logEntry.requestedAt
    }

-- | Sanitize file name to prevent command injection
-- Only allows alphanumeric, dash, underscore, and dot characters
sanitizeFileName :: Text -> Text
sanitizeFileName = T.filter isAllowedChar
  where
    isAllowedChar c = isAlphaNum c || c == '-' || c == '_' || c == '.'

-- | Upload file content to SFTP server (secure implementation)
-- Uses SFTPConfig from AppEnv (configured in Dhall)
-- Security: Uses SFTP batch file and sanitized file names to prevent command injection
uploadToSFTP ::
  ( MonadFlow m,
    Log m,
    MonadIO m,
    MonadReader r m,
    HasField "sftpConfig" r SFTPConfig
  ) =>
  Text ->
  Text ->
  m Bool
uploadToSFTP rawFileName content = withLogTag "SFTP" $ do
  -- Sanitize file name to prevent command injection
  let fileName = sanitizeFileName rawFileName
  when (T.null fileName) $ do
    logError $ "Invalid file name after sanitization: " <> rawFileName
    pure ()

  -- Use secure temp files to prevent symlink/TOCTOU attacks
  (tmpContentPath, hContent) <- liftIO $ openTempFile "/tmp" ("sftp_export_" <> T.unpack fileName)
  liftIO $ hPutStr hContent (T.unpack content) >> hClose hContent
  logInfo $ "Written export file to " <> T.pack tmpContentPath

  -- Read SFTP config from environment
  sftpCfg <- asks (.sftpConfig)
  let host = T.unpack sftpCfg.host
      port = show sftpCfg.port
      username = T.unpack sftpCfg.username
      privateKeyPath = T.unpack sftpCfg.privateKeyPath
      remotePath = T.unpack sftpCfg.remotePath
      remoteFilePath = remotePath <> "/" <> T.unpack fileName

  -- Create SFTP batch file in a secure temp file
  (tmpBatchPath, hBatch) <- liftIO $ openTempFile "/tmp" ("sftp_batch_" <> T.unpack fileName <> ".txt")
  let batchContent = "put " <> tmpContentPath <> " " <> remoteFilePath <> "\nquit\n"
  liftIO $ hPutStr hBatch batchContent >> hClose hBatch
  logDebug $ "Created SFTP batch file: " <> T.pack tmpBatchPath

  -- Build SFTP argument list (no shell interpolation)
  let sftpArgs =
        [ "-i",
          privateKeyPath,
          "-P",
          port,
          "-o",
          "StrictHostKeyChecking=yes",
          "-b",
          tmpBatchPath,
          username <> "@" <> host
        ]

  logInfo $ "Uploading file " <> fileName <> " to SFTP server " <> sftpCfg.host <> ":" <> sftpCfg.remotePath
  logDebug $ "SFTP arguments: " <> T.pack (show sftpArgs)

  -- Execute SFTP upload
  result <- liftIO (Control.Exception.try $ callProcess "sftp" sftpArgs :: IO (Either SomeException ()))

  -- Clean up temp files
  liftIO $ removeFile tmpContentPath `catch` \(_ :: SomeException) -> pure ()
  liftIO $ removeFile tmpBatchPath `catch` \(_ :: SomeException) -> pure ()
  logDebug "Cleaned up temp files"

  case result of
    Right () -> do
      logInfo $ "Successfully uploaded " <> fileName <> " via SFTP"
      pure True
    Left err -> do
      logError $ "SFTP upload failed: " <> T.pack (show err)
      pure False
