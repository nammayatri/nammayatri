{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the

 GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 -}

module SharedLogic.Allocator.Jobs.Settlement.SettlementReportIngestion
  ( runSettlementReportIngestionJob,
  )
where

import qualified Data.ByteString.Lazy as LBS
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as M
import Data.Time.Calendar (addDays)
import Data.Time.Clock (UTCTime (UTCTime), secondsToDiffTime, utctDay)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.External.Encryption ()
import qualified Kernel.External.Settlement.Sources.Email as EmailSource
import qualified Kernel.External.Settlement.Sources.SFTP as SFTPSource
import Kernel.External.Settlement.Types (SettlementParseConfig (..), SettlementService (..), SettlementSourceConfig (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Lib.Finance.Settlement.Ingestion (ingestPaymentSettlementReport)
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import qualified Lib.Scheduler.JobStorageType.SchedulerType as JC
import SharedLogic.Allocator (AllocatorJobType (..), SettlementReportIngestionJobData (..))
import Storage.Beam.SchedulerJob ()

runSettlementReportIngestionJob ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    MonadFlow m,
    MonadIO m,
    HasShortDurationRetryCfg r c,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "blackListedJobs" r [Text],
    JobCreatorEnv r,
    HasSchemaName SchedulerJobT
  ) =>
  Job 'SettlementReportIngestion ->
  m ExecutionResult
runSettlementReportIngestionJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantId = jobData.merchantId
      merchantOperatingCityId = jobData.merchantOperatingCityId
      service = parseSettlementService jobData.settlementService

  let lockKey = "SettlementIngestion:" <> merchantId.getId <> ":" <> jobData.settlementService
  resultRef <- liftIO $ newIORef Complete

  mbResult <- Hedis.whenWithLockRedisAndReturnValue lockKey 3600 $ do
    logInfo $ "Starting settlement report ingestion for " <> jobData.settlementService

    csvResult <- fetchCsv jobData.sourceConfig
    case csvResult of
      Left err -> do
        logWarning $ "Failed to fetch settlement CSV: " <> err
        liftIO $ writeIORef resultRef Retry
      Right csvData -> do
        let filePattern = case jobData.sourceConfig of
              SFTPSourceConfig _ fileName -> Just fileName
              EmailSourceConfig _ -> Nothing
            parseConfig =
              SettlementParseConfig
                { service = service,
                  merchantId = merchantId.getId,
                  merchantOperatingCityId = merchantOperatingCityId.getId,
                  filePattern = filePattern
                }
        result <- ingestPaymentSettlementReport parseConfig csvData
        logInfo $ "Ingestion result: " <> show result

        when (result.totalFailed > 0) $
          logWarning $ "Settlement ingestion had " <> show result.totalFailed <> " failures"

        scheduleNextIngestionJob merchantId merchantOperatingCityId jobData
        liftIO $ writeIORef resultRef Complete

  case mbResult of
    Left () -> pure Complete
    Right () -> liftIO $ readIORef resultRef
  where
    fetchCsv ::
      (EncFlow m r, MonadIO m) =>
      SettlementSourceConfig ->
      m (Either Text LBS.ByteString)
    fetchCsv (SFTPSourceConfig cfg fileName) = SFTPSource.fetchSettlementFile cfg fileName
    fetchCsv (EmailSourceConfig cfg) = EmailSource.fetchSettlementFile cfg

    parseSettlementService :: Text -> SettlementService
    parseSettlementService "HyperPG" = HyperPG
    parseSettlementService "BillDesk" = BillDesk
    parseSettlementService _ = HyperPG

    scheduleNextIngestionJob ::
      ( BeamFlow m r,
        CacheFlow m r,
        EsqDBFlow m r,
        JobCreatorEnv r,
        HasSchemaName SchedulerJobT,
        HasField "schedulerType" r SchedulerType
      ) =>
      Id DM.Merchant ->
      Id DMOC.MerchantOperatingCity ->
      SettlementReportIngestionJobData ->
      m ()
    scheduleNextIngestionJob mId mOpCityId jd = do
      now <- getCurrentTime
      let todayDay = utctDay now
          tomorrowDay = addDays 1 todayDay
          tomorrowRunTime = UTCTime tomorrowDay (secondsToDiffTime 10800)
          scheduleAfter = diffUTCTime tomorrowRunTime now
      logInfo $ "Scheduling next settlement ingestion in " <> show scheduleAfter
      JC.createJobIn @_ @'SettlementReportIngestion (Just mId) (Just mOpCityId) scheduleAfter jd
