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
    runPgSettlementIngestionJob,
  )
where

import qualified Data.Map.Strict as M
import Data.Time.Calendar (addDays)
import Data.Time.Clock (UTCTime (UTCTime), secondsToDiffTime, utctDay)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.SubscriptionPurchase as DSP
import qualified EulerHS.Language as L
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.External.Encryption ()
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.External.Settlement.Types (JuspayOrderStatusConfig (..), SettlementService (..), SettlementServiceConfig (..))
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id (Id (..), ShortId (..))
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Domain.Types.PgPaymentSettlementReport as PgDom
import Lib.Finance.Settlement.Ingestion (ingestPaymentSettlementReport)
import Lib.Finance.Settlement.Pipeline (PipelineResult (..), runSettlementPipeline)
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPO
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import qualified Lib.Scheduler.JobStorageType.SchedulerType as JC
import SharedLogic.Allocator (AllocatorJobType (..), PgSettlementIngestionJobData (..), SettlementReportIngestionJobData (..))
import Storage.Beam.SchedulerJob ()
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import qualified Storage.Queries.SubscriptionPurchase as QSP

-- | Lock TTL reduced from 3600s to 600s (10 minutes) to avoid long lock holds
lockTTLSeconds :: Int
lockTTLSeconds = 600

-- ---------------------------------------------------------------------------
-- Old job: CSV/SFTP settlement ingestion (unchanged from main)
-- ---------------------------------------------------------------------------

runSettlementReportIngestionJob ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    MonadFlow m,
    MonadIO m,
    CoreMetrics m,
    L.MonadFlow m,
    HasRequestId r,
    MonadReader r m,
    HasShortDurationRetryCfg r c,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "blackListedJobs" r [Text],
    JobCreatorEnv r,
    HasSchemaName SchedulerJobT,
    Finance.HasActorInfo m r
  ) =>
  Job 'SettlementReportIngestion ->
  m ExecutionResult
runSettlementReportIngestionJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantId = jobData.merchantId
      merchantOperatingCityId = jobData.merchantOperatingCityId

  let lockKey = "SettlementIngestion:" <> merchantId.getId <> ":" <> merchantOperatingCityId.getId

  -- Acquire lock only for the ingestion phase, not for scheduling
  mbResult <- Hedis.whenWithLockRedisAndReturnValue lockKey lockTTLSeconds $ do
    logInfo "Starting settlement report ingestion, fetching configs from MerchantServiceConfig"

    settlementConfigs <- getSettlementConfigs merchantId merchantOperatingCityId
    if null settlementConfigs
      then do
        logWarning "No SettlementService configs found in MerchantServiceConfig"
        pure True -- success, nothing to do
      else do
        mbJuspayCfg <- case jobData.juspayServiceName of
          Just svcName -> getJuspayOrderStatusConfig merchantOperatingCityId svcName
          Nothing -> pure Nothing
        -- Process each service independently, catch errors per-service to avoid one failure blocking others
        results <- forM settlementConfigs $ \settlementSvcCfg -> do
          logInfo $ "Processing settlement service: " <> show settlementSvcCfg.settlementService
          let mbJuspayCfgForService =
                if fromMaybe False settlementSvcCfg.useJuspayOrderStatus
                  then mbJuspayCfg
                  else Nothing
          serviceResult <-
            try @_ @SomeException $
              ingestPaymentSettlementReport settlementSvcCfg mbJuspayCfgForService merchantId.getId merchantOperatingCityId.getId resolveOrderType
          case serviceResult of
            Left err -> do
              logError $ "Settlement ingestion for " <> show settlementSvcCfg.settlementService <> " threw exception: " <> show err
              pure False
            Right result -> do
              logInfo $ "Ingestion result for " <> show settlementSvcCfg.settlementService <> ": " <> show result
              when (result.totalFailed > 0) $
                logError $
                  "Settlement ingestion for " <> show settlementSvcCfg.settlementService <> " had " <> show result.totalFailed
                    <> " failures out of "
                    <> show result.totalParsed
                    <> " rows"
              pure (result.totalFailed == 0)

        pure $ and results -- True if all services succeeded
  case mbResult of
    Left () -> do
      logWarning $ "Settlement ingestion lock contention, will retry: " <> lockKey
      pure Retry
    Right allSucceeded -> do
      -- Schedule next run regardless of partial failures (to avoid missing runs)
      scheduleNextIngestionJob merchantId merchantOperatingCityId jobData
      if allSucceeded
        then pure Complete
        else do
          logWarning "Some settlement services had failures, but scheduling next run anyway"
          pure Complete
  where
    getSettlementConfigs ::
      (BeamFlow m r, CacheFlow m r, EsqDBFlow m r) =>
      Id DM.Merchant ->
      Id DMOC.MerchantOperatingCity ->
      m [SettlementServiceConfig]
    getSettlementConfigs _mId mOpCityId = do
      let allSettlementServices = [minBound .. maxBound] :: [SettlementService]
      configs <- forM allSettlementServices $ \service -> do
        mbConfig <- getOneConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = mOpCityId.getId, merchantId = Nothing, serviceName = Just (DMSC.SettlementService service)}) Nothing
        pure $ case mbConfig of
          Just cfg -> case cfg.serviceConfig of
            DMSC.SettlementServiceConfig settlementCfg -> Just settlementCfg
            _ -> Nothing
          Nothing -> Nothing
      pure $ catMaybes configs

-- ---------------------------------------------------------------------------
-- New job: PG settlement pipeline (API + CSV + SFTP — full pipeline)
-- ---------------------------------------------------------------------------

runPgSettlementIngestionJob ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    MonadFlow m,
    MonadIO m,
    CoreMetrics m,
    L.MonadFlow m,
    HasRequestId r,
    MonadReader r m,
    HasShortDurationRetryCfg r c,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "blackListedJobs" r [Text],
    JobCreatorEnv r,
    HasSchemaName SchedulerJobT,
    Finance.HasActorInfo m r
  ) =>
  Job 'PgSettlementIngestion ->
  m ExecutionResult
runPgSettlementIngestionJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantId = jobData.merchantId
      merchantOperatingCityId = jobData.merchantOperatingCityId
      shouldScheduleNext = fromMaybe True jobData.scheduleNextJob

  result <- try @_ @SomeException $ do
    configs <- pgGetSettlementConfigs merchantOperatingCityId jobData.settlementProvider
    case configs of
      [] -> do
        logWarning "No settlement configs found; nothing to ingest"
        when shouldScheduleNext $ scheduleNextPgIngestionJob merchantId merchantOperatingCityId jobData
        pure Complete
      _ -> do
        logInfo $ "Running settlement pipeline for " <> show (length configs) <> " provider(s)"
        results <- forM configs $ \cfg -> do
          let providerName = show cfg.settlementService
              lockKey = "settlement:ingestion:" <> providerName <> ":" <> merchantId.getId <> ":" <> merchantOperatingCityId.getId
          mbResult <- Hedis.whenWithLockRedisAndReturnValue lockKey lockTTLSeconds $ do
            mbJuspayCfg <- case jobData.juspayServiceName of
              Just svcName
                | fromMaybe False cfg.useJuspayOrderStatus ->
                  getJuspayOrderStatusConfig merchantOperatingCityId svcName
              _ -> pure Nothing
            pipelineResult <- runSettlementPipeline cfg mbJuspayCfg merchantId.getId merchantOperatingCityId.getId jobData.startTime jobData.endTime resolveOrderType
            case pipelineResult of
              PipelineSuccess ingResult -> do
                logInfo $ "Pipeline success for " <> providerName <> ": " <> show ingResult
                when (ingResult.totalFailed > 0) $
                  logError $
                    "Settlement ingestion for " <> providerName <> " had " <> show ingResult.totalFailed
                      <> " failures out of "
                      <> show ingResult.totalParsed
                      <> " rows"
                pure (ingResult.totalFailed == 0)
              PipelineSkipped reason -> do
                logInfo $ "Pipeline skipped for " <> providerName <> ": " <> reason
                pure True
              PipelineFailed err -> do
                logError $ "Pipeline failed for " <> providerName <> ": " <> err
                pure False
          case mbResult of
            Left () -> do
              logWarning $ "Settlement ingestion lock contention, will retry: " <> lockKey
              pure Nothing
            Right succeeded -> pure (Just succeeded)
        let lockContention = any isNothing results
        if lockContention
          then do
            logWarning "Lock contention on one or more providers, retrying"
            pure Retry
          else do
            when shouldScheduleNext $ scheduleNextPgIngestionJob merchantId merchantOperatingCityId jobData
            let allSucceeded = all (== Just True) results
            unless allSucceeded $
              logWarning "Some settlement services had failures, but scheduling next run anyway"
            pure Complete
  case result of
    Right execResult -> pure execResult
    Left err -> do
      logError $ "PgSettlementIngestion job crashed with exception: " <> show err
      pure Complete

-- ---------------------------------------------------------------------------
-- Helpers for the new PgSettlementIngestion job
-- ---------------------------------------------------------------------------

resolveOrderType ::
  (BeamFlow m r, EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  m (Maybe PgDom.OrderType, Maybe Bool, Maybe Text)
resolveOrderType orderId = do
  mbPaymentOrder <- QPO.findByShortId (ShortId orderId)
  case mbPaymentOrder of
    Nothing -> do
      logWarning $ "No payment order found for orderId: " <> orderId
      pure (Nothing, Nothing, Nothing)
    Just po -> do
      mbSubPurchase <- QSP.findByPaymentOrderId po.id
      case mbSubPurchase of
        Just sp ->
          pure (Just PgDom.SUBSCRIPTION, Just $ sp.status /= DSP.PENDING && sp.status /= DSP.FAILED, Just sp.id.getId)
        Nothing ->
          pure (Just PgDom.PAYOUT_REGISTRATION, Just False, Nothing)

pgGetSettlementConfigs ::
  (BeamFlow m r, CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe SettlementService ->
  m [SettlementServiceConfig]
pgGetSettlementConfigs mOpCityId mbService = do
  let services = maybe [minBound .. maxBound] (: []) mbService
  configs <- forM services $ \service -> do
    mbConfig <- getOneConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = mOpCityId.getId, merchantId = Nothing, serviceName = Just (DMSC.SettlementService service)}) Nothing
    pure $ case mbConfig of
      Just cfg -> case cfg.serviceConfig of
        DMSC.SettlementServiceConfig settlementCfg -> Just settlementCfg
        _ -> Nothing
      Nothing -> Nothing
  pure $ catMaybes configs

getJuspayOrderStatusConfig ::
  (BeamFlow m r, CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  DMSC.ServiceName ->
  m (Maybe JuspayOrderStatusConfig)
getJuspayOrderStatusConfig mOpCityId svcName = do
  mbCfg <- getOneConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = mOpCityId.getId, merchantId = Nothing, serviceName = Just svcName}) Nothing
  case mbCfg >>= extractPaymentServiceConfig . (.serviceConfig) of
    Just (Payment.JuspayConfig juspayCfg) ->
      pure . Just $
        JuspayOrderStatusConfig
          { juspayBaseUrl = juspayCfg.url,
            juspayApiKey = juspayCfg.apiKey
          }
    _ -> do
      logWarning $ "No Juspay MerchantServiceConfig found for juspayServiceName: " <> show svcName
      pure Nothing

extractPaymentServiceConfig :: DMSC.ServiceConfig -> Maybe Payment.PaymentServiceConfig
extractPaymentServiceConfig = \case
  DMSC.PaymentServiceConfig cfg -> Just cfg
  DMSC.RentalPaymentServiceConfig cfg -> Just cfg
  DMSC.CautioPaymentServiceConfig cfg -> Just cfg
  DMSC.MembershipPaymentServiceConfig cfg -> Just cfg
  DMSC.JuspayWalletServiceConfig cfg -> Just cfg
  _ -> Nothing

-- ---------------------------------------------------------------------------
-- Scheduling helpers
-- ---------------------------------------------------------------------------

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

scheduleNextPgIngestionJob ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    JobCreatorEnv r,
    HasSchemaName SchedulerJobT,
    HasField "schedulerType" r SchedulerType
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  PgSettlementIngestionJobData ->
  m ()
scheduleNextPgIngestionJob mId mOpCityId jd = do
  now <- getCurrentTime
  let ist = 19800
      nowIst = addUTCTime ist now
      todayIst = utctDay nowIst
      tomorrowIst = addDays 1 todayIst
      tomorrowRunTime = addUTCTime (negate ist) $ UTCTime tomorrowIst (secondsToDiffTime 7200)
      scheduleAfter = diffUTCTime tomorrowRunTime now
      nextStartTime = addUTCTime (negate ist) $ UTCTime todayIst 0
      nextEndTime = addUTCTime (negate ist) $ UTCTime todayIst (secondsToDiffTime 86399)
      nextJobData = jd {startTime = Just nextStartTime, endTime = Just nextEndTime, scheduleNextJob = Just True}
      minScheduleTime = tomorrowRunTime
      maxScheduleTime = addUTCTime 3600 tomorrowRunTime
  logInfo $
    "Scheduling next PG settlement ingestion in " <> show scheduleAfter
      <> " with startTime="
      <> show nextStartTime
      <> " endTime="
      <> show nextEndTime
  JC.createJobInWithCheck @_ @'PgSettlementIngestion (Just mId) (Just mOpCityId) scheduleAfter minScheduleTime maxScheduleTime "PgSettlementIngestion" (Just 1) nextJobData
