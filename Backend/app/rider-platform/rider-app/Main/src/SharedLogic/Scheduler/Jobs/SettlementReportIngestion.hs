{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the

 GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.SettlementReportIngestion
  ( runSettlementReportIngestionJob,
  )
where

import qualified Data.Map.Strict as M
import Data.Time.Calendar (addDays)
import Data.Time.Clock (UTCTime (UTCTime), secondsToDiffTime, utctDay)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.MerchantServiceConfig as DMSC
import qualified EulerHS.Language as L
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.External.Encryption ()
import qualified Kernel.External.Payment.Interface.Types as Payment
import Kernel.External.Settlement.Types (JuspayOrderStatusConfig (..), SettlementService, SettlementServiceConfig)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Settlement.Ingestion (ingestPaymentSettlementReport)
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import qualified Lib.Scheduler.JobStorageType.SchedulerType as JC
import SharedLogic.JobScheduler
import Storage.Beam.SchedulerJob ()
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))

-- | Lock TTL reduced from 3600s to 600s (10 minutes) to avoid long lock holds
lockTTLSeconds :: Int
lockTTLSeconds = 600

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
          Just svcName -> getJuspayOrderStatusConfig merchantId merchantOperatingCityId svcName
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
              ingestPaymentSettlementReport settlementSvcCfg mbJuspayCfgForService merchantId.getId merchantOperatingCityId.getId (\_ -> pure (Nothing, Nothing, Nothing))
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
      (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
      Id DM.Merchant ->
      Id DMOC.MerchantOperatingCity ->
      m [SettlementServiceConfig]
    getSettlementConfigs mId mOpCityId = do
      let allSettlementServices = [minBound .. maxBound] :: [SettlementService]
      configs <- forM allSettlementServices $ \service -> do
        mbConfig <-
          getOneConfig
            ( MerchantServiceConfigDimensions
                { merchantId = mId.getId,
                  merchantOperatingCityId = mOpCityId.getId,
                  serviceName = Just (DMSC.SettlementService service)
                }
            )
            Nothing
        pure $ case mbConfig of
          Just cfg -> case cfg.serviceConfig of
            DMSC.SettlementServiceConfig settlementCfg -> Just settlementCfg
            _ -> Nothing
          Nothing -> Nothing
      pure $ catMaybes configs

    getJuspayOrderStatusConfig ::
      (MonadFlow m, CacheFlow m r, EsqDBFlow m r, Log m) =>
      Id DM.Merchant ->
      Id DMOC.MerchantOperatingCity ->
      DMSC.ServiceName ->
      m (Maybe JuspayOrderStatusConfig)
    getJuspayOrderStatusConfig mId mOpCityId svcName = do
      mbCfg <-
        getOneConfig
          ( MerchantServiceConfigDimensions
              { merchantId = mId.getId,
                merchantOperatingCityId = mOpCityId.getId,
                serviceName = Just svcName
              }
          )
          Nothing
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
      DMSC.MetroPaymentServiceConfig cfg -> Just cfg
      DMSC.BusPaymentServiceConfig cfg -> Just cfg
      DMSC.BbpsPaymentServiceConfig cfg -> Just cfg
      DMSC.MultiModalPaymentServiceConfig cfg -> Just cfg
      DMSC.PassPaymentServiceConfig cfg -> Just cfg
      DMSC.ParkingPaymentServiceConfig cfg -> Just cfg
      DMSC.MembershipPaymentServiceConfig cfg -> Just cfg
      DMSC.JuspayWalletServiceConfig cfg -> Just cfg
      _ -> Nothing

    scheduleNextIngestionJob ::
      ( CacheFlow m r,
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
