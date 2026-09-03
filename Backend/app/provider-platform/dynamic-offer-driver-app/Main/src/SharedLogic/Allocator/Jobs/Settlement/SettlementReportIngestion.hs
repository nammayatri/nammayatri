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
import Lib.Finance.Settlement.Pipeline (PipelineResult (..), runSettlementPipeline)
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPO
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import qualified Lib.Scheduler.JobStorageType.SchedulerType as JC
import SharedLogic.Allocator (AllocatorJobType (..), SettlementReportIngestionJobData (..))
import Storage.Beam.SchedulerJob ()
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import qualified Storage.Queries.SubscriptionPurchase as QSP

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

  let shouldScheduleNext = fromMaybe True jobData.scheduleNextJob

  configs <- resolveSettlementConfigs merchantId merchantOperatingCityId jobData.settlementProvider
  case configs of
    [] -> do
      logWarning "No settlement configs found; nothing to ingest"
      when shouldScheduleNext $ scheduleNextIngestionJob merchantId merchantOperatingCityId jobData
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
          result <- runSettlementPipeline cfg mbJuspayCfg merchantId.getId merchantOperatingCityId.getId jobData.startTime jobData.endTime resolveOrderType
          case result of
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
          when shouldScheduleNext $ scheduleNextIngestionJob merchantId merchantOperatingCityId jobData
          let allSucceeded = all (== Just True) results
          unless allSucceeded $
            logWarning "Some settlement services had failures, but scheduling next run anyway"
          pure Complete
  where
    resolveSettlementConfigs ::
      (BeamFlow m r, CacheFlow m r, EsqDBFlow m r) =>
      Id DM.Merchant ->
      Id DMOC.MerchantOperatingCity ->
      Maybe Text ->
      m [SettlementServiceConfig]
    resolveSettlementConfigs mId mOpCityId = \case
      Just providerName -> do
        mbCfg <- getSettlementConfigForService mId mOpCityId providerName
        case mbCfg of
          Just cfg -> pure [cfg]
          Nothing -> do
            logWarning $ "No config found for settlement provider: " <> providerName
            pure []
      Nothing ->
        getSettlementConfigs mId mOpCityId

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

    getSettlementConfigForService ::
      (BeamFlow m r, CacheFlow m r, EsqDBFlow m r) =>
      Id DM.Merchant ->
      Id DMOC.MerchantOperatingCity ->
      Text ->
      m (Maybe SettlementServiceConfig)
    getSettlementConfigForService _mId mOpCityId serviceName = do
      let allServices = [minBound .. maxBound] :: [SettlementService]
          mbService = find (\s -> show s == serviceName) allServices
      case mbService of
        Nothing -> do
          logWarning $ "Unknown settlement service name: " <> serviceName
          pure Nothing
        Just service -> do
          mbConfig <- getOneConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = mOpCityId.getId, merchantId = Nothing, serviceName = Just (DMSC.SettlementService service)}) Nothing
          pure $ case mbConfig of
            Just cfg -> case cfg.serviceConfig of
              DMSC.SettlementServiceConfig settlementCfg -> Just settlementCfg
              _ -> Nothing
            Nothing -> Nothing

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
      let ist = 19800
          nowIst = addUTCTime ist now
          todayIst = utctDay nowIst
          tomorrowIst = addDays 1 todayIst
          tomorrowRunTime = addUTCTime (negate ist) $ UTCTime tomorrowIst (secondsToDiffTime 7200)
          scheduleAfter = diffUTCTime tomorrowRunTime now
          nextStartTime = addUTCTime (negate ist) $ UTCTime todayIst 0
          nextEndTime = addUTCTime (negate ist) $ UTCTime todayIst (secondsToDiffTime 86399)
          nextJobData = jd {startTime = Just nextStartTime, endTime = Just nextEndTime, scheduleNextJob = Just True}
      logInfo $
        "Scheduling next settlement ingestion in " <> show scheduleAfter
          <> " with startTime="
          <> show nextStartTime
          <> " endTime="
          <> show nextEndTime
      JC.createJobIn @_ @'SettlementReportIngestion (Just mId) (Just mOpCityId) scheduleAfter nextJobData
