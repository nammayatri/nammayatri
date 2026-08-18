{-# OPTIONS_GHC -Wno-deprecations #-}

{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.FRFSCCAvenueSplitPayout
  ( runFRFSCCAvenueSplitPayoutJob,
  )
where

import qualified Data.Map.Strict as M
import Data.Time.Calendar (addDays)
import Data.Time.Clock (UTCTime (UTCTime), secondsToDiffTime, utctDay)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified EulerHS.Language as L
import qualified External.CCAvenue.Split as CCASplit
import qualified External.CCAvenue.SplitPayout as CCA
import qualified External.CCAvenue.Types as CCAvenue
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import qualified Kernel.Storage.ClickhouseV2 as CH
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Domain.Types.PaymentTransaction as DTransaction
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import qualified Lib.Payment.Storage.Queries.PaymentTransaction as QTransaction
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import qualified Lib.Scheduler.JobStorageType.SchedulerType as JC
import SharedLogic.JobScheduler
import Storage.Beam.Payment ()
import Storage.Beam.SchedulerJob ()
import qualified Storage.Clickhouse.FRFSSplitPayoutCandidate as CHCandidate
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))

lockTTLSeconds :: Int
lockTTLSeconds = 1800

defaultRunAtHourUtc :: Integer
defaultRunAtHourUtc = 2

type SplitPayoutFlow m r =
  ( PaymentBeamFlow.BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    MonadFlow m,
    CoreMetrics m,
    L.MonadFlow m,
    HasRequestId r,
    MonadReader r m,
    CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m
  )

runFRFSCCAvenueSplitPayoutJob ::
  ( SplitPayoutFlow m r,
    HasShortDurationRetryCfg r c,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "blackListedJobs" r [Text],
    JobCreatorEnv r,
    HasSchemaName SchedulerJobT
  ) =>
  Job 'FRFSCCAvenueSplitPayout ->
  m ExecutionResult
runFRFSCCAvenueSplitPayoutJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantId = jobData.merchantId
      merchantOperatingCityId = jobData.merchantOperatingCityId

  mbConfig <- getSplitPayoutConfig merchantId merchantOperatingCityId
  case mbConfig of
    Nothing -> do
      logWarning "No SplitPayout_CCAvenue MerchantServiceConfig found, stopping (job will not reschedule)"
      pure Complete
    Just config
      | not config.enabled -> do
        logInfo "SplitPayout_CCAvenue config is disabled, stopping (job will not reschedule)"
        pure Complete
      | otherwise -> do
        let lockKey = "FRFSCCAvenueSplitPayout:" <> merchantId.getId <> ":" <> merchantOperatingCityId.getId
        mbResult <- Hedis.whenWithLockRedisAndReturnValue lockKey lockTTLSeconds $ do
          withTryCatch "frfsCCAvenueSplitPayoutJob" $ runForCity merchantOperatingCityId config
        case mbResult of
          Left () -> do
            logWarning $ "FRFS CCAvenue split payout lock contention, will retry: " <> lockKey
            pure Retry
          Right (Left err) -> do
            logError $ "FRFS CCAvenue split payout run threw, will retry: " <> show err
            pure Retry
          Right (Right stats) -> do
            scheduleNextRun merchantId merchantOperatingCityId jobData config
            logInfo $ "FRFS CCAvenue split payout run finished: " <> show stats
            pure Complete

data RunStats = RunStats
  { candidates :: Int,
    alreadySplit :: Int,
    skipped :: Int,
    sent :: Int,
    failed :: Int
  }
  deriving (Show, Eq, Generic, ToJSON)

emptyStats :: RunStats
emptyStats = RunStats {candidates = 0, alreadySplit = 0, skipped = 0, sent = 0, failed = 0}

runForCity ::
  SplitPayoutFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  CCAvenue.CCAvenueSplitPayoutConfig ->
  m RunStats
runForCity merchantOperatingCityId config = do
  workingKey <- decrypt config.workingKey
  orderShortIds <- CHCandidate.findSettledOrderShortIds merchantOperatingCityId config.lookbackDays config.queryPageSize

  foldM
    (processOrder config workingKey)
    emptyStats {candidates = length orderShortIds}
    orderShortIds

processOrder ::
  SplitPayoutFlow m r =>
  CCAvenue.CCAvenueSplitPayoutConfig ->
  Text ->
  RunStats ->
  Text ->
  m RunStats
processOrder config workingKey stats orderShortId = withLogTag ("OrderShortId-" <> orderShortId) do
  result <- withTryCatch "frfsCCAvenueSplitPayoutJob:processOrder" $ do
    mbOrder <- QOrder.findByShortId (ShortId orderShortId)
    case mbOrder of
      Nothing -> do
        logWarning "Payment order not found in Postgres, skipping"
        pure stats {skipped = stats.skipped + 1}
      Just order -> do
        mbTransaction <- QTransaction.findEarliestChargedTransactionByOrderId order.id
        case mbTransaction of
          Nothing -> do
            logInfo "No CHARGED transaction for this order, skipping"
            pure stats {skipped = stats.skipped + 1}
          Just transaction -> processTransaction config workingKey stats order transaction
  case result of
    Right updatedStats -> pure updatedStats
    Left err -> do
      logError $ "Split payout failed for order: " <> show err
      pure stats {failed = stats.failed + 1}

processTransaction ::
  SplitPayoutFlow m r =>
  CCAvenue.CCAvenueSplitPayoutConfig ->
  Text ->
  RunStats ->
  DOrder.PaymentOrder ->
  DTransaction.PaymentTransaction ->
  m RunStats
processTransaction config workingKey stats order transaction
  | alreadySucceeded transaction = do
    logDebug "Split payout already succeeded for this transaction, skipping"
    pure stats {alreadySplit = stats.alreadySplit + 1}
  | otherwise = do
    let amount = fromMaybe order.amount order.effectAmount
    case transaction.epgTxnId of
      Nothing -> do
        logWarning "CHARGED transaction has no epgTxnId, skipping"
        pure stats {skipped = stats.skipped + 1}
      Just epgTxnId ->
        case CCASplit.calculateSplits amount config.vendorSplits of
          Left err -> do
            logError $ "Could not build split_data_list for amount " <> show amount <> ": " <> err
            pure stats {skipped = stats.skipped + 1}
          Right splitDataList -> do
            let request =
                  CCA.SplitPayoutRequest
                    { reference_no = epgTxnId,
                      split_tdr_charge_type = config.splitTdrChargeType,
                      merComm = config.merComm,
                      split_data_list = splitDataList
                    }
            response <- CCA.createSplitPayout config.gatewayUrl config.accessCode workingKey request
            QTransaction.updateSplitPayoutResponse transaction.id (Just $ toJSON response) (Just response.success)
            if response.success
              then do
                logInfo $ "Split payout accepted for epgTxnId " <> epgTxnId <> ", amount " <> show amount
                pure stats {sent = stats.sent + 1}
              else do
                logError $
                  "Split payout rejected for epgTxnId " <> epgTxnId <> ": "
                    <> fromMaybe "" response.ccavenueMessage
                pure stats {failed = stats.failed + 1}

alreadySucceeded :: DTransaction.PaymentTransaction -> Bool
alreadySucceeded transaction = transaction.splitPayoutSuccess == Just True

getSplitPayoutConfig ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  m (Maybe CCAvenue.CCAvenueSplitPayoutConfig)
getSplitPayoutConfig merchantId merchantOperatingCityId = do
  mbConfig <-
    getOneConfig
      ( MerchantServiceConfigDimensions
          { merchantId = merchantId.getId,
            merchantOperatingCityId = merchantOperatingCityId.getId,
            serviceName = Just (DMSC.SplitPayoutService DMSC.CCAvenue)
          }
      )
      Nothing
  pure $ case mbConfig of
    Just cfg -> case cfg.serviceConfig of
      DMSC.SplitPayoutServiceConfig splitCfg -> Just splitCfg
      _ -> Nothing
    Nothing -> Nothing

scheduleNextRun ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    JobCreatorEnv r,
    HasSchemaName SchedulerJobT,
    HasField "schedulerType" r SchedulerType
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  FRFSCCAvenueSplitPayoutJobData ->
  CCAvenue.CCAvenueSplitPayoutConfig ->
  m ()
scheduleNextRun merchantId merchantOperatingCityId jobData config = do
  now <- getCurrentTime
  let runAtHour = case config.runAtHourUtc of
        Just hour | hour >= 0 && hour <= 23 -> fromIntegral hour
        _ -> defaultRunAtHourUtc
      nextRunAt = UTCTime (addDays 1 (utctDay now)) (secondsToDiffTime $ runAtHour * 3600)
      scheduleAfter = diffUTCTime nextRunAt now
  logInfo $ "Scheduling next FRFS CCAvenue split payout run in " <> show scheduleAfter
  JC.createJobIn @_ @'FRFSCCAvenueSplitPayout (Just merchantId) (Just merchantOperatingCityId) scheduleAfter jobData
