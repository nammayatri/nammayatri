{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.Payout.DriverReferralPayout where

import Data.Time (addDays, utctDay)
import qualified Domain.Action.UI.Payout as DAP
import qualified Domain.Types.DailyStats as DS
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.PayoutConfig
import qualified Domain.Types.Vehicle as DV
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payout.Interface as Juspay
import qualified Kernel.External.Payout.Types as PT
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as Payout
import qualified Lib.Payment.Domain.Types.Common as DLP
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import SharedLogic.Allocator
import Storage.Beam.Payment ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.PayoutConfig as CPC
import qualified Storage.Queries.DailyStats as QDailyStats
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QV
import qualified Tools.Payout as TP

sendDriverReferralPayoutJobData ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    SchedulerFlow r
  ) =>
  Job 'DriverReferralPayout ->
  m ExecutionResult
sendDriverReferralPayoutJobData Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantOpCityId = jobData.merchantOperatingCityId
      merchantId = jobData.merchantId
      statusForRetry = jobData.statusForRetry
      toScheduleNextPayout = jobData.toScheduleNextPayout
  payoutConfigList <- CPC.findByMerchantOpCityIdAndIsPayoutEnabled merchantOpCityId True
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let reschuleTimeDiff = listToMaybe payoutConfigList <&> (.timeDiff)
  localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
  let lastDay = addDays (-1) (utctDay localTime)
  dailyStatsForEveryDriverList <- QDailyStats.findAllByDateAndPayoutStatus (Just transporterConfig.payoutBatchLimit) (Just 0) lastDay statusForRetry
  if null dailyStatsForEveryDriverList || null payoutConfigList
    then do
      when toScheduleNextPayout $ do
        case reschuleTimeDiff of
          Just timeDiff' -> do
            maxShards <- asks (.maxShards)
            createJobIn @_ @'DriverReferralPayout timeDiff' maxShards $
              DriverReferralPayoutJobData
                { merchantId = merchantId,
                  merchantOperatingCityId = merchantOpCityId,
                  toScheduleNextPayout = toScheduleNextPayout,
                  statusForRetry = statusForRetry
                }
          Nothing -> pure ()
      return Complete
    else do
      for_ dailyStatsForEveryDriverList $ \executionData -> do
        fork ("processing Payout for DriverId : " <> executionData.driverId.getId) $ do
          callPayout merchantId (cast merchantOpCityId) executionData payoutConfigList statusForRetry
      ReSchedule <$> getRescheduledTime (fromMaybe 5 transporterConfig.driverFeeCalculatorBatchGap)

callPayout ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    SchedulerFlow r
  ) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  DS.DailyStats ->
  [PayoutConfig] ->
  DS.PayoutStatus ->
  m ()
callPayout merchantId merchantOpCityId DS.DailyStats {..} payoutConfigList statusForRetry = do
  mbVehicle <- QV.findById driverId
  let vehicleCategory = fromMaybe DV.AUTO_CATEGORY ((.category) =<< mbVehicle)
  let payoutConfig' = find (\payoutConfig -> payoutConfig.vehicleCategory == vehicleCategory) payoutConfigList
  case payoutConfig' of
    Just payoutConfig -> do
      uid <- generateGUID
      person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      dInfo <- QDI.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      merchantOperatingCity <- CQMOC.findById (cast merchantOpCityId) >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
      case dInfo.payoutVpa of
        Just vpa -> do
          Redis.withWaitOnLockRedisWithExpiry (DAP.payoutProcessingLockKey driverId.getId) 3 3 $ do
            QDailyStats.updatePayoutStatusById DS.Processing id
            QDailyStats.updatePayoutOrderId (Just uid) id
          phoneNo <- mapM decrypt person.mobileNumber
          let entityName = DLP.DRIVER_DAILY_STATS
              createPayoutOrderReq =
                Juspay.CreatePayoutOrderReq
                  { orderId = uid,
                    amount = referralEarnings,
                    customerPhone = fromMaybe "6666666666" phoneNo, -- dummy no.
                    customerEmail = fromMaybe "dummymail@gmail.com" person.email, -- dummy mail
                    customerId = driverId.getId,
                    orderType = payoutConfig.orderType,
                    remark = payoutConfig.remark,
                    customerName = person.firstName,
                    customerVpa = vpa
                  }
          if referralEarnings <= payoutConfig.thresholdPayoutAmountPerPerson
            then do
              logDebug $ "calling create payoutOrder with driverId: " <> driverId.getId <> " | amount: " <> show referralEarnings <> " | orderId: " <> show uid
              let serviceName = DEMSC.PayoutService PT.Juspay
                  createPayoutOrderCall = TP.createPayoutOrder merchantId merchantOpCityId serviceName
              mbPayoutOrderResp <- try @_ @SomeException $ Payout.createPayoutService (cast merchantId) (cast driverId) (Just [id]) (Just entityName) (show merchantOperatingCity.city) createPayoutOrderReq createPayoutOrderCall
              errorCatchAndHandle id driverId.getId mbPayoutOrderResp payoutConfig statusForRetry (\_ -> pure ())
            else do
              Redis.withWaitOnLockRedisWithExpiry (DAP.payoutProcessingLockKey driverId.getId) 3 3 $ do
                QDailyStats.updatePayoutStatusById DS.ManualReview id
          pure ()
        Nothing -> pure ()
    Nothing -> pure ()

errorCatchAndHandle ::
  (EsqDBReplicaFlow m r, EsqDBFlow m r, EncFlow m r, CacheFlow m r) =>
  Text ->
  Text ->
  Either SomeException a ->
  PayoutConfig ->
  DS.PayoutStatus ->
  (a -> m ()) ->
  m ()
errorCatchAndHandle dailyStatsId driverId resp' payoutConfig statusForRetry function = do
  case resp' of
    Left _ -> do
      eligibleForRetryInNextBatch <- isEligibleForRetryInNextBatch (mkManualLinkErrorTrackingByDailyStatsIdKey dailyStatsId) payoutConfig.maxRetryCount
      if eligibleForRetryInNextBatch
        then Redis.withWaitOnLockRedisWithExpiry (DAP.payoutProcessingLockKey driverId) 3 3 $ do
          QDailyStats.updatePayoutStatusById statusForRetry dailyStatsId
        else do
          let status = if statusForRetry == DS.ManualReview then DS.Processing else DS.ManualReview
          Redis.withWaitOnLockRedisWithExpiry (DAP.payoutProcessingLockKey driverId) 3 3 $ do
            QDailyStats.updatePayoutStatusById status dailyStatsId
    Right resp -> function resp

isEligibleForRetryInNextBatch :: (MonadFlow m, CacheFlow m r) => Text -> Int -> m Bool
isEligibleForRetryInNextBatch key maxCount = do
  count <-
    Hedis.get key >>= \case
      Just count -> return count
      Nothing -> return 0
  if count < maxCount
    then do
      void $ Hedis.incr key
      Hedis.expire key (60 * 60 * 6) ---- 6 hours expiry
      return True
    else return False

mkManualLinkErrorTrackingByDailyStatsIdKey :: Text -> Text
mkManualLinkErrorTrackingByDailyStatsIdKey dailyStatsId = "ErrCntPayout:DsId:" <> dailyStatsId

getRescheduledTime :: (MonadFlow m) => NominalDiffTime -> m UTCTime
getRescheduledTime gap = addUTCTime gap <$> getCurrentTime