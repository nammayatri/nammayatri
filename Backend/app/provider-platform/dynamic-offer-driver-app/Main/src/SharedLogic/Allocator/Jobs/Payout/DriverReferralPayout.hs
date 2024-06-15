{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.Payout.DriverReferralPayout where

import Data.Time (utctDay)
-- import qualified Domain.Action.UI.Payment as AP
import qualified Domain.Types.DailyStats as DS
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.PayoutConfig
-- import Domain.Types.Person
-- import Domain.Types.TransporterConfig
-- import qualified Kernel.Payment
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payment.Interface as Juspay
-- import qualified Kernel.External.Payment.Juspay.Types.Payout as Payout
import qualified Kernel.External.Payment.Types as PT
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
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
-- import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.DailyStats as QDailyStats
import qualified Storage.Queries.DriverInformation as QDI
-- import qualified Storage.Queries.DriverReferral as QDR
-- import qualified Storage.Queries.DriverStats as QDS
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QV
-- import Tools.Error
import qualified Tools.Payment as TP

-- import Utils.Common.Cac.KeyNameConstants

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
  dailyStatsForEveryDriverList <- QDailyStats.findAllByDateAndPayoutStatus (Just transporterConfig.payoutBatchLimit) (Just 0) (utctDay localTime) statusForRetry
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
      -- rescheduleTime <- getRescheduledTime transporterConfig
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
  vehicle <- QV.findById driverId >>= fromMaybeM (VehicleNotFound driverId.getId)
  let vehicleVariant = vehicle.variant
  let payoutConfig' = find (\payoutConfig -> payoutConfig.vehicleVariant == vehicleVariant) payoutConfigList
  case payoutConfig' of
    Just payoutConfig -> do
      uid <- generateGUID
      QDailyStats.updatePayoutStatusById DS.Processing id
      person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      dInfo <- QDI.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
      merchantOperatingCity <- CQMOC.findById (cast merchantOpCityId) >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
      case dInfo.payoutVpa of
        Just vpa -> do
          phoneNo <- mapM decrypt person.mobileNumber
          let entityName = DLP.DRIVER_DAILY_STATS
              createPayoutOrderReq =
                Juspay.CreatePayoutOrderReq
                  { orderId = uid,
                    amount = referralEarnings,
                    customerPhone = fromMaybe "" phoneNo, -- handle
                    customerEmail = fromMaybe "" person.email, -- handle
                    customerId = driverId.getId,
                    orderType = "FULFILL_ONLY", -- can be configurable (but value is fixed for payout)
                    remark = "Referral Reward From Nammayatri", -- can be configurable
                    customerName = person.firstName,
                    customerVpa = vpa
                  }
          if referralEarnings <= payoutConfig.thresholdPayoutAmountPerPerson
            then do
              let serviceName = DEMSC.PayoutService PT.JuspayPayout
                  createPayoutOrderCall = TP.createPayoutOrder merchantId merchantOpCityId serviceName
              mbPayoutOrderResp <- try @_ @SomeException $ Payout.createPayoutService (cast merchantId) (cast driverId) (Just id) (Just entityName) (show merchantOperatingCity.city) createPayoutOrderReq createPayoutOrderCall
              errorCatchAndHandle id mbPayoutOrderResp payoutConfig statusForRetry (\_ -> pure ())
            else -- whenJust mbPayoutOrderResp $ \payoutOrderResp -> do -- cant do like that, either type
            --   QDailyStats.updatePayoutStatusById (AP.castPayoutOrderStatus payoutOrderResp.status) id       -- req or not?
            do
              QDailyStats.updatePayoutStatusById DS.ManualReview id
          pure ()
        Nothing -> pure ()
    Nothing -> pure ()

errorCatchAndHandle ::
  (EsqDBReplicaFlow m r, EsqDBFlow m r, EncFlow m r, CacheFlow m r) =>
  Text ->
  Either SomeException a ->
  PayoutConfig ->
  DS.PayoutStatus ->
  (a -> m ()) ->
  m ()
errorCatchAndHandle dailyStatsId resp' payoutConfig statusForRetry function = do
  case resp' of
    Left _ -> do
      eligibleForRetryInNextBatch <- isEligibleForRetryInNextBatch (mkManualLinkErrorTrackingByDailyStatsIdKey dailyStatsId) payoutConfig.maxRetryCount
      if eligibleForRetryInNextBatch
        then QDailyStats.updatePayoutStatusById statusForRetry dailyStatsId -- check
        else do
          let status = if statusForRetry == DS.ManualReview then DS.Processing else DS.ManualReview
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

-- getRescheduledTime :: MonadTime m => TransporterConfig -> m UTCTime
-- getRescheduledTime tc = addUTCTime tc.mandateExecutionRescheduleInterval <$> getCurrentTime

getRescheduledTime :: (MonadFlow m) => NominalDiffTime -> m UTCTime
getRescheduledTime gap = addUTCTime gap <$> getCurrentTime
