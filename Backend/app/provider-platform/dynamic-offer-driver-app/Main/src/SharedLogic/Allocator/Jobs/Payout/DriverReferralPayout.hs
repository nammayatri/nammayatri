{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.Payout.DriverReferralPayout where

import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Time (utctDay)
import Domain.Action.UI.Call
import qualified Domain.Types.CallStatus as SCS
import qualified Domain.Types.DailyStats as DS
import Domain.Types.Merchant
import Domain.Types.PayoutConfig
import Domain.Types.Person
import qualified Kernel.External.Payment.Interface as Juspay
import Kernel.External.Types (Language (..), SchedulerFlow)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as Payout
import qualified Lib.Payment.Domain.Types.Common as DLP
import Lib.Scheduler
import SharedLogic.Allocator
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant.MerchantMessage as CMM
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.CachedQueries.Merchant.Overlay as CMO
import qualified Storage.CachedQueries.Merchant.PayoutConfig as CPC
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.DailyStats as QDailyStats
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverReferral as QDR
import qualified Storage.Queries.DriverStats as QDS
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import Utils.Common.Cac.KeyNameConstants

sendDriverReferralPayoutJobData ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
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
  payoutConfigList <- CPC.findByMerchantOpCityIdAndIsPayoutEnabled merchantOpCityId True >>= fromMaybeM (InternalError "Payout config not present")
  -- for_ vehicleVariants $ \variant -> do
  -- payoutConfigsList <- CPC.findAllPayoutConfig
  transporterConfig <- SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc -- use payoutConfig.timeDiff
  dailyStatsForEveryDriverList <- QDailyStats.findAllByDateAndPayoutStatus transporterConfig.payoutBatchLimit 0 (utctDay localTime) statusForRetry -- handle
  if null dailyStatsForEveryDriverList
    then return Complete
    else do
      for_ dailyStatsForEveryDriverList $ \executionData -> do
        fork ("processing Payout for DriverId : " <> executionData.driverId.id.getId) $ do
          callPayout executionData merchantId merchantOpCityId payoutConfigList
      ReSchedule <$> getRescheduledTime transporterConfig
  -- forM_ dailyStatsForEveryDriverList (callPayout merchantId merchantOpCityId)
  -- when toScheduleNextPayout $ PENDING: Reschedule the job
  pure Complete

callPayout ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    SchedulerFlow r
  ) =>
  Id Merchant ->
  Id MerchantOperatingCity ->
  DS.DailyStats ->
  m ()
callPayout merchantId merchantOpCityId DS.DailyStats {..} = do
  QDailyStats.updatePayoutStatusById DS.Processing id
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  dInfo <- QDI.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)

  uid <- generateGUID
  case dInfo.payoutVpa of
    Just vpa -> do
      let entityName = DLP.DRIVER_DAILY_STATS
      let createPayoutOrderReq =
            Juspay.CreatePayoutOrderReq
              { orderId = uid,
                amount = referralEarnings,
                customerPhone = person.mobileNumber,
                customerEmail = person.email,
                customerId = "",
                orderType = "",
                remark = "Referral Reward From Nammayatri",
                customerName = person.firstName,
                customerVpa = vpa
              }
      if referralEarnings <= payoutConfig.thresholdPayoutAmountPerPerson
        then do
          let createPayoutOrderCall = Payment.payoutOrderStatus merchantId merchantOpCityId createPayoutOrderReq
          mbPayoutOrderResp <- try @_ @SomeException $ Payout.createPayoutService merchantId driverId entityName id "city" createPayoutOrderReq createPayoutOrderCall -- PENDING: handle this and below errorCatchAndHandle
          errorCatchAndHandle driverId mbPayoutOrderResp subscriptionConfigs endTime now (\_ -> QDPlan.updateLastPaymentLinkSentAtDateByDriverIdAndServiceName (Just endTime) driverId serviceName)
          whenJust mbPayoutOrderResp $ \payoutOrderResp -> do
            -- when payoutOrderResp.status == Juspay.FULFILLMENTS_SUCCESSFUL $
            QDailyStats.updatePayoutStatusById (castPayoutOrderStatus payoutOrderResp.status) id --DS.Success id
        else do
          -- update entityName to manual
          QDailyStats.updatePayoutStatusById DS.ManualReview id
      pure ()
    Nothing -> pure ()

errorCatchAndHandle ::
  (EsqDBReplicaFlow m r, EsqDBFlow m r, EncFlow m r, CacheFlow m r, HasField "smsCfg" r SmsConfig) =>
  Id Driver ->
  Either SomeException a ->
  PayoutConfig ->
  UTCTime ->
  UTCTime ->
  (a -> m ()) ->
  m ()
errorCatchAndHandle driverId resp' payoutConfig endTime _ function = do
  case resp' of
    Left _ -> do
      eligibleForRetryInNextBatch <- isEligibleForRetryInNextBatch (mkManualLinkErrorTrackingByDriverIdKey driverId) payoutConfig.maxRetryCount
      if eligibleForRetryInNextBatch
        then return ()
        else pure () -- change
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

mkManualLinkErrorTrackingByDriverIdKey :: Id Driver -> Text
mkManualLinkErrorTrackingByDriverIdKey driverId = "ErrorRetryCountFor:DriverId:" <> driverId.getId

getsetManualLinkErrorTrackingKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> m (Maybe Int)
getsetManualLinkErrorTrackingKey driverId = Hedis.get (mkManualLinkErrorTrackingByDriverIdKey driverId)

getRescheduledTime :: MonadTime m => TransporterConfig -> m UTCTime
getRescheduledTime tc = addUTCTime tc.mandateExecutionRescheduleInterval <$> getCurrentTime
