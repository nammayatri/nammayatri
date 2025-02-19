{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.Payout.MetroIncentivePayout where

import qualified Domain.Action.Internal.Payout as DAP
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import Domain.Types.FRFSTicketBooking
import Domain.Types.Merchant as DM
import Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.PayoutConfig
import Kernel.External.Encryption (decrypt)
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
import SharedLogic.JobScheduler
import Storage.Beam.Payment ()
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.PayoutConfig as CPC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.FRFSTicketBooking as QFTB
import qualified Storage.Queries.Person as QPerson
import qualified Tools.Payout as TP

sendCustomerRefund ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    SchedulerFlow r
  ) =>
  Job 'MetroIncentivePayout ->
  m ExecutionResult
sendCustomerRefund Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantOpCityId = jobData.merchantOperatingCityId
      merchantId = jobData.merchantId
      statusForRetry = jobData.statusForRetry
      toScheduleNextPayout = jobData.toScheduleNextPayout
  payoutConfig <- CPC.findByMerchantOpCityIdAndIsPayoutEnabledAndPayoutEntity merchantOpCityId True METRO_TICKET_CASHBACK Nothing
  riderConfig <- QRC.findByMerchantOperatingCityId merchantOpCityId Nothing >>= fromMaybeM (InternalError $ "RiderConfig not found for mocId: " <> show merchantOpCityId.getId)
  let rescheduleTimeDiff = payoutConfig <&> (.timeDiff)
  eligibleBookingsList <- QFTB.findAllByCashbackStatus riderConfig.payoutBatchSize (Just 0) (Just statusForRetry)
  logDebug $ "Bookings eligible for cashback: " <> show eligibleBookingsList
  if null eligibleBookingsList
    then do
      when toScheduleNextPayout $ do
        case rescheduleTimeDiff of
          Just timeDiff' -> do
            logDebug "Rescheduling the Job for Next Day"
            createJobIn @_ @'MetroIncentivePayout (Just merchantId) (Just merchantOpCityId) timeDiff' $
              MetroIncentivePayoutJobData
                { merchantOperatingCityId = merchantOpCityId,
                  toScheduleNextPayout = toScheduleNextPayout,
                  schedulePayoutForDay = Nothing,
                  ..
                }
          Nothing -> pure ()
      return Complete
    else do
      for_ eligibleBookingsList $ \booking -> do
        fork ("processing Payout for riderId : " <> show booking.riderId.getId <> "and bookingId : " <> show booking.id.getId) $ do
          callPayout merchantId (cast merchantOpCityId) booking payoutConfig statusForRetry

      ReSchedule <$> getRescheduledTime riderConfig.payoutBatchDelay

getRescheduledTime :: (MonadFlow m) => NominalDiffTime -> m UTCTime
getRescheduledTime gap = addUTCTime gap <$> getCurrentTime

callPayout ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    SchedulerFlow r
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  FRFSTicketBooking ->
  Maybe PayoutConfig ->
  CashbackStatus ->
  m ()
callPayout merchantId merchantOpCityId booking payoutConfig statusForRetry = do
  case payoutConfig of
    Just config -> do
      uid <- generateGUID
      person <- QPerson.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
      merchantOperatingCity <- CQMOC.findById person.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound person.merchantOperatingCityId.getId)
      case booking.payerVpa of
        Just payoutVpa -> do
          Redis.withWaitOnLockRedisWithExpiry (DAP.payoutProcessingLockKey booking.id.getId) 3 3 $ do
            QFTB.updatePayoutStatusById (Just PROCESSING) booking.id
            QFTB.updatePayoutOrderId (Just uid) booking.id
            phoneNo <- mapM decrypt person.mobileNumber
            emailId <- mapM decrypt person.email
            let amount = fromMaybe 0 booking.eventDiscountAmount
                entityName = DLP.METRO_BOOKING_CASHBACK
                createPayoutOrderReq = Payout.mkCreatePayoutOrderReq uid amount phoneNo emailId person.id.getId config.remark person.firstName payoutVpa config.orderType
            logDebug $ "calling create payoutOrder with riderId: " <> person.id.getId <> " | amount: " <> show booking.eventDiscountAmount <> " | orderId: " <> show uid
            let serviceName = DEMSC.PayoutService PT.Juspay
                createPayoutOrderCall = TP.createPayoutOrder person.merchantId person.merchantOperatingCityId serviceName
            mbPayoutOrderResp <- try @_ @SomeException $ Payout.createPayoutService (cast merchantId) (Just $ cast merchantOpCityId) (cast person.id) (Just [booking.id.getId]) (Just entityName) (show merchantOperatingCity.city) createPayoutOrderReq createPayoutOrderCall
            errorCatchAndHandle booking.id person.id.getId uid mbPayoutOrderResp config statusForRetry (\_ -> pure ())
            pure ()
        Nothing -> do
          Redis.withWaitOnLockRedisWithExpiry (DAP.payoutProcessingLockKey booking.id.getId) 3 3 $ do
            QFTB.updatePayoutStatusById (Just MANUAL_VERIFICATION) booking.id
          pure ()
    Nothing -> pure ()

errorCatchAndHandle ::
  (EsqDBReplicaFlow m r, EsqDBFlow m r, EncFlow m r, CacheFlow m r) =>
  Id FRFSTicketBooking ->
  Text ->
  Text ->
  Either SomeException a ->
  PayoutConfig ->
  CashbackStatus ->
  (a -> m ()) ->
  m ()
errorCatchAndHandle bookingId riderId orderId resp' payoutConfig statusForRetry function = do
  case resp' of
    Left _ -> do
      logDebug $ "Error in calling create payout riderId: " <> riderId <> " | orderId: " <> orderId
      eligibleForRetryInNextBatch <- isEligibleForRetryInNextBatch (mkMetroCashbackManualTrackingKey bookingId.getId) payoutConfig.maxRetryCount
      if eligibleForRetryInNextBatch
        then Redis.withWaitOnLockRedisWithExpiry (DAP.payoutProcessingLockKey bookingId.getId) 3 3 $ do
          QFTB.updatePayoutStatusById (Just statusForRetry) bookingId
        else do
          let status = if statusForRetry == MANUAL_VERIFICATION then PROCESSING else MANUAL_VERIFICATION
          Redis.withWaitOnLockRedisWithExpiry (DAP.payoutProcessingLockKey bookingId.getId) 3 3 $ do
            QFTB.updatePayoutStatusById (Just status) bookingId
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
      Hedis.expire key (60 * 60 * 6)
      return True
    else return False

mkMetroCashbackManualTrackingKey :: Text -> Text
mkMetroCashbackManualTrackingKey bookingId = "ErrCntPayout:FRFSBookingId:" <> bookingId
