{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.ExecutePaymentIntent where

import qualified Data.HashMap.Strict as HM
import qualified Domain.Types.Ride as DRide
import Kernel.Beam.Functions
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payment.Interface as PaymentInterface
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.LedgerEntry as LE
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import Lib.Scheduler
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified SharedLogic.Finance.RidePayment as RidePaymentFinance
import SharedLogic.JobScheduler
import SharedLogic.Payment as SPayment
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Error

executePaymentIntentJob ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    SchedulerFlow r,
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r
  ) =>
  Job 'ExecutePaymentIntent ->
  m ExecutionResult
executePaymentIntentJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      personId = jobData.personId
      rideId = jobData.rideId
      fare = jobData.fare
      applicationFeeAmount = jobData.applicationFeeAmount
  Redis.withWaitOnLockRedisWithExpiry (SPayment.paymentJobExecLockKey rideId.getId) 10 20 $ do
    logDebug "Executing payment intent"
    -- Check if payment is already completed (idempotent check using invoice status)
    ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
    -- Check if payment is already settled (idempotent check using ledger entry status)
    isPaymentSettled <- RidePaymentFinance.isRidePaymentSettled rideId.getId
    if isPaymentSettled
      then logInfo $ "Payment already settled (ledger status) for ride: " <> rideId.getId <> ", skipping"
      else do
        -- Check if existing order is already charged at gateway level
        mbOrderId <- SPayment.getOrderIdForRide rideId
        mbOrder <- maybe (pure Nothing) QOrder.findById mbOrderId
        case mbOrder of
          Just order | order.status == PaymentInterface.CHARGED -> do
            logInfo $ "Payment order already charged for ride: " <> rideId.getId <> ", marking payment as completed"
            QRide.markPaymentStatus DRide.Completed rideId
          _ -> do
            -- Proceed with payment capture
            QRide.markPaymentStatus DRide.Initiated rideId
            person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
            fareWithTip <- case ride.tipAmount of
              Nothing -> return fare
              Just tipAmount -> fare `addPrice` tipAmount
            booking <- runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
            (customerPaymentId, paymentMethodId) <- SPayment.getCustomerAndPaymentMethod booking person
            driverAccountId <- ride.driverAccountId & fromMaybeM (RideFieldNotPresent "driverAccountId")
            email <- mapM decrypt person.email
            let createPaymentIntentServiceReq =
                  DPayment.CreatePaymentIntentServiceReq
                    { amount = fareWithTip.amount,
                      applicationFeeAmount,
                      currency = fareWithTip.currency,
                      customer = customerPaymentId,
                      paymentMethod = paymentMethodId,
                      receiptEmail = email,
                      driverAccountId
                    }
            -- Use ledger entry IDs from Redis if available, otherwise no existing order
            let mbExistingOrderId = mbOrderId
            let ledgerCtx = RidePaymentFinance.buildRiderFinanceCtx person.merchantId.getId booking.merchantOperatingCityId.getId fareWithTip.currency person.id.getId rideId.getId Nothing Nothing
                ledgerInfo =
                  Just $
                    SPayment.RidePaymentLedgerInfo
                      { rideFare = fareWithTip.amount - applicationFeeAmount,
                        gstAmount = 0, -- TODO: extract GST from fare breakup
                        platformFee = applicationFeeAmount,
                        financeCtx = ledgerCtx
                      }
            paymentIntentResp <- SPayment.makePaymentIntent person.merchantId booking.merchantOperatingCityId booking.paymentMode person.id (Just rideId) mbExistingOrderId createPaymentIntentServiceReq ledgerInfo
            paymentCharged <- SPayment.chargePaymentIntent booking.merchantId booking.merchantOperatingCityId booking.paymentMode paymentIntentResp.paymentIntentId rideId RidePaymentFinance.settledReasonRidePayment
            if paymentCharged
              then QRide.markPaymentStatus DRide.Completed ride.id
              else do
                QRide.markPaymentStatus DRide.Failed ride.id
                logError $ "Failed to charge payment intent for ride: " <> ride.id.getId

            -- Handle tip payment orders (FALLBACK: check for pending tip ledger entries)
            tipEntries <- RidePaymentFinance.findRidePaymentEntries rideId.getId
            let pendingTipEntries = filter (\e -> e.referenceType == RidePaymentFinance.ridePaymentRefTip && e.status == LE.PENDING) tipEntries
            unless (null pendingTipEntries) $ do
              logInfo $ "Found " <> show (length pendingTipEntries) <> " pending tip entries for ride: " <> rideId.getId
  return Complete

cancelExecutePaymentIntentJob ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    SchedulerFlow r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r
  ) =>
  Job 'CancelExecutePaymentIntent ->
  m ExecutionResult
cancelExecutePaymentIntentJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      bookingId = jobData.bookingId
      personId = jobData.personId
      rideId = jobData.rideId
      cancellationAmount = jobData.cancellationAmount
  logDebug "Cancelling payment intent"
  booking <- runInReplica $ QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  merchantOpCity <- CQMOC.findById booking.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound booking.merchantOperatingCityId.getId)
  -- Lookup order via Redis-stored ledger entry IDs or by direct ID cast
  mbOrderId <- SPayment.getOrderIdForRide rideId
  orderId <- mbOrderId & fromMaybeM (PaymentOrderNotFound rideId.getId)
  order <- QOrder.findById orderId >>= fromMaybeM (PaymentOrderNotFound orderId.getId)
  let mobileCountryCode = person.mobileCountryCode
  mobileNumber <- mapM decrypt person.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
  when (isNothing ride.cancellationFeeIfCancelled) $ do
    QRide.updateCancellationFeeIfCancelledField (Just cancellationAmount.amount) rideId
  paymentCharged <- SPayment.makeCxCancellationPayment booking.merchantId booking.merchantOperatingCityId booking.paymentMode order.paymentServiceOrderId cancellationAmount.amount
  when paymentCharged $ QRide.markPaymentStatus DRide.Completed rideId
  void $
    CallBPPInternal.customerCancellationDuesSync
      (merchant.driverOfferApiKey)
      (merchant.driverOfferBaseUrl)
      (merchant.driverOfferMerchantId)
      mobileNumber
      (fromMaybe "+91" mobileCountryCode)
      (Just cancellationAmount.amount)
      (Just cancellationAmount)
      Nothing
      True
      (merchantOpCity.city)
  return Complete
