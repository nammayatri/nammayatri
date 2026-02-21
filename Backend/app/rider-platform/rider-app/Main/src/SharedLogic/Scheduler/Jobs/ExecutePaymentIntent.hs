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
import qualified Domain.Action.UI.RidePayment as DRidePayment
import Domain.Types.PaymentInvoice (PaymentPurpose (TIP))
import qualified Domain.Types.PaymentInvoice as DPI
import qualified Domain.Types.Ride as DRide
import Kernel.Beam.Functions
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payment.Interface as PaymentInterface
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import Lib.Scheduler
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import SharedLogic.JobScheduler
import SharedLogic.Payment as SPayment
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.PaymentInvoice as QPaymentInvoice
import qualified Storage.Queries.PaymentInvoiceExtra as QPaymentInvoiceExtra
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
  Redis.withWaitOnLockRedisWithExpiry (DRidePayment.paymentJobExecLockKey rideId.getId) 10 20 $ do
    logDebug "Executing payment intent"
    -- Check if payment is already completed (idempotent check using invoice status)
    ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
    mbInvoiceForCheck <- QPaymentInvoiceExtra.findByRideIdAndTypeAndPurpose rideId DPI.PAYMENT DPI.RIDE
    let isPaymentCaptured = maybe False (\inv -> inv.paymentStatus == DPI.CAPTURED) mbInvoiceForCheck
    if isPaymentCaptured
      then logInfo $ "Payment already captured (invoice status) for ride: " <> rideId.getId <> ", skipping"
      else do
        -- Get payment order via payment_invoice (order id is not ride id)
        let mbPaymentOrderId = mbInvoiceForCheck >>= (.paymentOrderId)
        mbOrder <- maybe (pure Nothing) QOrder.findById mbPaymentOrderId
        case mbOrder of
          Just order | order.status == PaymentInterface.CHARGED -> do
            logInfo $ "Payment order already charged for ride: " <> rideId.getId <> ", marking payment as completed"
            whenJust mbInvoiceForCheck $ \inv ->
              QPaymentInvoice.updatePaymentStatus DPI.CAPTURED inv.id
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
            let mbExistingOrderId = mbInvoiceForCheck >>= (.paymentOrderId)
            paymentIntentResp <- SPayment.makePaymentIntent person.merchantId booking.merchantOperatingCityId booking.paymentMode person.id mbExistingOrderId createPaymentIntentServiceReq
            paymentCharged <- SPayment.chargePaymentIntent booking.merchantId booking.merchantOperatingCityId booking.paymentMode paymentIntentResp.paymentIntentId
            let orderId = paymentIntentResp.orderId
            if paymentCharged
              then do
                QRide.markPaymentStatus DRide.Completed ride.id
                -- Update invoice status to CAPTURED when payment is successfully charged
                mbInvoiceByOrder <- QPaymentInvoice.findByPaymentOrderIdAndInvoiceType (Just orderId) DPI.PAYMENT
                whenJust mbInvoiceByOrder $ \invoice -> QPaymentInvoice.updatePaymentStatus DPI.CAPTURED invoice.id
              else do
                -- Update ride.paymentStatus and invoice.paymentStatus to FAILED when scheduler fails
                QRide.markPaymentStatus DRide.Failed ride.id
                logError $ "Failed to charge payment intent for ride: " <> ride.id.getId
                mbInvoiceByOrder <- QPaymentInvoice.findByPaymentOrderIdAndInvoiceType (Just orderId) DPI.PAYMENT
                whenJust mbInvoiceByOrder $ \invoice -> QPaymentInvoice.updatePaymentStatus DPI.FAILED invoice.id

            -- Handle tip payment orders (FALLBACK: for tips that weren't immediately captured)
            allInvoices <- QPaymentInvoice.findAllByRideId rideId
            let tipInvoices = filter (\inv -> inv.paymentPurpose == TIP && inv.paymentStatus == DPI.PENDING) allInvoices

            -- Process each tip payment order
            forM_ tipInvoices $ \tipInvoice -> do
              whenJust tipInvoice.paymentOrderId $ \tipPaymentOrderId -> do
                tipPaymentOrder <- QOrder.findById tipPaymentOrderId
                whenJust tipPaymentOrder $ \tipOrder -> do
                  let paymentIntentId = tipOrder.paymentServiceOrderId
                  tipPaymentCharged <- SPayment.chargePaymentIntent booking.merchantId booking.merchantOperatingCityId booking.paymentMode paymentIntentId
                  if tipPaymentCharged
                    then do
                      QPaymentInvoice.updatePaymentStatus DPI.CAPTURED tipInvoice.id
                      logDebug $ "Successfully charged tip payment intent: " <> paymentIntentId
                    else do
                      QPaymentInvoice.updatePaymentStatus DPI.FAILED tipInvoice.id
                      logError $ "Failed to charge tip payment intent: " <> paymentIntentId
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
  -- Lookup order via PaymentInvoice since order.id is independent of ride.id
  mbInvoice <- QPaymentInvoiceExtra.findByRideIdAndTypeAndPurpose rideId DPI.PAYMENT DPI.RIDE
  invoice <- mbInvoice & fromMaybeM (InternalError $ "Payment invoice not found for ride: " <> rideId.getId)
  orderId <- invoice.paymentOrderId & fromMaybeM (InternalError $ "Payment order ID not found in invoice for ride: " <> rideId.getId)
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
