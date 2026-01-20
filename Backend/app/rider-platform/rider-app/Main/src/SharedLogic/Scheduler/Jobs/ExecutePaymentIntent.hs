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
import Domain.Types.EmptyDynamicParam as EmptyDynamicParam
import Domain.Types.PaymentInvoice (PaymentPurpose (TIP))
import qualified Domain.Types.PaymentInvoice as DPI
import qualified Domain.Types.Ride as DRide
import Kernel.Beam.Functions
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Notification as Notification
import Kernel.External.Payment.Interface.Types as Payment
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QOrder
import Lib.Scheduler
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import SharedLogic.JobScheduler
import SharedLogic.Payment as SPayment
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.PaymentInvoice as QPaymentInvoice
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Notifications as Notify

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
    QRide.markPaymentStatus DRide.Initiated rideId
    person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
    fareWithTip <- case ride.tipAmount of
      Nothing -> return fare
      Just tipAmount -> fare `addPrice` tipAmount
    booking <- runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
    (customerPaymentId, paymentMethodId) <- SPayment.getCustomerAndPaymentMethod booking person
    driverAccountId <- ride.driverAccountId & fromMaybeM (RideFieldNotPresent "driverAccountId")
    email <- mapM decrypt person.email
    let createPaymentIntentReq =
          Payment.CreatePaymentIntentReq
            { orderShortId = ride.shortId.getShortId,
              amount = fareWithTip.amount,
              applicationFeeAmount,
              currency = fareWithTip.currency,
              customer = customerPaymentId,
              paymentMethod = paymentMethodId,
              receiptEmail = email,
              driverAccountId
            }
    paymentIntentResp <- SPayment.makeRidePaymentIntent person.merchantId person.merchantOperatingCityId booking.paymentMode person.id ride createPaymentIntentReq
    paymentCharged <- SPayment.chargePaymentIntent booking.merchantId booking.merchantOperatingCityId booking.paymentMode paymentIntentResp.paymentIntentId
    when paymentCharged $ do
      QRide.markPaymentStatus DRide.Completed ride.id
      paymentOrder <- QOrder.findById (cast ride.id)
      whenJust paymentOrder $ \order -> do
        mbInvoice <- QPaymentInvoice.findByPaymentOrderId (Just order.id)
        whenJust mbInvoice $ \invoice -> QPaymentInvoice.updatePaymentStatus DPI.CAPTURED invoice.id
    allInvoices <- QPaymentInvoice.findAllByRideId rideId
    let tipInvoices = filter (\inv -> inv.paymentPurpose == TIP && inv.paymentStatus == DPI.PENDING) allInvoices
    forM_ tipInvoices $ \tipInvoice -> do
      whenJust tipInvoice.paymentOrderId $ \tipPaymentOrderId -> do
        tipPaymentOrder <- QOrder.findById tipPaymentOrderId
        whenJust tipPaymentOrder $ \order -> do
          let paymentIntentId = order.paymentServiceOrderId
          tipPaymentCharged <- SPayment.chargePaymentIntent booking.merchantId booking.merchantOperatingCityId booking.paymentMode paymentIntentId
          when tipPaymentCharged $ do
            QPaymentInvoice.updatePaymentStatus DPI.CAPTURED tipInvoice.id
            logDebug $ "Successfully charged tip payment intent: " <> paymentIntentId
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
  let mobileCountryCode = person.mobileCountryCode
  mobileNumber <- mapM decrypt person.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")

  when (isNothing ride.cancellationFeeIfCancelled) $ do
    QRide.updateCancellationFeeIfCancelledField (Just cancellationAmount.amount) rideId

  mbInvoice <- case jobData.noShowCharge of
    Just paymentPurpose -> do
      invoices <- QPaymentInvoice.findAllByRideId rideId
      let cancellationInvoice = find (\inv -> inv.paymentPurpose == paymentPurpose) invoices
      return cancellationInvoice
    Nothing -> return Nothing

  mbOrder <- QOrder.findById (cast rideId)
  paymentCharged <- case mbOrder of
    Just order -> do
      logInfo $ "Processing cancellation payment for card ride: " <> rideId.getId
      charged <- SPayment.makeCxCancellationPayment booking.merchantId booking.merchantOperatingCityId booking.paymentMode order.paymentServiceOrderId cancellationAmount.amount
      when charged $ do
        QRide.markPaymentStatus DRide.Completed rideId
        whenJust mbInvoice $ \invoice -> do
          QPaymentInvoice.updatePaymentStatus DPI.CAPTURED invoice.id
          logInfo $ "Updated payment invoice status to CAPTURED for invoiceId: " <> invoice.id.getId
      unless charged $ do
        whenJust mbInvoice $ \invoice -> do
          QPaymentInvoice.updatePaymentStatus DPI.FAILED invoice.id
          logInfo $ "Updated payment invoice status to FAILED for invoiceId: " <> invoice.id.getId
      return charged
    Nothing -> do
      logInfo $ "No payment order found for ride (Cash ride case): " <> rideId.getId <> ". Adding to pending dues."
      return False
  void $
    CallBPPInternal.customerCancellationDuesSync
      (merchant.driverOfferApiKey)
      (merchant.driverOfferBaseUrl)
      (merchant.driverOfferMerchantId)
      mobileNumber
      (fromMaybe "+91" mobileCountryCode)
      (if paymentCharged then Just cancellationAmount.amount else Nothing)
      (if paymentCharged then Just cancellationAmount else Nothing)
      Nothing
      paymentCharged
      (merchantOpCity.city)

  whenJust jobData.noShowCharge $ \_ -> do
    fork "Notify Driver - Card Ride Cancellation Charges" $ do
      void $
        CallBPPInternal.sendDriverCancellationNotification
          (merchant.driverOfferApiKey)
          (merchant.driverOfferBaseUrl)
          (merchant.driverOfferMerchantId)
          (ride.bppRideId.getId)
          (booking.id.getId)
          cancellationAmount
          (if paymentCharged then "SUCCESS" else "PENDING")

  unless paymentCharged $ do
    let duesAmount = show cancellationAmount.amount <> " " <> show cancellationAmount.currency
    mbMerchantPN <- CPN.findMatchingMerchantPN person.merchantOperatingCityId "CANCELLATION_DUES_PENDING" Nothing Nothing person.language Nothing
    case mbMerchantPN of
      Just merchantPN -> do
        let title = merchantPN.title
            body = merchantPN.body
            notificationData =
              Notification.NotificationReq
                { category = Notification.PAYMENT_FAILED,
                  subCategory = Nothing,
                  showNotification = Notification.SHOW,
                  messagePriority = Nothing,
                  entity = Notification.Entity Notification.Product person.id.getId (),
                  body = body,
                  title = title,
                  dynamicParams = EmptyDynamicParam.EmptyDynamicParam,
                  auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken,
                  ttl = Nothing,
                  sound = Nothing
                }
        Notify.notifyPerson person.merchantId person.merchantOperatingCityId person.id notificationData Nothing
      Nothing -> do
        logInfo $ "MerchantPushNotification not found for CANCELLATION_DUES_PENDING, sending default notification for personId: " <> person.id.getId
        let entityData = Notify.NotifReq {title = "Pending Cancellation Dues", message = "You have pending cancellation dues of " <> duesAmount <> ". Please clear your dues before booking a new ride."}
        Notify.notifyPersonOnEvents person entityData Notification.PAYMENT_FAILED

  return Complete
