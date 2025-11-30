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
import qualified Domain.Types.Ride as DRide
import Kernel.Beam.Functions
import Kernel.External.Encryption (decrypt)
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
  Redis.withWaitOnLockRedisWithExpiry (DRidePayment.paymentJobExecLockKey rideId.getId) 10 20 $ do
    logDebug "Executing payment intent"
    QRide.markPaymentStatus DRide.Initiated rideId
    person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
    fareWithTip <- case ride.tipAmount of
      Nothing -> return fare
      Just tipAmount -> fare `addPrice` tipAmount
    customerPaymentId <- person.customerPaymentId & fromMaybeM (PersonFieldNotPresent "customerPaymentId")
    paymentMethodId <- person.defaultPaymentMethodId & fromMaybeM (PersonFieldNotPresent "defaultPaymentMethodId")
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

    booking <- runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
    paymentIntentResp <- SPayment.makePaymentIntent person.merchantId person.merchantOperatingCityId person.id ride createPaymentIntentReq
    paymentCharged <- SPayment.chargePaymentIntent booking.merchantId booking.merchantOperatingCityId paymentIntentResp.paymentIntentId
    when paymentCharged $ QRide.markPaymentStatus DRide.Completed ride.id
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
  order <- QOrder.findById (cast rideId) >>= fromMaybeM (PaymentOrderNotFound rideId.getId)
  let mobileCountryCode = person.mobileCountryCode
  mobileNumber <- mapM decrypt person.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
  when (isNothing ride.cancellationFeeIfCancelled) $ do
    QRide.updateCancellationFeeIfCancelledField (Just cancellationAmount.amount) rideId
  paymentCharged <- SPayment.makeCxCancellationPayment booking.merchantId booking.merchantOperatingCityId order.paymentServiceOrderId cancellationAmount.amount
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
