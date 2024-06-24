module SharedLogic.Payment where

import qualified Domain.Types.Booking as Booking
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Prelude
import Kernel.Types.CacheFlow
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import Storage.Beam.Payment ()
import qualified Tools.Payment as TPayment

makePaymentIntent ::
  (MonadFlow m, EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Id Merchant.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Id Person.Person ->
  Ride.Ride ->
  Booking.Booking ->
  Payment.CustomerId ->
  Payment.PaymentMethodId ->
  Payment.AccountId ->
  Maybe Text ->
  m Payment.CreatePaymentIntentResp
makePaymentIntent merchantId merchantOpCityId personId ride booking customerPaymentId paymentMethodId driverAccountId email = do
  let createPaymentIntentReq =
        Payment.CreatePaymentIntentReq
          { amount = booking.estimatedFare.amount,
            applicationFeeAmount = maybe 0.0 (.amount) booking.estimatedApplicationFee,
            currency = booking.estimatedFare.currency,
            customer = customerPaymentId,
            paymentMethod = paymentMethodId,
            receiptEmail = email,
            driverAccountId
          }
      commonMerchantId = cast @Merchant.Merchant @DPayment.Merchant merchantId
      commonPersonId = cast @Person.Person @DPayment.Person personId
      commonRideId = cast @Ride.Ride @DPayment.Ride ride.id
      createPaymentIntentCall = TPayment.createPaymentIntent merchantId merchantOpCityId
      updatePaymentIntentAmountCall = TPayment.updateAmountInPaymentIntent merchantId merchantOpCityId
  DPayment.createPaymentIntentService commonMerchantId commonPersonId commonRideId ride.shortId.getShortId createPaymentIntentReq createPaymentIntentCall updatePaymentIntentAmountCall
