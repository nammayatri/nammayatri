module SharedLogic.Payment where

import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as Ride
import qualified Kernel.External.Payment.Interface as Payment
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
  Payment.CreatePaymentIntentReq ->
  m Payment.CreatePaymentIntentResp
makePaymentIntent merchantId merchantOpCityId personId ride createPaymentIntentReq = do
  let commonMerchantId = cast @Merchant.Merchant @DPayment.Merchant merchantId
      commonPersonId = cast @Person.Person @DPayment.Person personId
      commonRideId = cast @Ride.Ride @DPayment.Ride ride.id
      createPaymentIntentCall = TPayment.createPaymentIntent merchantId merchantOpCityId
      updatePaymentIntentAmountCall = TPayment.updateAmountInPaymentIntent merchantId merchantOpCityId
  DPayment.createPaymentIntentService commonMerchantId commonPersonId commonRideId ride.shortId.getShortId createPaymentIntentReq createPaymentIntentCall updatePaymentIntentAmountCall
