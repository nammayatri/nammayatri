module Domain.Action.UI.RidePayment where

import qualified API.Types.UI.RidePayment
import Data.Maybe (listToMaybe)
import qualified Data.Text as Text
import qualified Domain.Types.FareBreakup
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.PaymentCustomer as DPaymentCustomer
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Domain.Types.RideStatus
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import Kernel.External.Encryption (decrypt)
import Kernel.External.Payment.Interface.Types
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.CacheFlow
import Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Error
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified SharedLogic.Payment as Payment
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.PaymentCustomer as QPaymentCustomer
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Tools.Payment as Payment

data DFareBreakup = DFareBreakup
  { amount :: Price,
    description :: Text
  }
  deriving (Generic, Show)

getcustomer ::
  Domain.Types.Person.Person -> Environment.Flow Payment.CreateCustomerResp
getcustomer person = do
  let paymentMode = fromMaybe DMPM.LIVE person.paymentMode
  mbCustomer <- QPaymentCustomer.findByCustomerIdAndPaymentMode person.id.getId (Just paymentMode)
  case mbCustomer of
    Just customer -> do
      now <- getCurrentTime
      if maybe False (> now) customer.clientAuthTokenExpiry
        then
          return $
            Payment.CreateCustomerResp
              { customerId = customer.customerId,
                clientAuthToken = customer.clientAuthToken,
                clientAuthTokenExpiry = customer.clientAuthTokenExpiry
              }
        else do
          getCustomer <- Payment.getCustomer person.merchantId person.merchantOperatingCityId person.paymentMode person.id.getId
          QPaymentCustomer.updateCATAndExipry getCustomer.clientAuthToken getCustomer.clientAuthTokenExpiry getCustomer.customerId (Just paymentMode)
          return $ getCustomer
    Nothing -> do
      customer <- Payment.getCustomer person.merchantId person.merchantOperatingCityId person.paymentMode person.id.getId
      paymentCustomer <- buildCreateCustomer customer paymentMode
      QPaymentCustomer.create paymentCustomer
      return customer

buildCreateCustomer ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r
  ) =>
  Payment.CreateCustomerResp ->
  DMPM.PaymentMode ->
  m DPaymentCustomer.PaymentCustomer
buildCreateCustomer createCustomerResp paymentMode = do
  now <- getCurrentTime
  return
    DPaymentCustomer.PaymentCustomer
      { clientAuthToken = createCustomerResp.clientAuthToken,
        clientAuthTokenExpiry = createCustomerResp.clientAuthTokenExpiry,
        customerId = createCustomerResp.customerId,
        paymentMode = Just paymentMode,
        createdAt = now,
        updatedAt = now
      }

getCustomerPaymentId :: Domain.Types.Person.Person -> Environment.Flow CustomerId
getCustomerPaymentId person = do
  let paymentMode = fromMaybe DMPM.LIVE person.paymentMode
  let mbCustomerId =
        case paymentMode of
          DMPM.LIVE -> person.customerPaymentId
          DMPM.TEST -> person.customerTestPaymentId
  case mbCustomerId of
    Just customerId -> return customerId
    Nothing -> do
      --- Create a customer in payment service if not there ---
      mbEmailDecrypted <- mapM decrypt person.email
      phoneDecrypted <- mapM decrypt person.mobileNumber
      let req = Payment.CreateCustomerReq {email = mbEmailDecrypted, name = person.firstName, phone = phoneDecrypted, lastName = Nothing, objectReferenceId = Nothing, mobileCountryCode = Nothing, optionsGetClientAuthToken = Nothing}
      customer <- Payment.createCustomer person.merchantId person.merchantOperatingCityId person.paymentMode req
      case paymentMode of
        DMPM.LIVE -> QPerson.updateCustomerPaymentId (Just customer.customerId) person.id
        DMPM.TEST -> QPerson.updateTestCustomerPaymentId (Just customer.customerId) person.id
      return customer.customerId

checkIfPaymentMethodExists :: Domain.Types.Person.Person -> PaymentMethodId -> Environment.Flow Bool
checkIfPaymentMethodExists person paymentMethodId = do
  customerPaymentId <- getCustomerPaymentId person
  cardList <- Payment.getCardList person.merchantId person.merchantOperatingCityId person.paymentMode customerPaymentId
  return $ paymentMethodId `elem` (cardList <&> (.cardId))

getPaymentMethods ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.Flow API.Types.UI.RidePayment.PaymentMethodsResponse
  )
getPaymentMethods (mbPersonId, _) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  customerPaymentId <- getCustomerPaymentId person
  resp <- Payment.getCardList person.merchantId person.merchantOperatingCityId person.paymentMode customerPaymentId -- TODO: Add pagination, do we need to store the card details in our DB?
  let savedPaymentMethodIds = resp <&> (.cardId)
  let paymentMode = fromMaybe DMPM.LIVE person.paymentMode
  let defaultPaymentMethodId =
        case paymentMode of
          DMPM.LIVE -> person.defaultPaymentMethodId
          DMPM.TEST -> person.defaultTestPaymentMethodId
  when (maybe False (\dpm -> dpm `notElem` savedPaymentMethodIds) defaultPaymentMethodId) $ do
    let firstSavedPaymentMethodId = listToMaybe savedPaymentMethodIds
    Payment.updateDefaultPersonPaymentMethodId person firstSavedPaymentMethodId
  return $ API.Types.UI.RidePayment.PaymentMethodsResponse {list = resp, defaultPaymentMethodId}

postPaymentMethodsMakeDefault ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.External.Payment.Interface.Types.PaymentMethodId ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postPaymentMethodsMakeDefault (mbPersonId, _) paymentMethodId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  checkIfPaymentMethodExists person paymentMethodId >>= \case
    False -> throwError $ InvalidRequest "Payment method doesn't belong to Customer"
    True -> Payment.updateDefaultPersonPaymentMethodId person (Just paymentMethodId) >> pure Success

getPaymentIntentSetup ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.Flow API.Types.UI.RidePayment.SetupIntentResponse
  )
getPaymentIntentSetup (mbPersonId, _) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)

  customerPaymentId <- getCustomerPaymentId person
  ephemeralKey <- Payment.createEphemeralKeys person.merchantId person.merchantOperatingCityId person.paymentMode customerPaymentId
  setupIntent <- Payment.createSetupIntent person.merchantId person.merchantOperatingCityId person.paymentMode customerPaymentId
  return $
    API.Types.UI.RidePayment.SetupIntentResponse
      { setupIntentClientSecret = setupIntent.clientSecret,
        customerId = customerPaymentId,
        ephemeralKey = ephemeralKey
      }

-- TODO: Add the logic to create a payment intent
getPaymentIntentPayment ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.Flow API.Types.UI.RidePayment.PaymentIntentResponse
  )
getPaymentIntentPayment (_mbPersonId, _) = throwError $ InternalError "Not implemented"

postPaymentMethodUpdate ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    PaymentMethodId ->
    Environment.Flow APISuccess
  )
postPaymentMethodUpdate (mbPersonId, _) rideId newPaymentMethodId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let paymentMode = fromMaybe DMPM.LIVE person.paymentMode
  -- check if payment method exists for the customer
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  unless ride.onlinePayment $ throwError (InvalidRequest "Could not update payment method for Cash ride")
  checkIfPaymentMethodExists person newPaymentMethodId >>= \case
    False -> throwError $ InvalidRequest "Payment method doesn't belong to Customer"
    True -> do
      booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
      unless (booking.riderId == personId) $ throwError $ InvalidRequest "Person is not the owner of the ride"
      let bookingPaymentMode = fromMaybe DMPM.LIVE booking.paymentMode
      unless (paymentMode == bookingPaymentMode) $ throwError (InvalidRequest "Invalid payment mode")
  order <- runInReplica $ QPaymentOrder.findById (Kernel.Types.Id.cast rideId) >>= fromMaybeM (InternalError $ "No payment order found for the ride " <> rideId.getId)
  Payment.updatePaymentMethodInIntent person.merchantId person.merchantOperatingCityId person.paymentMode order.paymentServiceOrderId newPaymentMethodId
  Payment.updateDefaultPersonPaymentMethodId person (Just newPaymentMethodId)
  -- Update booking payment method
  return Success

deletePaymentMethodsDelete ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    PaymentMethodId ->
    Environment.Flow APISuccess
  )
deletePaymentMethodsDelete (mbPersonId, _) paymentMethodId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  Payment.deleteCard person.merchantId person.merchantOperatingCityId person.paymentMode paymentMethodId
  return Success

postPaymentAddTip ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    API.Types.UI.RidePayment.AddTipRequest ->
    Environment.Flow APISuccess
  )
postPaymentAddTip (mbPersonId, merchantId) rideId tipRequest = do
  Redis.withWaitOnLockRedisWithExpiry (paymentJobExecLockKey rideId.getId) 10 20 $ do
    personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
    person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
    unless (ride.status == Domain.Types.RideStatus.COMPLETED) $
      throwError $ RideInvalidStatus ("Ride is not completed yet." <> Text.pack (show ride.status))
    booking <- QB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
    unless (booking.riderId == personId) $ throwError $ InvalidRequest "Person is not the owner of the ride"
    unless ride.onlinePayment $ throwError (InvalidRequest "Could not add tip for Cash ride")
    fareBreakups <- runInReplica $ QFareBreakup.findAllByEntityIdAndEntityType rideId.getId Domain.Types.FareBreakup.RIDE
    when (any (\fb -> fb.description == tipFareBreakupTitle) fareBreakups) $ throwError $ InvalidRequest "Tip already added"
    (customerPaymentId, paymentMethodId) <- Payment.getCustomerAndPaymentMethod booking person
    driverAccountId <- ride.driverAccountId & fromMaybeM (RideFieldNotPresent "driverAccountId")
    email <- mapM decrypt person.email
    if ride.paymentStatus == Domain.Types.Ride.NotInitiated
      then do
        -- we will add this tip amount in ride end and charge it in job which is already created in ride end
        let tipAmount = mkPrice (Just tipRequest.amount.currency) tipRequest.amount.amount
        totalFare <- ride.totalFare & fromMaybeM (RideFieldNotPresent "totalFare")
        fareWithTip <- totalFare `addPrice` tipAmount
        let applicationFeeAmount = fromMaybe 0 booking.commission
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
        void $ Payment.makePaymentIntent person.merchantId person.merchantOperatingCityId booking.paymentMode person.id ride createPaymentIntentReq
        QRide.updateTipByRideId (Just tipAmount) rideId -- update tip in ride
      else do
        -- Here we creating a new payment intent for tip if the ride payment status is already initiated
        let createPaymentIntentReq =
              Payment.CreatePaymentIntentReq
                { orderShortId = ride.shortId.getShortId,
                  amount = tipRequest.amount.amount,
                  applicationFeeAmount = applicationFeeAmountForTipAmount tipRequest,
                  currency = tipRequest.amount.currency,
                  customer = customerPaymentId,
                  paymentMethod = paymentMethodId,
                  receiptEmail = email,
                  driverAccountId
                }
        paymentIntentResp <- Payment.makePaymentIntent person.merchantId person.merchantOperatingCityId booking.paymentMode person.id ride createPaymentIntentReq
        void $ Payment.chargePaymentIntent person.merchantId person.merchantOperatingCityId booking.paymentMode paymentIntentResp.paymentIntentId
    createFareBreakup
    merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
    void $ CallBPPInternal.populateTipAmount merchant.driverOfferApiKey merchant.driverOfferBaseUrl ride.bppRideId.getId tipRequest.amount.amount
  return Success
  where
    tipFareBreakupTitle :: Text
    tipFareBreakupTitle = "RIDE_TIP"

    createFareBreakup = do
      id <- generateGUID
      let tipFareBreakup =
            Domain.Types.FareBreakup.FareBreakup
              { id,
                entityId = rideId.getId,
                entityType = Domain.Types.FareBreakup.RIDE,
                description = tipFareBreakupTitle,
                amount = mkPriceFromAPIEntity tipRequest.amount
              }
      QFareBreakup.create tipFareBreakup

paymentJobExecLockKey :: Text -> Text
paymentJobExecLockKey rideId = "PaymentJobExec:RideId-" <> rideId

applicationFeeAmountForRide :: [DFareBreakup] -> HighPrecMoney
applicationFeeAmountForRide fareBreakups = do
  let applicationFeeAmountBreakups = ["INSURANCE_CHARGE", "CARD_CHARGES_ON_FARE", "CARD_CHARGES_FIXED"]
  sum $ map (.amount.amount) $ filter (\fp -> fp.description `elem` applicationFeeAmountBreakups) fareBreakups

applicationFeeAmountForTipAmount :: API.Types.UI.RidePayment.AddTipRequest -> HighPrecMoney
applicationFeeAmountForTipAmount tipRequest = do
  let cardFixedCharges = HighPrecMoney 0.3
  let cardPercentageCharges = 0.029 -- 2.9%
  HighPrecMoney (tipRequest.amount.amount.getHighPrecMoney * cardPercentageCharges) + cardFixedCharges

getPaymentCustomer ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.Flow Kernel.External.Payment.Interface.Types.CreateCustomerResp
  )
getPaymentCustomer (mbPersonId, _) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  getcustomer person
