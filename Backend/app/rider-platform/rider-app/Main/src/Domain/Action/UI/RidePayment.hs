module Domain.Action.UI.RidePayment where

import qualified API.Types.UI.RidePayment
import Data.Maybe (listToMaybe)
import qualified Data.Text as Text
import qualified Domain.Types.FareBreakup
import qualified Domain.Types.Merchant
import qualified Domain.Types.PaymentCustomer as DPaymentCustomer
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
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
  mbCustomer <- QPaymentCustomer.findByCustomerId person.id.getId
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
          getCustomer <- Payment.getCustomer person.merchantId person.merchantOperatingCityId person.id.getId
          QPaymentCustomer.updateCATAndExipry getCustomer.clientAuthToken getCustomer.clientAuthTokenExpiry getCustomer.customerId
          return $ getCustomer
    Nothing -> do
      customer <- Payment.getCustomer person.merchantId person.merchantOperatingCityId person.id.getId
      paymentCustomer <- buildCreateCustomer customer
      QPaymentCustomer.create paymentCustomer
      return $ customer

buildCreateCustomer ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r
  ) =>
  Payment.CreateCustomerResp ->
  m DPaymentCustomer.PaymentCustomer
buildCreateCustomer createCustomerResp = do
  now <- getCurrentTime
  return
    DPaymentCustomer.PaymentCustomer
      { clientAuthToken = createCustomerResp.clientAuthToken,
        clientAuthTokenExpiry = createCustomerResp.clientAuthTokenExpiry,
        customerId = createCustomerResp.customerId,
        createdAt = now,
        updatedAt = now
      }

getCustomerPaymentId :: Domain.Types.Person.Person -> Environment.Flow CustomerId
getCustomerPaymentId person =
  case person.customerPaymentId of
    Just customerId -> return customerId
    Nothing -> do
      --- Create a customer in payment service if not there ---
      mbEmailDecrypted <- mapM decrypt person.email
      phoneDecrypted <- mapM decrypt person.mobileNumber
      let req = Payment.CreateCustomerReq {email = mbEmailDecrypted, name = person.firstName, phone = phoneDecrypted, lastName = Nothing, objectReferenceId = Nothing, mobileCountryCode = Nothing, optionsGetClientAuthToken = Nothing}
      customer <- Payment.createCustomer person.merchantId person.merchantOperatingCityId req
      QPerson.updateCustomerPaymentId (Just customer.customerId) person.id
      return customer.customerId

checkIfPaymentMethodExists :: Domain.Types.Person.Person -> PaymentMethodId -> Environment.Flow Bool
checkIfPaymentMethodExists person paymentMethodId = do
  customerPaymentId <- getCustomerPaymentId person
  cardList <- Payment.getCardList person.merchantId person.merchantOperatingCityId customerPaymentId
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
  resp <- Payment.getCardList person.merchantId person.merchantOperatingCityId customerPaymentId -- TODO: Add pagination, do we need to store the card details in our DB?
  let savedPaymentMethodIds = resp <&> (.cardId)
  when (maybe False (\dpm -> dpm `notElem` savedPaymentMethodIds) person.defaultPaymentMethodId) $ do
    let firstSavedPaymentMethodId = listToMaybe savedPaymentMethodIds
    QPerson.updateDefaultPaymentMethodId firstSavedPaymentMethodId personId
  return $ API.Types.UI.RidePayment.PaymentMethodsResponse {list = resp, defaultPaymentMethodId = person.defaultPaymentMethodId}

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
    True -> QPerson.updateDefaultPaymentMethodId (Just paymentMethodId) personId >> return Success

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
  ephemeralKey <- Payment.createEphemeralKeys person.merchantId person.merchantOperatingCityId customerPaymentId
  setupIntent <- Payment.createSetupIntent person.merchantId person.merchantOperatingCityId customerPaymentId

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
  -- check if payment method exists for the customer
  checkIfPaymentMethodExists person newPaymentMethodId >>= \case
    False -> throwError $ InvalidRequest "Payment method doesn't belong to Customer"
    True -> do
      ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
      booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
      unless (booking.riderId == personId) $ throwError $ InvalidRequest "Person is not the owner of the ride"
  order <- runInReplica $ QPaymentOrder.findById (Kernel.Types.Id.cast rideId) >>= fromMaybeM (InternalError $ "No payment order found for the ride " <> rideId.getId)
  Payment.updatePaymentMethodInIntent person.merchantId person.merchantOperatingCityId order.paymentServiceOrderId newPaymentMethodId
  QPerson.updateDefaultPaymentMethodId (Just newPaymentMethodId) personId
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
  Payment.deleteCard person.merchantId person.merchantOperatingCityId paymentMethodId
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
    unless (ride.status == Domain.Types.Ride.COMPLETED) $
      throwError $ RideInvalidStatus ("Ride is not completed yet." <> Text.pack (show ride.status))
    fareBreakups <- runInReplica $ QFareBreakup.findAllByEntityIdAndEntityType rideId.getId Domain.Types.FareBreakup.RIDE
    when (any (\fb -> fb.description == tipFareBreakupTitle) fareBreakups) $ throwError $ InvalidRequest "Tip already added"
    if ride.paymentStatus == Domain.Types.Ride.NotInitiated
      then do
        -- we will add this tip amount in ride end and charge it in job which is already created in ride end
        QRide.updateTipByRideId (Just $ mkPrice (Just tipRequest.amount.currency) tipRequest.amount.amount) rideId -- update tip in ride
      else do
        -- Here we creating a new payment intent for tip if the ride payment status is already initiated
        customerPaymentId <- person.customerPaymentId & fromMaybeM (PersonFieldNotPresent "customerPaymentId")
        paymentMethodId <- person.defaultPaymentMethodId & fromMaybeM (PersonFieldNotPresent "defaultPaymentMethodId")
        driverAccountId <- ride.driverAccountId & fromMaybeM (RideFieldNotPresent "driverAccountId")
        email <- mapM decrypt person.email
        let createPaymentIntentReq =
              Payment.CreatePaymentIntentReq
                { amount = tipRequest.amount.amount,
                  applicationFeeAmount = applicationFeeAmountForTipAmount tipRequest,
                  currency = tipRequest.amount.currency,
                  customer = customerPaymentId,
                  paymentMethod = paymentMethodId,
                  receiptEmail = email,
                  driverAccountId
                }
        paymentIntentResp <- Payment.makePaymentIntent person.merchantId person.merchantOperatingCityId person.id ride createPaymentIntentReq
        void $ Payment.chargePaymentIntent person.merchantId person.merchantOperatingCityId paymentIntentResp.paymentIntentId
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
