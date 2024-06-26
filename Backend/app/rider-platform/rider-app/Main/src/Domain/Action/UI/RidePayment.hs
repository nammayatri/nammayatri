{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.RidePayment where

import qualified API.Types.UI.RidePayment
import Data.Maybe (listToMaybe)
import Data.OpenApi (ToSchema)
import qualified Domain.Types.FareBreakup
import qualified Domain.Types.Merchant
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
import Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Error
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import qualified Lib.Payment.Storage.Queries.PaymentTransaction as QPaymentTransaction
import qualified SharedLogic.Payment as Payment
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Tools.Auth
import qualified Tools.Payment as Payment

getCustomerPaymentId :: Domain.Types.Person.Person -> Environment.Flow CustomerId
getCustomerPaymentId person =
  case person.customerPaymentId of
    Just customerId -> return customerId
    Nothing -> do
      --- Create a customer in payment service if not there ---
      email <- person.email & fromMaybeM (PersonFieldNotPresent "email")
      emailDecrypted <- decrypt email
      phoneDecrypted <- mapM decrypt person.mobileNumber
      let req = Payment.CreateCustomerReq {email = emailDecrypted, name = fromMaybe "User" person.firstName, phone = phoneDecrypted}
      customer <- Payment.createCustomer person.merchantId person.merchantOperatingCityId req
      QPerson.updateCustomerPaymentId (Just customer.customerId) person.id
      return customer.customerId

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
  return $ API.Types.UI.RidePayment.PaymentMethodsResponse {list = resp}

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
  order <- runInReplica $ QPaymentOrder.findById (Kernel.Types.Id.cast rideId) >>= fromMaybeM (InternalError $ "No payment order found for the ride " <> rideId.getId)
  Payment.updatePaymentMethodInIntent person.merchantId person.merchantOperatingCityId order.paymentServiceOrderId newPaymentMethodId
  -- Update booking and person default payment method
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
postPaymentAddTip (mbPersonId, _) rideId tipRequest = do
  Redis.whenWithLockRedis addTipLockKey 60 $ do
    personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
    person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
    unless (ride.status == Domain.Types.Ride.COMPLETED) $
      throwError $ RideInvalidStatus "Ride is not completed yet."
    fareBreakups <- runInReplica $ QFareBreakup.findAllByEntityIdAndEntityType rideId.getId Domain.Types.FareBreakup.RIDE
    if any (\fb -> fb.description == tipFareBreakupTitle) fareBreakups
      then throwError $ InvalidRequest "Tip already added"
      else do
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
    customerPaymentId <- person.customerPaymentId & fromMaybeM (PersonFieldNotPresent "customerPaymentId")
    paymentMethodId <- person.defaultPaymentMethodId & fromMaybeM (PersonFieldNotPresent "defaultPaymentMethodId")
    driverAccountId <- ride.driverAccountId & fromMaybeM (RideFieldNotPresent "driverAccountId")
    email <- mapM decrypt person.email
    -- handle error flow properly
    let createPaymentIntentReq =
          Payment.CreatePaymentIntentReq
            { amount = tipRequest.amount.amount,
              applicationFeeAmount = 0.0,
              currency = tipRequest.amount.currency,
              customer = customerPaymentId,
              paymentMethod = paymentMethodId,
              receiptEmail = email,
              driverAccountId
            }
    paymentIntentResp <- Payment.makePaymentIntent person.merchantId person.merchantOperatingCityId person.id ride createPaymentIntentReq
    transaction <- runInReplica $ QPaymentTransaction.findByTxnId paymentIntentResp.paymentIntentId >>= fromMaybeM (InternalError $ "No transaction found: " <> paymentIntentResp.paymentIntentId)
    Payment.capturePaymentIntent person.merchantId person.merchantOperatingCityId paymentIntentResp.paymentIntentId transaction.amount (HighPrecMoney 0.0)
  return Success
  where
    tipFareBreakupTitle :: Text
    tipFareBreakupTitle = "Post Ride Tip"

    addTipLockKey :: Text
    addTipLockKey = "Driver:AddTip:RideId-" <> rideId.getId
