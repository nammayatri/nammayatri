{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.RidePayment where

import qualified API.Types.UI.RidePayment
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import Kernel.External.Encryption (decrypt)
import Kernel.External.Payment.Interface.Types
import qualified Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Error
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
    Environment.Flow CustomerCardListResp
  )
getPaymentMethods (mbPersonId, _) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  customerPaymentId <- getCustomerPaymentId person
  Payment.getCardList person.merchantId person.merchantOperatingCityId customerPaymentId -- TODO: Add pagination, do we need to store the card details in our DB?

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

-- personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
-- person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)

-- customerPaymentId <- getCustomerPaymentId person
-- ephemeralKey <- Payment.createEphemeralKeys person.merchantId person.merchantOperatingCityId customerPaymentId

-- receiptEmail <- mapM decrypt person.email
-- let req = CreatePaymentIntentReq {amount = 0, currency = USD, customer = customerPaymentId, paymentMethod = "", receiptEmail, driverAccountId = ""}
-- paymentIntent <- Payment.createSetupIntent person.merchantId person.merchantOperatingCityId req

-- return $
--   API.Types.UI.RidePayment.PaymentIntentResponse
--     { paymentIntentClientSecret = setupIntent.clientSecret,
--       customerId = customerPaymentId,
--       ephemeralKey = ephemeralKey
--     }

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
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  paymentIntentId <- ride.paymentIntentId & fromMaybeM (InternalError $ "No payment intent found for the ride " <> rideId.getId)
  Payment.updatePaymentMethodInIntent person.merchantId person.merchantOperatingCityId paymentIntentId newPaymentMethodId
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
  customerId <- getCustomerPaymentId person
  Payment.deleteCard person.merchantId person.merchantOperatingCityId customerId paymentMethodId
  return Success

postPaymentAddTip ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    Environment.Flow APISuccess
  )
postPaymentAddTip = error "Logic yet to be decided"
