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
import qualified Kernel.External.Payment.Interface as PaymentInterface
import Kernel.External.Payment.Interface.Types
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.CacheFlow
import Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Error
import qualified Lib.Payment.Domain.Types.PaymentOrder as DPaymentOrder
import qualified Lib.Payment.Domain.Types.PaymentTransaction as DPaymentTransaction
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import qualified Lib.Payment.Storage.Queries.PaymentTransaction as QPaymentTransaction
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified SharedLogic.Payment as Payment
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.PaymentCustomer as QPaymentCustomer
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Tools.Payment as TPayment

data DFareBreakup = DFareBreakup
  { amount :: Price,
    description :: Text
  }
  deriving (Generic, Show)

getcustomer ::
  Domain.Types.Person.Person -> Environment.Flow TPayment.CreateCustomerResp
getcustomer person = do
  let paymentMode = fromMaybe DMPM.LIVE person.paymentMode
  mbCustomer <- QPaymentCustomer.findByCustomerIdAndPaymentMode person.id.getId (Just paymentMode)
  case mbCustomer of
    Just customer -> do
      now <- getCurrentTime
      if maybe False (> now) customer.clientAuthTokenExpiry
        then
          return $
            TPayment.CreateCustomerResp
              { customerId = customer.customerId,
                clientAuthToken = customer.clientAuthToken,
                clientAuthTokenExpiry = customer.clientAuthTokenExpiry
              }
        else do
          getCustomer <- TPayment.getCustomer person.merchantId person.merchantOperatingCityId person.paymentMode person.id.getId
          QPaymentCustomer.updateCATAndExipry getCustomer.clientAuthToken getCustomer.clientAuthTokenExpiry getCustomer.customerId (Just paymentMode)
          return $ getCustomer
    Nothing -> do
      customer <- TPayment.getCustomer person.merchantId person.merchantOperatingCityId person.paymentMode person.id.getId
      paymentCustomer <- buildCreateCustomer customer paymentMode
      QPaymentCustomer.create paymentCustomer
      return customer

buildCreateCustomer ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r
  ) =>
  TPayment.CreateCustomerResp ->
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
      let req = CreateCustomerReq {email = mbEmailDecrypted, name = person.firstName, phone = phoneDecrypted, lastName = Nothing, objectReferenceId = Nothing, mobileCountryCode = Nothing, optionsGetClientAuthToken = Nothing}
      customer <- TPayment.createCustomer person.merchantId person.merchantOperatingCityId person.paymentMode req
      case paymentMode of
        DMPM.LIVE -> QPerson.updateCustomerPaymentId (Just customer.customerId) person.id
        DMPM.TEST -> QPerson.updateTestCustomerPaymentId (Just customer.customerId) person.id
      return customer.customerId

checkIfPaymentMethodExists :: Domain.Types.Person.Person -> PaymentMethodId -> Environment.Flow Bool
checkIfPaymentMethodExists person paymentMethodId = do
  customerPaymentId <- getCustomerPaymentId person
  cardList <- TPayment.getCardList person.merchantId person.merchantOperatingCityId person.paymentMode customerPaymentId
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
  resp <- TPayment.getCardList person.merchantId person.merchantOperatingCityId person.paymentMode customerPaymentId -- TODO: Add pagination, do we need to store the card details in our DB?
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
  ephemeralKey <- TPayment.createEphemeralKeys person.merchantId person.merchantOperatingCityId person.paymentMode customerPaymentId
  setupIntent <- TPayment.createSetupIntent person.merchantId person.merchantOperatingCityId person.paymentMode customerPaymentId
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
  TPayment.updatePaymentMethodInIntent person.merchantId person.merchantOperatingCityId person.paymentMode order.paymentServiceOrderId newPaymentMethodId
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
  TPayment.deleteCard person.merchantId person.merchantOperatingCityId person.paymentMode paymentMethodId
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
              PaymentInterface.CreatePaymentIntentReq
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
              PaymentInterface.CreatePaymentIntentReq
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

-- | Internal data type for single pending ride payment info
-- Note: With the new approach, only one ride can have pending payment at a time
-- because we capture pending payments before allowing a new ride
data PendingPaymentInfo = PendingPaymentInfo
  { ride :: Domain.Types.Ride.Ride,
    paymentOrder :: DPaymentOrder.PaymentOrder,
    applicationFeeAmount :: HighPrecMoney
  }

-- | Get the single pending payment for a rider (if any)
-- With the new approach, only one ride can have pending payment at a time
getPendingPayment :: Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.Flow (Maybe PendingPaymentInfo)
getPendingPayment personId = do
  -- Get rides with payment status Initiated for this rider (should be at most 1)
  ridesWithInitiatedPayment <- runInReplica $ QRide.findRidesWithPaymentStatusInitiated personId
  case ridesWithInitiatedPayment of
    [] -> pure Nothing
    (ride : _) -> checkForPendingPayment ride
  where
    checkForPendingPayment :: Domain.Types.Ride.Ride -> Environment.Flow (Maybe PendingPaymentInfo)
    checkForPendingPayment ride = do
      -- PaymentOrder.id = ride.id (cast)
      let paymentOrderId = Kernel.Types.Id.cast ride.id
      mbOrder <- runInReplica $ QPaymentOrder.findById paymentOrderId
      case mbOrder of
        Nothing -> pure Nothing -- No payment order exists
        Just order -> do
          -- Skip if already charged
          if order.status == PaymentInterface.CHARGED
            then pure Nothing
            else do
              -- Get only the latest transaction for this order
              mbLatestTransaction <- runInReplica $ QPaymentTransaction.findLatestByOrderId order.id
              if isPaymentPending mbLatestTransaction
                then do
                  -- Extract applicationFeeAmount from transaction, default to 0 if no transaction
                  let appFeeAmount = maybe 0 (.applicationFeeAmount) mbLatestTransaction
                  pure $
                    Just
                      PendingPaymentInfo
                        { ride = ride,
                          paymentOrder = order,
                          applicationFeeAmount = appFeeAmount
                        }
                else pure Nothing

    -- Check if the payment is pending/failed:
    -- 1. No transactions exist (payment never initiated)
    -- 2. Latest transaction is CANCELLED with retryCount >= 2 and bankErrorCode is not null
    isPaymentPending :: Maybe DPaymentTransaction.PaymentTransaction -> Bool
    isPaymentPending Nothing = True -- No transaction exists
    isPaymentPending (Just latestTxn) =
      latestTxn.status == PaymentInterface.CANCELLED
        && latestTxn.retryCount >= 2
        && isJust latestTxn.bankErrorCode

-- | Get pending payment (if any) for the authenticated rider
-- Note: With the new approach, only one ride can have pending payment at a time
getPaymentGetDueAmount ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.Flow API.Types.UI.RidePayment.GetDueAmountResp
  )
getPaymentGetDueAmount (mbPersonId, _) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  mbPendingPayment <- getPendingPayment personId
  case mbPendingPayment of
    Nothing ->
      pure $
        API.Types.UI.RidePayment.GetDueAmountResp
          { rides = [],
            totalDueAmount = 0,
            currency = INR -- Default currency when no dues
          }
    Just pendingInfo ->
      pure $
        API.Types.UI.RidePayment.GetDueAmountResp
          { rides =
              [ API.Types.UI.RidePayment.DueAmountRide
                  { rideId = pendingInfo.ride.id,
                    amount = pendingInfo.paymentOrder.amount
                  }
              ],
            totalDueAmount = pendingInfo.paymentOrder.amount,
            currency = pendingInfo.paymentOrder.currency
          }

-- | Clear pending dues for the authenticated rider
-- Uses the existing PaymentOrder (created at ride end) and ride's driverAccountId for split settlement
postPaymentClearDues ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.RidePayment.ClearDuesReq ->
    Environment.Flow API.Types.UI.RidePayment.ClearDuesResp
  )
postPaymentClearDues (mbPersonId, _merchantId) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)

  -- 1. Get the single pending payment (if any)
  mbPendingPayment <- getPendingPayment personId

  pendingInfo <- mbPendingPayment & fromMaybeM (InvalidRequest "No pending dues to clear")

  let ride = pendingInfo.ride
  let existingOrder = pendingInfo.paymentOrder

  -- 2. Get/validate payment method (before lock to fail early)
  paymentMethodId <- case req.paymentMethodId of
    Just pmId -> do
      checkIfPaymentMethodExists person pmId >>= \case
        False -> throwError $ InvalidRequest "Payment method doesn't belong to Customer"
        True -> return pmId
    Nothing -> getDefaultPaymentMethodForDues person

  -- 3. Get driver account ID from the ride for proper split settlement
  _driverAccountId <- ride.driverAccountId & fromMaybeM (RideFieldNotPresent "driverAccountId")

  -- Use IORef to store the result from inside the lock
  resultRef <- liftIO $ newIORef Nothing

  -- Use Redis lock to prevent concurrent execution with scheduler
  Redis.withWaitOnLockRedisWithExpiry (paymentJobExecLockKey ride.id.getId) 10 20 $ do
    -- 4. Update payment method in existing payment intent
    TPayment.updatePaymentMethodInIntent person.merchantId person.merchantOperatingCityId person.paymentMode existingOrder.paymentServiceOrderId paymentMethodId

    -- 5. Create a new PaymentTransaction for this retry attempt
    now <- getCurrentTime
    transaction <- buildRetryTransaction existingOrder pendingInfo.applicationFeeAmount now
    QPaymentTransaction.create transaction

    -- 6. Capture payment immediately with proper split settlement to driver
    captureResult <- try $ TPayment.capturePaymentIntent person.merchantId person.merchantOperatingCityId person.paymentMode existingOrder.paymentServiceOrderId existingOrder.amount pendingInfo.applicationFeeAmount

    result <- case captureResult of
      Right () -> do
        -- Payment successful
        -- Update transaction and order status
        QPaymentTransaction.updateStatusAndError transaction.id PaymentInterface.CHARGED Nothing Nothing
        QPaymentOrder.updateStatus existingOrder.id existingOrder.paymentServiceOrderId PaymentInterface.CHARGED

        -- Mark payment_status as Completed for the ride
        QRide.markPaymentStatus Domain.Types.Ride.Completed ride.id

        -- Update default payment method
        Payment.updateDefaultPersonPaymentMethodId person (Just paymentMethodId)

        pure $
          API.Types.UI.RidePayment.ClearDuesResp
            { orderId = Just existingOrder.id,
              status = "SUCCESS",
              amountCleared = existingOrder.amount,
              currency = existingOrder.currency,
              ridesCleared = [ride.id],
              errorMessage = Nothing
            }
      Left (err :: SomeException) -> do
        -- Payment failed
        let errMessage = Just $ show err
        QPaymentTransaction.updateStatusAndError transaction.id PaymentInterface.CANCELLED Nothing errMessage

        pure $
          API.Types.UI.RidePayment.ClearDuesResp
            { orderId = Just existingOrder.id,
              status = "FAILED",
              amountCleared = 0,
              currency = existingOrder.currency,
              ridesCleared = [],
              errorMessage = errMessage
            }

    liftIO $ writeIORef resultRef (Just result)

  -- Read the result from the IORef
  mbResult <- liftIO $ readIORef resultRef
  mbResult & fromMaybeM (InternalError "Payment operation did not complete")
  where
    getDefaultPaymentMethodForDues :: Domain.Types.Person.Person -> Environment.Flow PaymentMethodId
    getDefaultPaymentMethodForDues p = do
      let paymentMode = fromMaybe DMPM.LIVE p.paymentMode
      let mbDefaultPmId = case paymentMode of
            DMPM.LIVE -> p.defaultPaymentMethodId
            DMPM.TEST -> p.defaultTestPaymentMethodId
      mbDefaultPmId & fromMaybeM (InvalidRequest "No default payment method found. Please provide a payment method.")

    buildRetryTransaction ::
      DPaymentOrder.PaymentOrder ->
      HighPrecMoney ->
      Kernel.Prelude.UTCTime ->
      Environment.Flow DPaymentTransaction.PaymentTransaction
    buildRetryTransaction order appFeeAmount now' = do
      txnId <- generateGUID
      -- Get the latest transaction to determine retry count
      mbLatestTxn <- runInReplica $ QPaymentTransaction.findLatestByOrderId order.id
      let retryCount = maybe 0 (\t -> t.retryCount + 1) mbLatestTxn
      pure
        DPaymentTransaction.PaymentTransaction
          { id = Kernel.Types.Id.Id txnId,
            txnUUID = Just order.paymentServiceOrderId,
            txnId = Just order.paymentServiceOrderId,
            paymentMethodType = Nothing,
            paymentMethod = Nothing,
            respMessage = Nothing,
            respCode = Nothing,
            gatewayReferenceId = Nothing,
            orderId = order.id,
            merchantId = order.merchantId,
            amount = order.amount,
            applicationFeeAmount = appFeeAmount,
            retryCount = retryCount,
            currency = order.currency,
            dateCreated = Nothing,
            statusId = 0,
            status = PaymentInterface.STARTED,
            juspayResponse = Nothing,
            mandateStatus = Nothing,
            mandateStartDate = Nothing,
            mandateEndDate = Nothing,
            mandateId = Nothing,
            bankErrorMessage = Nothing,
            bankErrorCode = Nothing,
            mandateFrequency = Nothing,
            mandateMaxAmount = Nothing,
            splitSettlementResponse = Nothing,
            createdAt = now',
            updatedAt = now',
            merchantOperatingCityId = order.merchantOperatingCityId
          }
