module Domain.Action.UI.RidePayment where

import qualified API.Types.UI.RidePayment
import AWS.S3 as S3
import Data.Maybe (listToMaybe)
import qualified Data.Text as Text
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Domain.Types.Booking
import qualified Domain.Types.FareBreakup
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.PaymentCustomer as DPaymentCustomer
import Domain.Types.PaymentInvoice (PaymentPurpose (RIDE_TIP, TIP))
import qualified Domain.Types.PaymentInvoice as DPI
import qualified Domain.Types.Person
import qualified Domain.Types.RefundRequest as DRefundRequest
import qualified Domain.Types.Ride
import qualified Domain.Types.RideStatus
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payment.Interface
import Kernel.External.Payment.Interface.Types
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.CacheFlow
import Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DPaymentOrder
import qualified Lib.Payment.Domain.Types.PaymentTransaction as DTransaction
import qualified Lib.Payment.Domain.Types.Refunds as DRefunds
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import qualified Lib.Payment.Storage.Queries.PaymentTransaction as QPaymentTransaction
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified SharedLogic.Payment as SPayment
import qualified SharedLogic.PaymentInvoice as SPInvoice
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.PaymentCustomer as QPaymentCustomer
import qualified Storage.Queries.PaymentInvoice as QPaymentInvoice
import qualified Storage.Queries.PaymentInvoiceExtra as QPaymentInvoiceExtra
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RefundRequest as QRefundRequest
import qualified Storage.Queries.Ride as QRide
import Tools.Error
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
    SPayment.updateDefaultPersonPaymentMethodId person firstSavedPaymentMethodId
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
    True -> SPayment.updateDefaultPersonPaymentMethodId person (Just paymentMethodId) >> pure Success

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
  SPayment.updateDefaultPersonPaymentMethodId person (Just newPaymentMethodId)
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
    booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
    unless (booking.riderId == personId) $ throwError $ InvalidRequest "Person is not the owner of the ride"
    unless ride.onlinePayment $ throwError (InvalidRequest "Could not add tip for Cash ride")
    fareBreakups <- runInReplica $ QFareBreakup.findAllByEntityIdAndEntityType rideId.getId Domain.Types.FareBreakup.RIDE
    when (any (\fb -> fb.description == tipFareBreakupTitle) fareBreakups) $ throwError $ InvalidRequest "Tip already added"
    (customerPaymentId, paymentMethodId) <- SPayment.getCustomerAndPaymentMethod booking person
    driverAccountId <- ride.driverAccountId & fromMaybeM (RideFieldNotPresent "driverAccountId")
    email <- mapM decrypt person.email
    if ride.paymentStatus == Domain.Types.Ride.Completed
      then do
        -- Tip added after payment is captured (Completed status)
        -- Create a new payment order for the tip and capture immediately
        let createPaymentIntentServiceReq =
              DPayment.CreatePaymentIntentServiceReq
                { amount = tipRequest.amount.amount,
                  applicationFeeAmount = applicationFeeAmountForTipAmount tipRequest,
                  currency = tipRequest.amount.currency,
                  customer = customerPaymentId,
                  paymentMethod = paymentMethodId,
                  receiptEmail = email,
                  driverAccountId
                }
        -- Create a new payment intent for tip (separate from the main ride payment)
        -- Pass Nothing for existing order ID since tip orders are always new
        tipPaymentIntentResp <- SPayment.makePaymentIntent person.merchantId person.merchantOperatingCityId booking.paymentMode person.id Nothing createPaymentIntentServiceReq
        now <- getCurrentTime
        let tipPaymentOrderId = tipPaymentIntentResp.orderId
        let tipAmount = mkPrice (Just tipRequest.amount.currency) tipRequest.amount.amount
        let paymentMethod = fromMaybe (DMPM.Cash) booking.paymentInstrument
        -- Create tip invoice using unified buildInvoice
        merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
        tipInvoice <- SPInvoice.buildInvoice merchant.shortId rideId (Just tipPaymentOrderId) DPI.PAYMENT TIP DPI.PENDING tipAmount.amount tipAmount.currency paymentMethod booking.merchantId booking.merchantOperatingCityId now
        QPaymentInvoice.create tipInvoice
        -- Update tip amount in ride
        QRide.updateTipByRideId (Just tipAmount) rideId
        -- Immediately attempt to capture the tip payment intent
        tipPaymentCaptured <- SPayment.chargePaymentIntent booking.merchantId booking.merchantOperatingCityId booking.paymentMode tipPaymentIntentResp.paymentIntentId
        if tipPaymentCaptured
          then QPaymentInvoice.updatePaymentStatus DPI.CAPTURED tipInvoice.id
          else do
            -- Mark invoice as FAILED to avoid stuck PENDING invoices
            QPaymentInvoice.updatePaymentStatus DPI.FAILED tipInvoice.id
            logError $ "Failed to capture tip payment intent: " <> tipPaymentIntentResp.paymentIntentId
      else do
        -- Tip added before payment is captured (NotInitiated or Initiated)
        -- Update existing payment intent and invoice
        let tipAmount = mkPrice (Just tipRequest.amount.currency) tipRequest.amount.amount
        totalFare <- ride.totalFare & fromMaybeM (RideFieldNotPresent "totalFare")
        fareWithTip <- totalFare `addPrice` tipAmount
        let applicationFeeAmount = fromMaybe 0 booking.commission
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
        -- Lookup existing invoice to get order ID for retry handling
        mbExistingInvoice <- QPaymentInvoiceExtra.findByRideIdAndTypeAndPurpose rideId DPI.PAYMENT DPI.RIDE
        let mbExistingOrderId = mbExistingInvoice >>= (.paymentOrderId)
        mainPaymentIntentResp <- SPayment.makePaymentIntent person.merchantId person.merchantOperatingCityId booking.paymentMode person.id mbExistingOrderId createPaymentIntentServiceReq
        QRide.updateTipByRideId (Just tipAmount) rideId -- update tip in ride
        -- Update existing invoice to RIDE_TIP purpose and new amount (fare + tip)
        -- Also update invoice number: replace RF (Ride Fare) with TRF (Tip + Ride Fare)
        mbInvoice <- QPaymentInvoice.findByPaymentOrderIdAndInvoiceType (Just mainPaymentIntentResp.orderId) DPI.PAYMENT
        whenJust mbInvoice $ \invoice -> do
          -- Update purpose prefix in invoice number: 190126-NY-RF-PMT-000001 → 190126-NY-TRF-PMT-000001
          let newInvoiceNumber = Text.replace "-RF-" "-TRF-" invoice.invoiceNumber
          let updatedInvoice = invoice{paymentPurpose = RIDE_TIP, amount = fareWithTip.amount, invoiceNumber = newInvoiceNumber}
          QPaymentInvoice.updateByPrimaryKey updatedInvoice
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

postPaymentRefundRequestCreate ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    API.Types.UI.RidePayment.RefundRequestReq ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postPaymentRefundRequestCreate (mbPersonId, _) rideId req = do
  let orderId = Kernel.Types.Id.cast @Domain.Types.Ride.Ride @DPaymentOrder.PaymentOrder rideId
  Redis.whenWithLockRedis (refundRequestProccessingKey orderId) 60 $ do
    mbExistingRefundRequest <- QRefundRequest.findByOrderId orderId
    whenJust mbExistingRefundRequest $ \_ -> throwError (RefundRequestAlreadyExists orderId.getId)
    personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
    ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
    refundPurpose <- case ride.status of
      Domain.Types.RideStatus.COMPLETED -> pure DRefundRequest.RIDE_FARE
      Domain.Types.RideStatus.CANCELLED -> pure DRefundRequest.CANCELLATION_FEE
      _ -> throwError $ RideInvalidStatus ("Refund request available only for COMPLETED or CANCELLED ride. Ride status: " <> show ride.status)
    unless (ride.paymentStatus == Domain.Types.Ride.Completed) $
      throwError (RideInvalidStatus $ "Ride payment is not completed yet. Payment status: " <> show ride.paymentStatus)

    booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
    unless (booking.riderId == personId) $ throwError $ InvalidRequest "Person is not the owner of the ride"
    unless ride.onlinePayment $ throwError (InvalidRequest "Could not refund cash ride")

    -- We need earliest transaction in case if we have more than one transaction (ride fare and ride tip)
    -- Later we can add transactions differentiation based on purpose: RIDE_FARE | RIDE_TIP | CANCELLATION_FEE
    transaction <- QPaymentTransaction.findEarliestChargedTransactionByOrderId orderId >>= fromMaybeM (InvalidRequest "No transaction found for refund")

    whenJust req.requestedAmount $ \requestedAmount -> do
      unless (requestedAmount.currency == transaction.currency) $
        throwError (InvalidRequest "Invalid currency")
      when (requestedAmount.amount > transaction.amount) $
        throwError (InvalidRequest $ "Could not request refund more than transaction amount: " <> show transaction.amount)

    evidenceS3Path <- forM req.evidence $ \evidence -> do
      imageExtension <- validateContentType
      path <- createPath booking.merchantId personId rideId refundPurpose imageExtension
      fork "S3 Put Image" do
        Redis.withLockRedis (imageS3Lock path) 5 $
          S3.put (Text.unpack path) evidence
      pure path
    refundRequest <- buildRefundRequest orderId booking refundPurpose transaction evidenceS3Path req
    QRefundRequest.create refundRequest
    QRide.updateRefundRequestStatus (Just refundRequest.status) rideId
  pure Success
  where
    validateContentType = do
      fileType <- req.fileType & fromMaybeM (InvalidRequest "fileType is required")
      reqContentType <- req.reqContentType & fromMaybeM (InvalidRequest "reqContentType is required")
      case fileType of
        S3.Image -> case reqContentType of
          "image/png" -> pure "png"
          "image/jpeg" -> pure "jpg"
          _ -> throwError $ FileFormatNotSupported reqContentType
        _ -> throwError $ FileFormatNotSupported reqContentType

refundRequestProccessingKey :: Kernel.Types.Id.Id DPaymentOrder.PaymentOrder -> Text
refundRequestProccessingKey orderId = "RefundRequest:Processing:OrderId" <> orderId.getId

imageS3Lock :: Text -> Text
imageS3Lock path = "image-s3-lock-" <> path

buildRefundRequest ::
  Kernel.Types.Id.Id DPaymentOrder.PaymentOrder ->
  Domain.Types.Booking.Booking ->
  DRefundRequest.RefundPurpose ->
  DTransaction.PaymentTransaction ->
  Kernel.Prelude.Maybe Text ->
  API.Types.UI.RidePayment.RefundRequestReq ->
  Environment.Flow DRefundRequest.RefundRequest
buildRefundRequest orderId booking refundPurpose transaction evidenceS3Path API.Types.UI.RidePayment.RefundRequestReq {..} = do
  id <- generateGUID
  now <- getCurrentTime
  pure
    DRefundRequest.RefundRequest
      { requestedAmount = requestedAmount <&> (.amount),
        transactionId = transaction.id,
        transactionAmount = transaction.amount,
        currency = transaction.currency,
        status = DRefundRequest.OPEN,
        responseDescription = Nothing,
        refundsId = Nothing,
        refundsAmount = Nothing, -- should be updated after admin approve
        refundsTries = 0,
        personId = booking.riderId,
        merchantId = booking.merchantId,
        merchantOperatingCityId = booking.merchantOperatingCityId,
        createdAt = now,
        updatedAt = now,
        ..
      }

createPath ::
  (MonadTime m, MonadReader r m, HasField "s3Env" r (S3Env m)) =>
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
  DRefundRequest.RefundPurpose ->
  Text ->
  m Text
createPath merchantId personId rideId refundPurpose imageExtension = do
  pathPrefix <- asks (.s3Env.pathPrefix)
  now <- getCurrentTime
  let fileName = Text.replace (Text.singleton ':') (Text.singleton '-') (Text.pack $ iso8601Show now)
  return
    ( pathPrefix <> "/payment-refunds/" <> "org-" <> merchantId.getId <> "/"
        <> personId.getId
        <> "/"
        <> rideId.getId
        <> "/"
        <> show refundPurpose
        <> "/"
        <> fileName
        <> "."
        <> imageExtension
    )

getPaymentRefundRequest ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Environment.Flow API.Types.UI.RidePayment.RefundRequestResp
  )
getPaymentRefundRequest (mbPersonId, _merchantId) rideId refreshRefunds = do
  let refundRequestInfoHandler =
        RefundRequestInfoHandler
          { validateRefundRequestOwner = \refundRequest -> do
              personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
              unless (refundRequest.personId == personId) $ throwError $ InvalidRequest "Person is not the owner of the ride",
            mkRefundRequestInfoResp = \refundRequest evidence _refundStatus _errorCode -> mkRefundRequestResp refundRequest evidence,
            fetchRefunds = \_refundsId -> pure Nothing, -- required only for admin
            notifyRefunds = \_refundRequest -> pure () -- notification not required for UI api
          }
  fetchPaymentRefundRequestInfo @API.Types.UI.RidePayment.RefundRequestResp refundRequestInfoHandler refreshRefunds rideId

mkRefundRequestResp ::
  DRefundRequest.RefundRequest ->
  Maybe Text ->
  API.Types.UI.RidePayment.RefundRequestResp
mkRefundRequestResp DRefundRequest.RefundRequest {..} evidence =
  API.Types.UI.RidePayment.RefundRequestResp
    { rideId = Kernel.Types.Id.cast @DPaymentOrder.PaymentOrder @Domain.Types.Ride.Ride orderId,
      refundsAmount = flip PriceAPIEntity currency <$> refundsAmount,
      requestedAmount = flip PriceAPIEntity currency <$> requestedAmount,
      transactionAmount = PriceAPIEntity transactionAmount currency,
      ..
    }

data RefundRequestInfoHandler resp = RefundRequestInfoHandler
  { validateRefundRequestOwner :: DRefundRequest.RefundRequest -> Environment.Flow (),
    mkRefundRequestInfoResp :: DRefundRequest.RefundRequest -> Maybe Text -> Maybe Kernel.External.Payment.Interface.RefundStatus -> Maybe Text -> resp,
    fetchRefunds :: Kernel.Types.Id.Id DRefunds.Refunds -> Environment.Flow (Maybe DRefunds.Refunds), -- required only for error code and status
    notifyRefunds :: DRefundRequest.RefundRequest -> Environment.Flow ()
  }

fetchPaymentRefundRequestInfo ::
  forall resp.
  RefundRequestInfoHandler resp ->
  Maybe Bool ->
  Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
  Environment.Flow resp
fetchPaymentRefundRequestInfo h refreshRefunds rideId = do
  let orderId = Kernel.Types.Id.cast @Domain.Types.Ride.Ride @DPaymentOrder.PaymentOrder rideId
  eResult <- Redis.whenWithLockRedisAndReturnValue (refundRequestProccessingKey orderId) 60 $ do
    refundRequest <- QRefundRequest.findByOrderId orderId >>= fromMaybeM (RefundRequestDoesNotExist orderId.getId)
    h.validateRefundRequestOwner refundRequest
    evidence <- fetchEvidenceFromS3 refundRequest
    if refreshRefunds == Just True
      then do
        -- refresh is possible only if refund object already created on Stripe side and in refunds db table
        ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
        booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
        driverAccountId <- ride.driverAccountId & fromMaybeM (RideFieldNotPresent "driverAccountId")
        let refreshStripeRefundReq =
              DPayment.RefreshStripeRefundReq
                { orderId = refundRequest.orderId,
                  driverAccountId
                }
        result <- SPayment.refreshStripeRefund refundRequest.merchantId refundRequest.merchantOperatingCityId booking.paymentMode refreshStripeRefundReq
        let updStatus = castRefundRequestStatus result.status
        when (refundRequest.refundsId /= Just result.refundsId || refundRequest.status /= updStatus) $
          QRefundRequest.updateRefundIdAndStatus (Just result.refundsId) updStatus refundRequest.id
        let updRefundRequest = refundRequest{refundsId = Just result.refundsId, status = updStatus}
        when (refundRequest.status /= updStatus) $ do
          QRide.updateRefundRequestStatus (Just updRefundRequest.status) rideId
          -- Update refund invoice status using purpose from refund request
          let paymentPurpose = SPInvoice.refundPurposeToPaymentPurpose refundRequest.refundPurpose
              invoiceStatus = SPInvoice.refundStatusToInvoiceStatus result.status
          QPaymentInvoiceExtra.updatePaymentStatusByRideIdAndTypeAndPurpose rideId DPI.REFUNDS paymentPurpose invoiceStatus
          h.notifyRefunds updRefundRequest
        pure $ h.mkRefundRequestInfoResp updRefundRequest evidence (Just result.status) result.errorCode
      else do
        mbRefunds <- case refundRequest.refundsId of
          Just refundsId -> h.fetchRefunds refundsId
          Nothing -> pure Nothing
        pure $ h.mkRefundRequestInfoResp refundRequest evidence (mbRefunds <&> (.status)) (mbRefunds >>= (.errorCode))

  case eResult of
    Left () -> do
      logError $ "Order refund locked: " <> orderId.getId
      throwError (InvalidRequest "Order refund locked")
    Right result -> return result

castRefundRequestStatus :: Payment.RefundStatus -> DRefundRequest.RefundRequestStatus
castRefundRequestStatus = \case
  Payment.REFUND_PENDING -> DRefundRequest.APPROVED -- did not changed
  Payment.REFUND_FAILURE -> DRefundRequest.FAILED
  Payment.REFUND_SUCCESS -> DRefundRequest.REFUNDED
  Payment.MANUAL_REVIEW -> DRefundRequest.APPROVED -- did not changed
  Payment.REFUND_CANCELED -> DRefundRequest.FAILED
  Payment.REFUND_REQUIRES_ACTION -> DRefundRequest.APPROVED -- did not changed

fetchEvidenceFromS3 :: DRefundRequest.RefundRequest -> Environment.Flow (Maybe Text)
fetchEvidenceFromS3 refundRequest = forM refundRequest.evidenceS3Path $ \evidenceS3Path -> do
  eImage <-
    Redis.whenWithLockRedisAndReturnValue (imageS3Lock evidenceS3Path) 60 $
      S3.get $ Text.unpack evidenceS3Path
  case eImage of
    Left _err -> do
      logError $ "Evidence image locked: " <> refundRequest.orderId.getId
      throwError (InvalidRequest "Evidence image locked")
    Right image -> pure image
