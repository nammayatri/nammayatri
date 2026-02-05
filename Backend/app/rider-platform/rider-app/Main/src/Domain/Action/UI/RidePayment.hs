module Domain.Action.UI.RidePayment where

import qualified API.Types.UI.RidePayment
import AWS.S3 as S3
import Data.Maybe (listToMaybe)
import qualified Data.Text as Text
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Domain.Types.Booking
import qualified Domain.Types.FareBreakup
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
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
import qualified Kernel.External.Payment.Interface as PaymentInterface
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
import qualified Lib.Payment.Domain.Types.PaymentTransaction as DPaymentTransaction
import qualified Lib.Payment.Domain.Types.Refunds as DRefunds
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import qualified Lib.Payment.Storage.Queries.PaymentTransaction as QPaymentTransaction
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified SharedLogic.Payment as SPayment
import qualified SharedLogic.PaymentInvoice as SPInvoice
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.PaymentCustomer as QPaymentCustomer
import qualified Storage.Queries.PaymentInvoice as QPaymentInvoice
import qualified Storage.Queries.PaymentInvoiceExtra as QPaymentInvoiceExtra
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RefundRequest as QRefundRequest
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Payment as TPayment

data DFareBreakup = DFareBreakup
  { amount :: Price,
    description :: Text
  }
  deriving (Generic, Show)

-- | Filter invoices to find pending dues: FAILED PAYMENT invoices that are not settled and not excluded
filterPendingDuesInvoices :: [DPI.PaymentInvoice] -> [DPI.PaymentPurpose] -> [DPI.PaymentInvoice]
filterPendingDuesInvoices allInvoices excludedPurposes =
  allInvoices
    & filter (\inv -> inv.invoiceType == DPI.PAYMENT && inv.paymentStatus == DPI.FAILED)
    & filter (\inv -> inv.paymentPurpose `notElem` excludedPurposes)
    & filter (\inv -> isNothing inv.settledByInvoiceId)
    & filter (\inv -> inv.paymentPurpose /= DPI.DEBT_SETTLEMENT)

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
  DPaymentTransaction.PaymentTransaction ->
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
    mkRefundRequestInfoResp :: DRefundRequest.RefundRequest -> Maybe Text -> Maybe PaymentInterface.RefundStatus -> Maybe Text -> resp,
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

castRefundRequestStatus :: TPayment.RefundStatus -> DRefundRequest.RefundRequestStatus
castRefundRequestStatus = \case
  TPayment.REFUND_PENDING -> DRefundRequest.APPROVED -- did not changed
  TPayment.REFUND_FAILURE -> DRefundRequest.FAILED
  TPayment.REFUND_SUCCESS -> DRefundRequest.REFUNDED
  TPayment.MANUAL_REVIEW -> DRefundRequest.APPROVED -- did not changed
  TPayment.REFUND_CANCELED -> DRefundRequest.FAILED
  TPayment.REFUND_REQUIRES_ACTION -> DRefundRequest.APPROVED -- did not changed

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

-- | Internal data type for single pending ride payment info
-- Note: With the new approach, only one ride can have pending payment at a time
-- because we capture pending payments before allowing a new ride
-- | SIMPLIFIED: Get pending dues for authenticated rider using invoice-based approach
-- Returns dues from FAILED invoices on most recent ride, excluding TIP and already-settled invoices
getPaymentGetDueAmount ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.Flow API.Types.UI.RidePayment.GetDueAmountResp
  )
getPaymentGetDueAmount (mbPersonId, _) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  riderConfig <- runInReplica $ QRC.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing
  let excludedPurposes = case riderConfig >>= (.duesExcludedPaymentPurposes) of
        Nothing -> [] -- No config = don't exclude anything
        Just textList -> mapMaybe (readMaybe . Text.unpack) textList :: [DPI.PaymentPurpose]

  -- 1. Find most recent ride for this rider
  mbLatestRide <- runInReplica $ QRide.findMostRecentRideForRider personId

  case mbLatestRide of
    Nothing -> returnNoDues personId
    Just ride -> do
      -- 2. Get all invoices for this ride and filter for pending dues
      allInvoices <- runInReplica $ QPaymentInvoice.findAllByRideId ride.id
      let filteredInvoices = filterPendingDuesInvoices allInvoices excludedPurposes

      -- Audit logging for debugging
      let failedPaymentInvoices = filter (\inv -> inv.invoiceType == DPI.PAYMENT && inv.paymentStatus == DPI.FAILED) allInvoices
      let numSettled = length $ filter (isJust . (.settledByInvoiceId)) failedPaymentInvoices
      when (numSettled > 0 || length failedPaymentInvoices /= length filteredInvoices) $
        logInfo $
          "Get Dues filtering for ride " <> ride.id.getId <> ": "
            <> "failed="
            <> show (length failedPaymentInvoices)
            <> ", settled="
            <> show numSettled
            <> ", final_due="
            <> show (length filteredInvoices)

      -- 4. Calculate total
      if null filteredInvoices
        then returnNoDues personId
        else do
          let totalAmount = sum $ map (.amount) filteredInvoices
          let currency = Kernel.Prelude.head filteredInvoices & (.currency)
          logInfo $
            "AUDIT: Get Dues - person_id: " <> personId.getId
              <> ", ride_id: "
              <> ride.id.getId
              <> ", due_invoices: "
              <> show (length filteredInvoices)
              <> ", total_amount: "
              <> show totalAmount
          pure $
            API.Types.UI.RidePayment.GetDueAmountResp
              { rides = [API.Types.UI.RidePayment.DueAmountRide {rideId = ride.id, amount = totalAmount}],
                totalDueAmount = totalAmount,
                currency = Just currency
              }
  where
    returnNoDues _ = do
      -- No currency when no dues (avoids sending a wrong default like INR)
      pure $
        API.Types.UI.RidePayment.GetDueAmountResp
          { rides = [],
            totalDueAmount = 0,
            currency = Nothing
          }

-- | SIMPLIFIED: Clear pending dues by creating NEW payment order and DEBT_SETTLEMENT invoice
-- Creates a new order for the total due amount and settles parent invoices on success
postPaymentClearDues ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.RidePayment.ClearDuesReq ->
    Environment.Flow API.Types.UI.RidePayment.ClearDuesResp
  )
postPaymentClearDues (mbPersonId, merchantId) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)

  -- 1. Re-fetch dues using Get Dues logic (DRY principle)
  duesResp <- getPaymentGetDueAmount (mbPersonId, merchantId)
  when (duesResp.totalDueAmount <= 0 || null duesResp.rides) $
    throwError $ InvalidRequest "No pending dues to clear"
  currency <- duesResp.currency & fromMaybeM (InvalidRequest "Currency required when dues present")

  -- 2. Get failed invoices to link as parents (same exclusion as Get Dues: config-based)
  riderConfig <- runInReplica $ QRC.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing
  let excludedPurposes = case riderConfig >>= (.duesExcludedPaymentPurposes) of
        Nothing -> [] -- No config = don't exclude anything
        Just textList -> mapMaybe (readMaybe . Text.unpack) textList :: [DPI.PaymentPurpose]
  let rideId = Kernel.Prelude.head duesResp.rides & (.rideId)
  ride <- QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  allInvoices <- QPaymentInvoice.findAllByRideId rideId
  let parentInvoices = filterPendingDuesInvoices allInvoices excludedPurposes

  -- 3. Validate and get payment details
  paymentMethodId <- case req.paymentMethodId of
    Just pmId -> do
      checkIfPaymentMethodExists person pmId >>= \case
        False -> throwError $ InvalidRequest "Payment method doesn't belong to Customer"
        True -> return pmId
    Nothing -> getDefaultPaymentMethodForDues person

  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  (customerPaymentId, _) <- SPayment.getCustomerAndPaymentMethod booking person
  driverAccountId <- ride.driverAccountId & fromMaybeM (RideFieldNotPresent "driverAccountId")
  email <- mapM decrypt person.email

  -- 4. Create NEW payment order for debt settlement
  let createPaymentIntentServiceReq =
        DPayment.CreatePaymentIntentServiceReq
          { amount = duesResp.totalDueAmount,
            applicationFeeAmount = 0, -- No app fee for debt settlement
            currency = currency,
            customer = customerPaymentId,
            paymentMethod = paymentMethodId,
            receiptEmail = email,
            driverAccountId
          }

  paymentIntentResp <-
    SPayment.makePaymentIntent
      person.merchantId booking.merchantOperatingCityId booking.paymentMode
      person.id Nothing createPaymentIntentServiceReq

  -- 5. Create DEBT_SETTLEMENT invoice with parent links
  now <- getCurrentTime
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  let paymentMethod = fromMaybe DMPM.Cash booking.paymentInstrument

  debtSettlementInvoice <-
    buildDebtSettlementInvoice
      merchant.shortId
      rideId
      (Just paymentIntentResp.orderId)
      duesResp.totalDueAmount
      currency
      paymentMethod
      (map (.id) parentInvoices) -- parent_invoice_ids
      booking.merchantId booking.merchantOperatingCityId now

  -- Audit log: Creating DEBT_SETTLEMENT invoice
  logInfo $
    "Creating DEBT_SETTLEMENT invoice " <> debtSettlementInvoice.id.getId <> " for ride " <> rideId.getId
      <> ", amount: "
      <> show duesResp.totalDueAmount
      <> ", parent_invoices: "
      <> show (map ((.getId) . (.id)) parentInvoices)

  QPaymentInvoice.create debtSettlementInvoice

  -- 6. Attempt capture
  captureResult <-
    try $
      SPayment.chargePaymentIntent
        person.merchantId booking.merchantOperatingCityId person.paymentMode
        paymentIntentResp.paymentIntentId

  case captureResult of
    Right True -> do
      -- Success: update invoice and settle parents
      QPaymentInvoice.updatePaymentStatus DPI.CAPTURED debtSettlementInvoice.id
      QPaymentInvoiceExtra.updateSettledByForInvoices (map (.id) parentInvoices) debtSettlementInvoice.id

      -- Audit log: Settlement successful
      logInfo $
        "AUDIT: Debt settlement CAPTURED - invoice_id: " <> debtSettlementInvoice.id.getId
          <> ", order_id: "
          <> paymentIntentResp.orderId.getId
          <> ", amount: "
          <> show duesResp.totalDueAmount
          <> ", ride_id: "
          <> rideId.getId
      logInfo $
        "AUDIT: Marked " <> show (length parentInvoices) <> " parent invoice(s) as settled: "
          <> show (map ((.getId) . (.id)) parentInvoices)

      pure $
        API.Types.UI.RidePayment.ClearDuesResp
          { orderId = Just paymentIntentResp.orderId,
            status = API.Types.UI.RidePayment.SUCCESS,
            amountCleared = duesResp.totalDueAmount,
            currency = currency,
            ridesCleared = [rideId],
            errorMessage = Nothing
          }
    Right False -> do
      QPaymentInvoice.updatePaymentStatus DPI.FAILED debtSettlementInvoice.id
      logError $
        "AUDIT: Debt settlement FAILED - invoice_id: " <> debtSettlementInvoice.id.getId
          <> ", order_id: "
          <> paymentIntentResp.orderId.getId
          <> ", amount: "
          <> show duesResp.totalDueAmount
          <> ", ride_id: "
          <> rideId.getId
          <> " (parent invoices remain unsettled)"
      pure $
        API.Types.UI.RidePayment.ClearDuesResp
          { orderId = Just paymentIntentResp.orderId,
            status = API.Types.UI.RidePayment.FAILED,
            amountCleared = 0,
            currency = currency,
            ridesCleared = [],
            errorMessage = Just "Payment failed. Please try again."
          }
    Left (err :: SomeException) -> do
      QPaymentInvoice.updatePaymentStatus DPI.FAILED debtSettlementInvoice.id
      let userFriendlyMessage = mapPaymentErrorToUserMessage (show err)
      logError $
        "AUDIT: Debt settlement EXCEPTION - invoice_id: " <> debtSettlementInvoice.id.getId
          <> ", order_id: "
          <> paymentIntentResp.orderId.getId
          <> ", ride_id: "
          <> rideId.getId
          <> ", error: "
          <> show err
          <> " (parent invoices remain unsettled)"
      pure $
        API.Types.UI.RidePayment.ClearDuesResp
          { orderId = Just paymentIntentResp.orderId,
            status = API.Types.UI.RidePayment.FAILED,
            amountCleared = 0,
            currency = currency,
            ridesCleared = [],
            errorMessage = Just userFriendlyMessage
          }
  where
    getDefaultPaymentMethodForDues :: Domain.Types.Person.Person -> Environment.Flow PaymentMethodId
    getDefaultPaymentMethodForDues p = do
      let paymentMode = fromMaybe DMPM.LIVE p.paymentMode
      let mbDefaultPmId = case paymentMode of
            DMPM.LIVE -> p.defaultPaymentMethodId
            DMPM.TEST -> p.defaultTestPaymentMethodId
      mbDefaultPmId & fromMaybeM (InvalidRequest "No default payment method found. Please provide a payment method.")

    buildDebtSettlementInvoice ::
      Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
      Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
      Maybe (Kernel.Types.Id.Id DPaymentOrder.PaymentOrder) ->
      HighPrecMoney ->
      Currency ->
      DMPM.PaymentInstrument ->
      [Kernel.Types.Id.Id DPI.PaymentInvoice] ->
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
      Kernel.Types.Id.Id DMOC.MerchantOperatingCity ->
      Kernel.Prelude.UTCTime ->
      Environment.Flow DPI.PaymentInvoice
    buildDebtSettlementInvoice merchantShortId rideId' paymentOrderId amount currency paymentInstrument parentInvoiceIds merchantId' merchantOperatingCityId' now' = do
      invoiceId <- generateGUID
      invoiceNumber <- SPInvoice.generateInvoiceNumber merchantShortId DPI.DEBT_SETTLEMENT DPI.PAYMENT now'
      pure
        DPI.PaymentInvoice
          { id = Kernel.Types.Id.Id invoiceId,
            rideId = rideId',
            paymentOrderId = paymentOrderId,
            paymentStatus = DPI.PENDING,
            paymentInstrument = paymentInstrument,
            paymentPurpose = DPI.DEBT_SETTLEMENT,
            invoiceType = DPI.PAYMENT,
            invoiceNumber = invoiceNumber,
            amount = amount,
            currency = currency,
            parentInvoiceIds = Just parentInvoiceIds,
            settledByInvoiceId = Nothing,
            merchantId = Just merchantId',
            merchantOperatingCityId = Just merchantOperatingCityId',
            createdAt = now',
            updatedAt = now'
          }

    mapPaymentErrorToUserMessage :: Text -> Text
    mapPaymentErrorToUserMessage errMsg
      | "insufficient" `Text.isInfixOf` Text.toLower errMsg = "Payment failed due to insufficient funds. Please check your payment method."
      | "declined" `Text.isInfixOf` Text.toLower errMsg = "Payment was declined by your bank. Please try a different payment method."
      | "expired" `Text.isInfixOf` Text.toLower errMsg = "Payment method has expired. Please update your payment method."
      | "invalid" `Text.isInfixOf` Text.toLower errMsg = "Invalid payment method. Please check your payment details."
      | "timeout" `Text.isInfixOf` Text.toLower errMsg = "Payment request timed out. Please try again."
      | "network" `Text.isInfixOf` Text.toLower errMsg = "Network error occurred. Please check your connection and try again."
      | otherwise = "Payment failed. Please try again or contact support if the issue persists."

-- | Capture a PENDING invoice by ID. Updates invoice and ride (and payment order via gateway flow).
-- Returns error if invoice not found, not PENDING, or caller is not the ride owner.
postPaymentInvoiceCapture ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id DPI.PaymentInvoice ->
    Environment.Flow APISuccess
  )
postPaymentInvoiceCapture (mbPersonId, _merchantId) invoiceId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  invoice <- QPaymentInvoice.findById invoiceId >>= fromMaybeM (InvalidRequest "Invoice not found")
  Redis.withWaitOnLockRedisWithExpiry (paymentJobExecLockKey invoice.rideId.getId) 10 20 $ do
    when (invoice.paymentStatus /= DPI.PENDING) $
      throwError $ InvalidRequest "Invoice is not pending; already captured or failed"
    ride <- QRide.findById invoice.rideId >>= fromMaybeM (RideNotFound invoice.rideId.getId)
    booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
    unless (booking.riderId == personId) $
      throwError $ InvalidRequest "Person is not the owner of the ride"
    order <-
      invoice.paymentOrderId
        & fromMaybeM (InvalidRequest "Invoice has no payment order")
    paymentOrder <- QPaymentOrder.findById order >>= fromMaybeM (InvalidRequest "Payment order not found")
    let paymentIntentId = paymentOrder.paymentServiceOrderId
    paymentCharged <-
      SPayment.chargePaymentIntent
        booking.merchantId
        booking.merchantOperatingCityId
        booking.paymentMode
        paymentIntentId
    if paymentCharged
      then do
        QPaymentInvoice.updatePaymentStatus DPI.CAPTURED invoice.id
        QRide.markPaymentStatus Domain.Types.Ride.Completed ride.id
        pure ()
      else do
        QPaymentInvoice.updatePaymentStatus DPI.FAILED invoice.id
        QRide.markPaymentStatus Domain.Types.Ride.Failed ride.id
        throwError $ InvalidRequest "Payment capture failed. Please try again."
  pure Success
