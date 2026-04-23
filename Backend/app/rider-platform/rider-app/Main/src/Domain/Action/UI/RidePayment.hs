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
import qualified Domain.Types.OfferEntity as DOfferEntity
import qualified Domain.Types.PaymentCustomer as DPaymentCustomer
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
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.LedgerEntry as LE
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Domain.Types.PaymentOrder as DPaymentOrder
import qualified Lib.Payment.Domain.Types.PaymentTransaction as DPaymentTransaction
import qualified Lib.Payment.Domain.Types.Refunds as DRefunds
import qualified Lib.Payment.Storage.HistoryQueries.PaymentTransaction as HQPaymentTransaction
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified SharedLogic.Finance.RidePayment as RidePaymentFinance
import qualified SharedLogic.Payment as SPayment
import qualified Storage.CachedQueries.Merchant as CQM
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.OfferEntity as QOfferEntity
import qualified Storage.Queries.PaymentCustomer as QPaymentCustomer
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

-- | Filter ledger entries to find dues: DUE entries excluding specified reference types
filterPendingDuesEntries :: [LE.LedgerEntry] -> [Text] -> [LE.LedgerEntry]
filterPendingDuesEntries entries excludedRefTypes =
  entries
    & filter (\e -> e.status == LE.DUE)
    & filter (\e -> e.referenceType `notElem` excludedRefTypes)

getcustomer ::
  Domain.Types.Person.Person -> Environment.Flow TPayment.CreateCustomerResp
getcustomer person = do
  let paymentMode = fromMaybe DMPM.LIVE person.paymentMode
  customer <- getOrCreatePaymentCustomer person
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
      getCustomerResp <- TPayment.getCustomer person.merchantId person.merchantOperatingCityId person.paymentMode customer.customerId
      QPaymentCustomer.updateCATAndExipry getCustomerResp.clientAuthToken getCustomerResp.clientAuthTokenExpiry getCustomerResp.customerId (Just paymentMode)
      return getCustomerResp

buildCreateCustomer ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r
  ) =>
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  TPayment.CreateCustomerResp ->
  DMPM.PaymentMode ->
  m DPaymentCustomer.PaymentCustomer
buildCreateCustomer personId createCustomerResp paymentMode = do
  now <- getCurrentTime
  return
    DPaymentCustomer.PaymentCustomer
      { personId = Just personId,
        clientAuthToken = createCustomerResp.clientAuthToken,
        clientAuthTokenExpiry = createCustomerResp.clientAuthTokenExpiry,
        customerId = createCustomerResp.customerId,
        defaultPaymentMethodId = Nothing,
        paymentMode = Just paymentMode,
        createdAt = now,
        updatedAt = now
      }

getOrCreatePaymentCustomer :: Domain.Types.Person.Person -> Environment.Flow DPaymentCustomer.PaymentCustomer
getOrCreatePaymentCustomer person = do
  logInfo $ "getOrCreatePaymentCustomer: " <> person.id.getId <> " " <> show person.paymentMode
  let paymentMode = fromMaybe DMPM.LIVE person.paymentMode
      lockKey = "PaymentCustomer:Create:" <> person.id.getId <> ":" <> show paymentMode
  Redis.withLockRedisAndReturnValue lockKey 60 $ do
    -- Re-check inside the lock: another concurrent request may have already created it
    mbCustomer <- QPaymentCustomer.findByPersonIdAndPaymentMode (Just person.id) (Just paymentMode)
    case mbCustomer of
      Just customer -> return customer
      Nothing -> do
        -- Create a customer in payment service if not there
        mbEmailDecrypted <- mapM decrypt person.email
        phoneDecrypted <- mapM decrypt person.mobileNumber
        let req = CreateCustomerReq {email = mbEmailDecrypted, name = person.firstName, phone = phoneDecrypted, lastName = Nothing, objectReferenceId = Nothing, mobileCountryCode = Nothing, optionsGetClientAuthToken = Nothing}
        customerResp <- TPayment.createCustomer person.merchantId person.merchantOperatingCityId person.paymentMode req
        paymentCustomer <- buildCreateCustomer person.id customerResp paymentMode
        QPaymentCustomer.create paymentCustomer
        return paymentCustomer

checkIfPaymentMethodExists :: Domain.Types.Person.Person -> PaymentMethodId -> Environment.Flow Bool
checkIfPaymentMethodExists person paymentMethodId = do
  paymentCustomer <- getOrCreatePaymentCustomer person
  cardList <- TPayment.getCardList person.merchantId person.merchantOperatingCityId person.paymentMode paymentCustomer.customerId
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
  paymentCustomer <- getOrCreatePaymentCustomer person
  resp <- TPayment.getCardList person.merchantId person.merchantOperatingCityId person.paymentMode paymentCustomer.customerId
  let savedPaymentMethodIds = resp <&> (.cardId)
  let defaultPaymentMethodId = paymentCustomer.defaultPaymentMethodId
  when (maybe False (\dpm -> dpm `notElem` savedPaymentMethodIds) defaultPaymentMethodId) $ do
    let firstSavedPaymentMethodId = listToMaybe savedPaymentMethodIds
    whenJust firstSavedPaymentMethodId $ \pmId ->
      SPayment.updateDefaultPersonPaymentMethodId person pmId
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
    True -> SPayment.updateDefaultPersonPaymentMethodId person paymentMethodId >> pure Success

getPaymentIntentSetup ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.Flow API.Types.UI.RidePayment.SetupIntentResponse
  )
getPaymentIntentSetup (mbPersonId, _) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)

  paymentCustomer <- getOrCreatePaymentCustomer person
  ephemeralKey <- TPayment.createEphemeralKeys person.merchantId person.merchantOperatingCityId person.paymentMode paymentCustomer.customerId
  setupIntent <- TPayment.createSetupIntent person.merchantId person.merchantOperatingCityId person.paymentMode paymentCustomer.customerId
  return $
    API.Types.UI.RidePayment.SetupIntentResponse
      { setupIntentClientSecret = setupIntent.clientSecret,
        customerId = paymentCustomer.customerId,
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
  orderId <- SPayment.getOrderIdForRide rideId >>= fromMaybeM (InternalError $ "No payment order found for the ride " <> rideId.getId)
  order <- runInReplica $ QPaymentOrder.findById orderId >>= fromMaybeM (InternalError $ "No payment order found for the ride " <> rideId.getId)
  TPayment.updatePaymentMethodInIntent person.merchantId person.merchantOperatingCityId person.paymentMode order.paymentServiceOrderId newPaymentMethodId
  SPayment.updateDefaultPersonPaymentMethodId person newPaymentMethodId
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
  Redis.withWaitOnLockRedisWithExpiry (SPayment.paymentJobExecLockKey rideId.getId) 10 20 $ do
    personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
    person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
    unless (ride.status == Domain.Types.RideStatus.COMPLETED) $
      throwError $ RideInvalidStatus ("Ride is not completed yet." <> Text.pack (show ride.status))
    booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
    unless (booking.riderId == personId) $ throwError $ InvalidRequest "Person is not the owner of the ride"
    unless ride.onlinePayment $ throwError (InvalidRequest "Could not add tip for Cash ride")
    mbRideOfferEntity <- QOfferEntity.findByEntityIdAndEntityType rideId.getId DOfferEntity.RIDE
    let rideDiscountAmount = maybe 0 (.discountAmount) mbRideOfferEntity
        ridePayoutAmount = maybe 0 (.payoutAmount) mbRideOfferEntity
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
                  discountAmount = 0,
                  offerId = Nothing,
                  applicationFeeAmount = 0, -- No platform commission for tips
                  currency = tipRequest.amount.currency,
                  customer = customerPaymentId,
                  paymentMethod = paymentMethodId,
                  receiptEmail = email,
                  driverAccountId
                }
        let tipAmount = mkPrice (Just tipRequest.amount.currency) tipRequest.amount.amount
        -- Create tip PI with distinct domainEntityId so it doesn't conflict with ride PI in findByDomainEntityId
        let tipDomainRideId = Kernel.Types.Id.Id @Domain.Types.Ride.Ride ("tip:" <> rideId.getId)
        mbTipPaymentIntentResp <- SPayment.makePaymentIntent person.merchantId person.merchantOperatingCityId booking.paymentMode person.id (Just tipDomainRideId) Nothing DOrder.RideHailing createPaymentIntentServiceReq Nothing
        whenJust mbTipPaymentIntentResp $ \tipPaymentIntentResp -> do
          -- Create separate PENDING tip ledger entry
          let tipCtx = RidePaymentFinance.buildRiderFinanceCtx booking.merchantId.getId booking.merchantOperatingCityId.getId tipAmount.currency True person.id.getId rideId.getId Nothing Nothing
          void $ RidePaymentFinance.createTipLedger tipCtx tipRequest.amount.amount
          -- Capture tip — settlement happens automatically inside chargePaymentIntent
          offerStatsInput <- SPayment.buildOfferStatsInput person
          tipPaymentCaptured <- SPayment.chargePaymentIntent booking.merchantId booking.merchantOperatingCityId booking.paymentMode DOrder.RideHailing tipPaymentIntentResp.paymentIntentId rideId RidePaymentFinance.settledReasonTipPayment booking.riderId offerStatsInput
          -- Update tip amount ONLY after successful capture
          if tipPaymentCaptured
            then QRide.updateTipByRideId (Just tipAmount) rideId
            else logError $ "Failed to capture tip payment intent: " <> tipPaymentIntentResp.paymentIntentId
      else do
        -- Tip added before payment is captured (NotInitiated or Initiated)
        -- Update existing payment intent (amount = fare + tip), create separate tip ledger entries
        let tipAmount = mkPrice (Just tipRequest.amount.currency) tipRequest.amount.amount
        totalFare <- ride.totalFare & fromMaybeM (RideFieldNotPresent "totalFare")
        fareWithTip <- totalFare `addPrice` tipAmount
        let applicationFeeAmount = fromMaybe 0 ride.commission
        let createPaymentIntentServiceReq =
              DPayment.CreatePaymentIntentServiceReq
                { amount = fareWithTip.amount,
                  discountAmount = rideDiscountAmount,
                  offerId = Id <$> booking.selectedOfferId,
                  applicationFeeAmount,
                  currency = fareWithTip.currency,
                  customer = customerPaymentId,
                  paymentMethod = paymentMethodId,
                  receiptEmail = email,
                  driverAccountId
                }
        -- Lookup existing order for retry handling
        mbExistingOrderId <- SPayment.getOrderIdForRide rideId
        -- Update the PI amount to include tip — pass Nothing for ledgerInfo so core ride entries aren't touched
        mbPaymentIntentResp <- SPayment.makePaymentIntent person.merchantId person.merchantOperatingCityId booking.paymentMode person.id (Just rideId) mbExistingOrderId DOrder.RideHailing createPaymentIntentServiceReq Nothing
        case mbPaymentIntentResp of
          Nothing -> do
            let ledgerCtx = RidePaymentFinance.buildRiderFinanceCtx person.merchantId.getId person.merchantOperatingCityId.getId totalFare.currency True person.id.getId ride.id.getId Nothing Nothing
                ledgerInfo =
                  Just $
                    SPayment.RidePaymentLedgerInfo
                      { rideFare = totalFare.amount - applicationFeeAmount - rideDiscountAmount,
                        gstAmount = 0, -- TODO: extract GST from fare breakup
                        platformFee = applicationFeeAmount,
                        offerDiscountAmount = rideDiscountAmount,
                        cashbackPayoutAmount = ridePayoutAmount,
                        financeCtx = ledgerCtx
                      }
            -- Create separate PENDING tip ledger entry via dedicated function
            let tipCtx = RidePaymentFinance.buildRiderFinanceCtx booking.merchantId.getId booking.merchantOperatingCityId.getId tipAmount.currency True person.id.getId rideId.getId Nothing Nothing
            void $ RidePaymentFinance.createTipLedger tipCtx tipRequest.amount.amount
            when (ride.status == Domain.Types.RideStatus.COMPLETED) $ do
              SPayment.zeroEffectivePaymentDueToOffer booking.merchantId booking.merchantOperatingCityId rideId person booking.selectedOfferId fareWithTip.currency ledgerInfo
          Just paymentIntentResp -> do
            -- Create separate PENDING tip ledger entry via dedicated function
            let tipCtx = RidePaymentFinance.buildRiderFinanceCtx booking.merchantId.getId booking.merchantOperatingCityId.getId tipAmount.currency True person.id.getId rideId.getId Nothing Nothing
            void $ RidePaymentFinance.createTipLedger tipCtx tipRequest.amount.amount
            -- Capture immediately — old auth was cancelled, new PI needs fresh capture
            when (ride.status == Domain.Types.RideStatus.COMPLETED) $ do
              offerStatsInput <- SPayment.buildOfferStatsInput person
              paymentCaptured <- SPayment.chargePaymentIntent booking.merchantId booking.merchantOperatingCityId booking.paymentMode DOrder.RideHailing paymentIntentResp.paymentIntentId rideId RidePaymentFinance.settledReasonRidePayment booking.riderId offerStatsInput
              if paymentCaptured
                then QRide.markPaymentStatus Domain.Types.Ride.Completed rideId
                else do
                  QRide.markPaymentStatus Domain.Types.Ride.Failed rideId
                  logError $ "Failed to capture payment intent after tip: " <> paymentIntentResp.paymentIntentId
        QRide.updateTipByRideId (Just tipAmount) rideId
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
  orderId <- SPayment.getOrderIdForRide rideId >>= fromMaybeM (InternalError $ "No payment order found for ride " <> rideId.getId)
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
    transaction <- HQPaymentTransaction.findEarliestChargedTransactionByOrderId orderId >>= fromMaybeM (InvalidRequest "No transaction found for refund")

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
  orderId <- SPayment.getOrderIdForRide rideId >>= fromMaybeM (InternalError $ "No payment order found for ride " <> rideId.getId)
  eResult <- Redis.whenWithLockRedisAndReturnValue (refundRequestProccessingKey orderId) 60 $ do
    refundRequest <- QRefundRequest.findByOrderId orderId >>= fromMaybeM (RefundRequestDoesNotExist orderId.getId)
    h.validateRefundRequestOwner refundRequest
    evidence <- fetchEvidenceFromS3 refundRequest
    if refreshRefunds == Just True
      then do
        -- refresh is possible only if refund object already created on payment gateway side
        ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
        booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
        mbResult <- SPayment.getRefundStatusForOrder refundRequest.merchantId refundRequest.merchantOperatingCityId booking.paymentMode refundRequest.orderId
        case mbResult of
          Nothing -> pure $ h.mkRefundRequestInfoResp refundRequest evidence Nothing Nothing
          Just result -> do
            let updStatus = castRefundRequestStatus result.status
                refundId = Kernel.Types.Id.Id result.refundId
            when (refundRequest.refundsId /= Just refundId || refundRequest.status /= updStatus) $
              QRefundRequest.updateRefundIdAndStatus (Just refundId) updStatus refundRequest.id
            let updRefundRequest = refundRequest{refundsId = Just refundId, status = updStatus}
            when (refundRequest.status /= updStatus) $ do
              QRide.updateRefundRequestStatus (Just updRefundRequest.status) rideId
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
  riderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId})
  let excludedPurposes = case riderConfig >>= (.duesExcludedPaymentPurposes) of
        Nothing -> [] -- No config = don't exclude anything
        Just textList -> textList -- Reference type strings used directly for filtering

  -- 1. Find most recent ride for this rider (with booking to avoid extra query)
  mbLatestRideBooking <- runInReplica $ QRide.findMostRecentRideForRider personId

  case mbLatestRideBooking of
    Nothing -> returnNoDues personId
    Just (ride, _) -> do
      -- 2. Get DUE ledger entries for this ride (capture attempted and failed)
      pendingEntries <- RidePaymentFinance.findDueRidePaymentEntries ride.id.getId
      let filteredEntries = filterPendingDuesEntries pendingEntries excludedPurposes

      -- 4. Calculate total
      if null filteredEntries
        then returnNoDues personId
        else do
          let totalAmount = sum $ map (.amount) filteredEntries
          let currency = Kernel.Prelude.head filteredEntries & (.currency)
          logInfo $
            "AUDIT: Get Dues - person_id: " <> personId.getId
              <> ", ride_id: "
              <> ride.id.getId
              <> ", due_invoices: "
              <> show (length filteredEntries)
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
  riderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId})
  let _excludedPurposes = case riderConfig >>= (.duesExcludedPaymentPurposes) of
        Nothing -> [] :: [Text] -- No config = don't exclude anything
        Just textList -> textList -- Reference type strings used directly for filtering
  rideId <- listToMaybe duesResp.rides <&> (.rideId) & fromMaybeM (InvalidRequest "No ride id found")
  Redis.withWaitAndLockRedis (SPayment.paymentJobExecLockKey rideId.getId) 10 20 $ do
    ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)

    -- Find DUE ledger entries (capture was attempted and failed)
    pendingEntries <- RidePaymentFinance.findDueRidePaymentEntries rideId.getId

    -- 3. Validate and get payment details
    paymentMethodId <- case req.paymentMethodId of
      Just pmId -> do
        checkIfPaymentMethodExists person pmId >>= \case
          False -> throwError $ InvalidRequest "Payment method doesn't belong to Customer"
          True -> return pmId
      Nothing -> getDefaultPaymentMethodForDues person

    booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
    mbRideOfferEntity' <- QOfferEntity.findByEntityIdAndEntityType rideId.getId DOfferEntity.RIDE
    let debtDiscountAmount = maybe 0 (.discountAmount) mbRideOfferEntity'
    (customerPaymentId, _) <- SPayment.getCustomerAndPaymentMethod booking person
    driverAccountId <- ride.driverAccountId & fromMaybeM (RideFieldNotPresent "driverAccountId")
    email <- mapM decrypt person.email

    -- 4. Create NEW payment order for debt settlement
    let debtApplicationFeeAmount = fromMaybe 0 booking.commission
    let createPaymentIntentServiceReq =
          DPayment.CreatePaymentIntentServiceReq
            { amount = duesResp.totalDueAmount,
              applicationFeeAmount = debtApplicationFeeAmount,
              discountAmount = debtDiscountAmount,
              offerId = Nothing,
              currency = currency,
              customer = customerPaymentId,
              paymentMethod = paymentMethodId,
              receiptEmail = email,
              driverAccountId
            }

    let debtLedgerCtx = RidePaymentFinance.buildRiderFinanceCtx booking.merchantId.getId booking.merchantOperatingCityId.getId currency True person.id.getId rideId.getId Nothing Nothing
        debtLedgerInfo =
          Just $
            SPayment.RidePaymentLedgerInfo
              { rideFare = duesResp.totalDueAmount - debtApplicationFeeAmount - debtDiscountAmount,
                gstAmount = 0,
                platformFee = debtApplicationFeeAmount,
                offerDiscountAmount = debtDiscountAmount,
                cashbackPayoutAmount = 0, -- debt settlement doesn't have cashback
                financeCtx = debtLedgerCtx
              }

    paymentIntentResp <-
      SPayment.makePaymentIntent
        person.merchantId booking.merchantOperatingCityId booking.paymentMode
        person.id (Just rideId) Nothing DOrder.RideHailing createPaymentIntentServiceReq debtLedgerInfo
        >>= fromMaybeM (InternalError "Payment order expired, please try again")

    -- 5. Debt settlement is now simply: capture pending entries
    --    No separate DEBT_SETTLEMENT invoice needed — settling PENDING entries IS the settlement

    logInfo $
      "AUDIT: Debt settlement for ride " <> rideId.getId
        <> ", amount: "
        <> show duesResp.totalDueAmount
        <> ", pending_entries: "
        <> show (length pendingEntries)

    -- 6. Attempt capture
    captureResult <-
      withTryCatch "postPaymentClearDues:chargePaymentIntent" $ do
        offerStatsInput <- SPayment.buildOfferStatsInput person
        SPayment.chargePaymentIntent
          person.merchantId booking.merchantOperatingCityId booking.paymentMode
          DOrder.RideHailing
          paymentIntentResp.paymentIntentId rideId RidePaymentFinance.settledReasonDebtSettlement booking.riderId offerStatsInput

    case captureResult of
      Right True -> do
        -- chargePaymentIntent now settles PENDING entries from DB directly
        -- Mark ride payment as completed
        QRide.markPaymentStatus Domain.Types.Ride.Completed rideId
        logInfo $
          "AUDIT: Debt settlement CAPTURED - order_id: " <> paymentIntentResp.orderId.getId
            <> ", amount: "
            <> show duesResp.totalDueAmount
            <> ", settled_entries: "
            <> show (length pendingEntries)
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
        logError $
          "AUDIT: Debt settlement FAILED - order_id: " <> paymentIntentResp.orderId.getId
            <> ", ride_id: "
            <> rideId.getId
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
        let userFriendlyMessage = mapPaymentErrorToUserMessage (show err)
        logError $
          "AUDIT: Debt settlement EXCEPTION - order_id: " <> paymentIntentResp.orderId.getId
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
      paymentCustomer <- QPaymentCustomer.findByPersonIdAndPaymentMode (Just p.id) (Just paymentMode) >>= fromMaybeM (InvalidRequest "No payment customer found.")
      paymentCustomer.defaultPaymentMethodId & fromMaybeM (InvalidRequest "No default payment method found. Please provide a payment method.")

    mapPaymentErrorToUserMessage :: Text -> Text
    mapPaymentErrorToUserMessage errMsg
      | "insufficient" `Text.isInfixOf` Text.toLower errMsg = "Payment failed due to insufficient funds. Please check your payment method."
      | "declined" `Text.isInfixOf` Text.toLower errMsg = "Payment was declined by your bank. Please try a different payment method."
      | "expired" `Text.isInfixOf` Text.toLower errMsg = "Payment method has expired. Please update your payment method."
      | "invalid" `Text.isInfixOf` Text.toLower errMsg = "Invalid payment method. Please check your payment details."
      | "timeout" `Text.isInfixOf` Text.toLower errMsg = "Payment request timed out. Please try again."
      | "network" `Text.isInfixOf` Text.toLower errMsg = "Network error occurred. Please check your connection and try again."
      | otherwise = "Payment failed. Please try again or contact support if the issue persists."

-- | Capture PENDING payment for a ride. Settles ledger entries and updates ride status.
--   Returns error if no pending entries found or caller is not the ride owner.
postPaymentRideCapture ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    Environment.Flow APISuccess
  )
postPaymentRideCapture (mbPersonId, _merchantId) rideId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  Redis.withWaitOnLockRedisWithExpiry (SPayment.paymentJobExecLockKey rideId.getId) 10 20 $ do
    pendingEntries <- RidePaymentFinance.findUnsettledRidePaymentEntries rideId.getId
    when (null pendingEntries) $
      throwError $ InvalidRequest "No pending payment found for this ride"
    ride <- QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
    booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
    unless (booking.riderId == personId) $
      throwError $ InvalidRequest "Person is not the owner of the ride"
    orderId <- SPayment.getOrderIdForRide rideId >>= fromMaybeM (InvalidRequest "No payment order found for this ride")
    paymentOrder <- QPaymentOrder.findById orderId >>= fromMaybeM (InvalidRequest "Payment order not found")
    let paymentIntentId = paymentOrder.paymentServiceOrderId
    offerStatsInput <- SPayment.buildOfferStatsInput person
    paymentCharged <-
      SPayment.chargePaymentIntent
        booking.merchantId
        booking.merchantOperatingCityId
        booking.paymentMode
        DOrder.RideHailing
        paymentIntentId
        ride.id
        RidePaymentFinance.settledReasonRidePayment
        booking.riderId
        offerStatsInput
    if paymentCharged
      then do
        -- Settlement already done inside chargePaymentIntent
        QRide.markPaymentStatus Domain.Types.Ride.Completed ride.id
      else do
        QRide.markPaymentStatus Domain.Types.Ride.Failed ride.id
        throwError $ InvalidRequest "Payment capture failed. Please try again."
  pure Success

postPaymentVerifyVpa ::
  ( ( Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postPaymentVerifyVpa (mbPersonId, merchantId) vpa = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let verifyVPAReq =
        VerifyVPAReq
          { orderId = Nothing,
            customerId = Nothing,
            vpa = vpa
          }
      verifyVpaCall = TPayment.verifyVpa merchantId person.merchantOperatingCityId Nothing TPayment.Normal (Just person.id.getId) person.clientSdkVersion
  resp <- withTryCatch "verifyVPAService:vpaVerificationForOffers" $ DPayment.verifyVPAService verifyVPAReq verifyVpaCall
  case resp of
    Left e -> throwError $ InvalidRequest $ "VPA Verification Failed: " <> show e
    Right response -> do
      if response.status == "VALID"
        then do
          let updatedPerson = person{payoutVpa = Just vpa}
          QPerson.updateByPrimaryKey updatedPerson
          pure Success
        else throwError $ InvalidRequest $ "VPA Verification Failed with status: " <> response.status

getPaymentVpaFromNumber ::
  ( ( Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.Flow API.Types.UI.RidePayment.VpaFromNumberResp
  )
getPaymentVpaFromNumber (mbPersonId, merchantId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  mobileNumber <- person.mobileNumber & fromMaybeM (InvalidRequest "Mobile number not found")
  decryptedMobileNumber <- decrypt mobileNumber
  let vpaReqString = decryptedMobileNumber <> "@mapper.npci"
  let verifyVPAReq =
        VerifyVPAReq
          { orderId = Nothing,
            customerId = Nothing,
            vpa = vpaReqString
          }
      verifyVpaCall = TPayment.verifyVpa merchantId person.merchantOperatingCityId Nothing TPayment.Normal (Just person.id.getId) person.clientSdkVersion
  resp <- withTryCatch "verifyVPAService:vpaRetrievalForOffers" $ DPayment.verifyVPAService verifyVPAReq verifyVpaCall
  vpa <-
    case resp of
      Left e -> throwError $ InvalidRequest $ "VPA retrieval failed: " <> show e
      Right response -> do
        if response.status == "VALID"
          then do
            let updatedPerson = person{payoutVpa = Just response.vpa}
            QPerson.updateByPrimaryKey updatedPerson
            pure response.vpa
          else throwError $ InvalidRequest $ "VPA Verification Failed with status: " <> response.status
  pure $ API.Types.UI.RidePayment.VpaFromNumberResp {vpa = vpa}
