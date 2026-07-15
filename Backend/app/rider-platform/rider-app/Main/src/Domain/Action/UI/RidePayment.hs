module Domain.Action.UI.RidePayment where

import qualified API.Types.UI.RidePayment
import AWS.S3 as S3
import Data.Maybe (listToMaybe)
import qualified Data.Text as Text
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Domain.SharedLogic.RideDiscount as RD
import qualified Domain.Types.Booking
import qualified Domain.Types.FareBreakup
import qualified Domain.Types.FareBreakupInfo as DFareBreakupInfo
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.OfferEntity as DOfferEntity
import qualified Domain.Types.PaymentCustomer as DPaymentCustomer
import qualified Domain.Types.Person
import qualified Domain.Types.RefundRequest as DRefundRequest
import qualified Domain.Types.Ride
import qualified Domain.Types.RideStatus
import Domain.Types.VehicleCategory as DV
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
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Finance (FinanceCtx)
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified Lib.Payment.Domain.Types.PaymentOrder as DPaymentOrder
import qualified Lib.Payment.Domain.Types.PaymentTransaction as DPaymentTransaction
import qualified Lib.Payment.Domain.Types.Refunds as DRefunds
import qualified Lib.Payment.Storage.HistoryQueries.PaymentTransaction as HQPaymentTransaction
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified SharedLogic.FareBreakupInfo as SFareBreakupInfo
import qualified SharedLogic.Finance.RidePayment as RidePaymentFinance
import SharedLogic.JobScheduler
import qualified SharedLogic.Payment as SPayment
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.PayoutConfig as CQPayoutCfg
import Storage.ConfigPilot.Config.PayoutConfig (PayoutConfigDimensions (..))
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.OfferEntity as QOfferEntity
import qualified Storage.Queries.PaymentCustomer as QPaymentCustomer
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RefundRequest as QRefundRequest
import qualified Storage.Queries.Ride as QRide
import qualified Tools.ActorInfo as ActorInfo
import Tools.Error
import qualified Tools.Notifications as Notify
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
  customer <- getOrCreatePaymentCustomer person
  now <- getCurrentTime
  if maybe False (> now) customer.clientAuthTokenExpiry
    then
      return $
        TPayment.CreateCustomerResp
          { customerId = person.id.getId, -- in-app UPI requires customer.personId only
            clientAuthToken = customer.clientAuthToken,
            clientAuthTokenExpiry = customer.clientAuthTokenExpiry
          }
    else do
      getCustomerResp <- TPayment.getCustomer person.merchantId person.merchantOperatingCityId person.paymentMode customer.customerId
      QPaymentCustomer.updateCATAndExipry getCustomerResp.clientAuthToken getCustomerResp.clientAuthTokenExpiry getCustomerResp.customerId (Just paymentMode)
      return $
        TPayment.CreateCustomerResp
          { customerId = person.id.getId, -- in-app UPI requires customer.personId only
            clientAuthToken = getCustomerResp.clientAuthToken,
            clientAuthTokenExpiry = getCustomerResp.clientAuthTokenExpiry
          }

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
      Just customer ->
        if customer.customerId == person.id.getId
          then do
            getCustomerResp <- TPayment.getCustomer person.merchantId person.merchantOperatingCityId person.paymentMode customer.customerId
            QPaymentCustomer.updateCATExpiryAndCustomerIdByPersonId getCustomerResp.clientAuthToken getCustomerResp.clientAuthTokenExpiry getCustomerResp.customerId (Just person.id) (Just paymentMode)
            return
              customer
                { DPaymentCustomer.customerId = getCustomerResp.customerId,
                  DPaymentCustomer.clientAuthToken = getCustomerResp.clientAuthToken,
                  DPaymentCustomer.clientAuthTokenExpiry = getCustomerResp.clientAuthTokenExpiry
                }
          else return customer
      Nothing -> do
        -- Create a customer in payment service if not there
        mbEmailDecrypted <- mapM decrypt person.email
        encryptedMobile <- person.mobileNumber & fromMaybeM (InvalidRequest "Person mobile number required to create payment customer")
        phoneDecrypted <- decrypt encryptedMobile
        let req = CreateCustomerReq {email = mbEmailDecrypted, name = person.firstName, phone = phoneDecrypted, lastName = Nothing, objectReferenceId = person.id.getId, mobileCountryCode = person.mobileCountryCode, optionsGetClientAuthToken = Nothing}
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
  defaultPaymentMethodId <-
    if (maybe True (\dpm -> dpm `notElem` savedPaymentMethodIds) paymentCustomer.defaultPaymentMethodId)
      then do
        let firstSavedPaymentMethodId = listToMaybe savedPaymentMethodIds
        whenJust firstSavedPaymentMethodId $ \pmId ->
          SPayment.updateDefaultPersonPaymentMethodId person pmId
        pure firstSavedPaymentMethodId
      else pure paymentCustomer.defaultPaymentMethodId
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
postPaymentAddTip (mbPersonId, merchantId) rideId tipRequest = ActorInfo.withMbPersonIdActorInfo mbPersonId $ do
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
    fareBreakups <- SFareBreakupInfo.getFareBreakupsWithFallback rideId.getId Domain.Types.FareBreakup.RIDE (runInReplica $ QFareBreakup.findAllByEntityIdAndEntityType rideId.getId Domain.Types.FareBreakup.RIDE)
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
          -- Capture tip — settlement happens automatically inside chargePaymentIntent
          offerStatsInput <- SPayment.buildOfferStatsInput person
          tipPaymentCaptured <- SPayment.chargePaymentIntent booking.merchantId booking.merchantOperatingCityId booking.paymentMode DOrder.RideHailing tipPaymentIntentResp.paymentIntentId rideId RidePaymentFinance.settledReasonTipPayment booking.riderId offerStatsInput
          if tipPaymentCaptured
            then do
              QRide.updateTipByRideId (Just tipAmount) rideId
              let tipCtx = RidePaymentFinance.buildRiderFinanceCtx booking.merchantId.getId booking.merchantOperatingCityId.getId tipAmount.currency True person.id.getId rideId.getId Nothing Nothing (listToMaybe $ catMaybes [booking.fromLocation.address.area, booking.fromLocation.address.street, booking.fromLocation.address.city])
              void $ RidePaymentFinance.createTipLedger tipCtx tipRequest.amount.amount
              RidePaymentFinance.regenerateRideTipInvoice rideId.getId tipRequest.amount.amount
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
        -- Update the PI amount to include tip. We still pass the current
        -- ledger info here — the core-entries guard inside makePaymentIntent
        -- compares newTotal vs. oldTotal, and since tip is not part of
        -- ledger core (handled separately via createTipLedger), the
        -- existing core entries won't be disturbed.
        tipRideFareBreakups <- SFareBreakupInfo.getFareBreakupsWithFallback rideId.getId Domain.Types.FareBreakup.RIDE (QFareBreakup.findAllByEntityIdAndEntityType rideId.getId Domain.Types.FareBreakup.RIDE)
        let tipLedgerCtx = RidePaymentFinance.buildRiderFinanceCtx person.merchantId.getId person.merchantOperatingCityId.getId totalFare.currency True person.id.getId ride.id.getId Nothing Nothing (listToMaybe $ catMaybes [booking.fromLocation.address.area, booking.fromLocation.address.street, booking.fromLocation.address.city])
        mbTipLedgerInfo <- SPayment.buildLedgerInfoFromBreakups tipRideFareBreakups rideDiscountAmount ridePayoutAmount applicationFeeAmount 0 tipLedgerCtx
        let tipLedgerInfo =
              fromMaybe
                SPayment.RidePaymentLedgerInfo
                  { rideFare = totalFare.amount - applicationFeeAmount - rideDiscountAmount,
                    gstAmount = 0,
                    tollFare = 0,
                    tollVatAmount = 0,
                    parkingCharge = 0,
                    parkingChargeVat = 0,
                    platformFee = applicationFeeAmount,
                    offerDiscountAmount = rideDiscountAmount,
                    cashbackPayoutAmount = ridePayoutAmount,
                    rideVatAbsorbedOnDiscount = 0,
                    cancellationCharge = 0,
                    cancellationTax = 0,
                    financeCtx = tipLedgerCtx
                  }
                mbTipLedgerInfo
        mbPaymentIntentResp <- SPayment.makePaymentIntent person.merchantId person.merchantOperatingCityId booking.paymentMode person.id (Just rideId) mbExistingOrderId DOrder.RideHailing createPaymentIntentServiceReq (Just tipLedgerInfo)
        case mbPaymentIntentResp of
          Nothing -> do
            bookingFareBreakup <- SFareBreakupInfo.getFareBreakupsWithFallback rideId.getId Domain.Types.FareBreakup.BOOKING (QFareBreakup.findAllByEntityIdAndEntityType rideId.getId Domain.Types.FareBreakup.BOOKING)
            let ledgerCtx = RidePaymentFinance.buildRiderFinanceCtx person.merchantId.getId person.merchantOperatingCityId.getId totalFare.currency True person.id.getId ride.id.getId Nothing Nothing (listToMaybe $ catMaybes [booking.fromLocation.address.area, booking.fromLocation.address.street, booking.fromLocation.address.city])
            mbLedgerInfo <- SPayment.buildLedgerInfoFromBreakups bookingFareBreakup rideDiscountAmount ridePayoutAmount applicationFeeAmount 0 ledgerCtx
            let ledgerInfo =
                  fromMaybe
                    SPayment.RidePaymentLedgerInfo
                      { rideFare = totalFare.amount - applicationFeeAmount - rideDiscountAmount,
                        gstAmount = 0,
                        tollFare = 0,
                        tollVatAmount = 0,
                        parkingCharge = 0,
                        parkingChargeVat = 0,
                        platformFee = applicationFeeAmount,
                        offerDiscountAmount = rideDiscountAmount,
                        cashbackPayoutAmount = ridePayoutAmount,
                        rideVatAbsorbedOnDiscount = 0,
                        cancellationCharge = 0,
                        cancellationTax = 0,
                        financeCtx = ledgerCtx
                      }
                    mbLedgerInfo
            logDebug $ "makePaymentIntent (tip-applied): breakups=" <> show bookingFareBreakup <> " discount=" <> show rideDiscountAmount <> " payout=" <> show ridePayoutAmount <> " platformFee=" <> show applicationFeeAmount <> " -> ledgerInfo=" <> show ledgerInfo
            -- Create separate PENDING tip ledger entry via dedicated function
            let tipCtx = RidePaymentFinance.buildRiderFinanceCtx booking.merchantId.getId booking.merchantOperatingCityId.getId tipAmount.currency True person.id.getId rideId.getId Nothing Nothing (listToMaybe $ catMaybes [booking.fromLocation.address.area, booking.fromLocation.address.street, booking.fromLocation.address.city])
            void $ RidePaymentFinance.createTipLedger tipCtx tipRequest.amount.amount
            when (ride.status == Domain.Types.RideStatus.COMPLETED) $ do
              let discountApplicableFareAmountTaxIncl = case RD.parseProjectFareParamsBreakup $ (\fb -> (fb.description, fb.amount.amount)) <$> bookingFareBreakup of
                    Just b -> b.discountApplicableRideFareTaxExclusive + b.discountApplicableRideFareTax
                    Nothing -> fareWithTip.amount
              SPayment.zeroEffectivePaymentDueToOffer booking.merchantId booking.merchantOperatingCityId rideId person booking.selectedOfferId fareWithTip.currency discountApplicableFareAmountTaxIncl rideDiscountAmount ledgerInfo booking
              RidePaymentFinance.regenerateRideTipInvoice rideId.getId tipAmount.amount
          Just paymentIntentResp -> do
            -- Create separate PENDING tip ledger entry via dedicated function
            let tipCtx = RidePaymentFinance.buildRiderFinanceCtx booking.merchantId.getId booking.merchantOperatingCityId.getId tipAmount.currency True person.id.getId rideId.getId Nothing Nothing (listToMaybe $ catMaybes [booking.fromLocation.address.area, booking.fromLocation.address.street, booking.fromLocation.address.city])
            void $ RidePaymentFinance.createTipLedger tipCtx tipRequest.amount.amount
            -- Capture immediately — old auth was cancelled, new PI needs fresh capture
            when (ride.status == Domain.Types.RideStatus.COMPLETED) $ do
              offerStatsInput <- SPayment.buildOfferStatsInput person
              paymentCaptured <- SPayment.chargePaymentIntent booking.merchantId booking.merchantOperatingCityId booking.paymentMode DOrder.RideHailing paymentIntentResp.paymentIntentId rideId RidePaymentFinance.settledReasonRidePayment booking.riderId offerStatsInput
              if paymentCaptured
                then do
                  QRide.markPaymentStatus Domain.Types.Ride.Completed rideId
                  RidePaymentFinance.regenerateRideTipInvoice rideId.getId tipAmount.amount
                else do
                  QRide.markPaymentStatus Domain.Types.Ride.Failed rideId
                  logError $ "Failed to capture payment intent after tip: " <> paymentIntentResp.paymentIntentId
        QRide.updateTipByRideId (Just tipAmount) rideId
        let tipFarePrice = mkPriceFromAPIEntity tipRequest.amount
        SFareBreakupInfo.addFareBreakupInfoItems rideId.getId Domain.Types.FareBreakup.RIDE [DFareBreakupInfo.FareBreakupInfoItem {description = tipFareBreakupTitle, amount = tipFarePrice.amount, currency = tipFarePrice.currency}] (Just booking.merchantId) (Just booking.merchantOperatingCityId)
    merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
    void $ CallBPPInternal.populateTipAmount merchant.driverOfferApiKey merchant.driverOfferBaseUrl ride.bppRideId.getId tipRequest.amount.amount
  return Success
  where
    tipFareBreakupTitle :: Text
    tipFareBreakupTitle = "RIDE_TIP"

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
  let h =
        RefundRequestCreateHandler
          { validateRefundRequester = \booking -> do
              personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
              unless (booking.riderId == personId) $ throwError $ InvalidRequest "Person is not the owner of the ride",
            mkRefundRequestRow = \ctx -> do
              -- Component-wise only — refundComponents required, no whole-amount fallback.
              when (null req.refundComponents) $ throwError (InvalidRequest "refundComponents required")
              validateRefundComponents rideId ctx.booking req.refundComponents
              personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
              evidenceS3Path <- forM req.evidence $ \evidence -> do
                imageExtension <- validateContentType req
                path <- createPath ctx.booking.merchantId personId rideId ctx.refundPurpose imageExtension
                fork "S3 Put Image" do
                  Redis.withLockRedis (imageS3Lock path) 5 $
                    S3.put (Text.unpack path) evidence
                pure path
              let requestedAmount = Just (sum (map (\c -> c.amount.amount) req.refundComponents))
                  refundsAmount = Nothing -- admin sets at /respond/approve
                  refundsTries = 0 -- no Stripe attempts yet
                  deductFromDriver = Nothing -- admin chooses absorb-vs-clawback at /respond/approve
                  requestedRefundComponents = Just (map (\c -> DRefundRequest.RefundComponentAmount {amount = c.amount.amount, component = c.component}) req.refundComponents)
              -- admin fixes the approved breakdown at /respond/approve; create leaves it unset.
              buildRefundRequestRow ctx evidenceS3Path req.code req.description requestedAmount refundsAmount refundsTries DRefundRequest.OPEN deductFromDriver requestedRefundComponents Nothing,
            postCreate = \_ -> pure Success
          }
  createPaymentRefundRequest h rideId

validateContentType ::
  API.Types.UI.RidePayment.RefundRequestReq ->
  Environment.Flow Text
validateContentType req = do
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

buildRefundRequestRow ::
  ValidatedRefundContext ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  DRefundRequest.RefundRequestCode ->
  Kernel.Prelude.Text ->
  Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney ->
  Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney ->
  Kernel.Prelude.Int ->
  DRefundRequest.RefundRequestStatus ->
  Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
  Kernel.Prelude.Maybe [DRefundRequest.RefundComponentAmount] ->
  Kernel.Prelude.Maybe [DRefundRequest.RefundComponentAmount] ->
  Environment.Flow DRefundRequest.RefundRequest
buildRefundRequestRow ctx evidenceS3Path code description requestedAmount refundsAmount refundsTries status deductFromDriver requestedRefundComponents approvedRefundedComponents = do
  newId <- generateGUID
  now <- getCurrentTime
  pure
    DRefundRequest.RefundRequest
      { id = newId,
        orderId = ctx.orderId,
        transactionId = ctx.transaction.id,
        transactionAmount = ctx.transaction.amount,
        currency = ctx.transaction.currency,
        refundPurpose = ctx.refundPurpose,
        personId = ctx.booking.riderId,
        merchantId = ctx.booking.merchantId,
        merchantOperatingCityId = ctx.booking.merchantOperatingCityId,
        requestedAmount,
        refundsAmount,
        refundsTries,
        status,
        code,
        description,
        deductFromDriver,
        requestedRefundComponents,
        approvedRefundedComponents,
        evidenceS3Path,
        responseDescription = Nothing,
        refundsId = Nothing,
        createdAt = now,
        updatedAt = now
      }

-- | Build per-component refund splits from the row's approved components, each split into fare + VAT
--   by the ride's breakup ratio. Returns the ride-fare total (the BPP commission-slice denominator).
--   Throws if the ride has no structured fare breakup, or if approved components are absent (approve always sets them).
computeRefundSplits ::
  Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
  DRefundRequest.RefundRequest ->
  FinanceCtx ->
  Environment.Flow (Kernel.Types.Common.HighPrecMoney, [RidePaymentFinance.RefundComponentSplit])
computeRefundSplits rideId refundRequest ctx = do
  mbOfferEntity <- QOfferEntity.findByEntityIdAndEntityType rideId.getId DOfferEntity.RIDE
  let discount = maybe 0 (.discountAmount) mbOfferEntity
  fareBreakups <-
    SFareBreakupInfo.getFareBreakupsWithFallback
      rideId.getId
      Domain.Types.FareBreakup.RIDE
      (QFareBreakup.findAllByEntityIdAndEntityType rideId.getId Domain.Types.FareBreakup.RIDE)
  mbLi <- SPayment.buildLedgerInfoFromBreakups fareBreakups discount 0 0 0 ctx
  li <- mbLi & fromMaybeM (InvalidRequest $ "Refund requires a structured fare breakup; ride " <> rideId.getId <> " has none")
  case refundRequest.approvedRefundedComponents of
    Just comps@(_ : _) -> pure (li.rideFare, map (splitComponent li) comps)
    _ -> throwError (InternalError $ "Refund splits require approved components; refundRequest for ride " <> rideId.getId <> " has none")
  where
    splitComponent li comp =
      let (fareTot, vatTot) = case comp.component of
            Domain.Types.FareBreakup.RIDE_FARE -> (li.rideFare, li.gstAmount)
            Domain.Types.FareBreakup.TOLL -> (li.tollFare, li.tollVatAmount)
            Domain.Types.FareBreakup.PARKING -> (li.parkingCharge, li.parkingChargeVat)
          tot = fareTot + vatTot
          fareAmt = if tot > 0 then comp.amount * fareTot / tot else comp.amount
       in RidePaymentFinance.RefundComponentSplit comp.component fareAmt (comp.amount - fareAmt)

-- | Build the per-component refund DTOs the BPP consumes (FareComponent → its Text tag).
mkRefundLedgerComponents :: [RidePaymentFinance.RefundComponentSplit] -> [CallBPPInternal.RefundLedgerComponent]
mkRefundLedgerComponents =
  map (\s -> CallBPPInternal.RefundLedgerComponent {component = s.component, fareAmount = s.fareAmount, vatAmount = s.vatAmount})

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
    Environment.Flow API.Types.UI.RidePayment.RefundRequestListResp
  )
getPaymentRefundRequest (mbPersonId, _merchantId) rideId = do
  orderId <- SPayment.getOrderIdForRide rideId >>= fromMaybeM (InternalError $ "No payment order found for ride " <> rideId.getId)
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  rows <- QRefundRequest.findAllByOrderId orderId
  forM_ rows $ \rr ->
    unless (rr.personId == personId) $
      throwError $ InvalidRequest "Person is not the owner of the ride"
  items <- forM (sortOn (Down . (.createdAt)) rows) $ \rr -> do
    evidence <- fetchEvidenceFromS3 rr
    pure $ mkRefundRequestResp rideId rr evidence
  pure $ API.Types.UI.RidePayment.RefundRequestListResp {refundRequests = items}

-- | Per-component fare+tax breakdown for a ride — the refundable options feeding
--   refundRequest/create. Reuses 'buildLedgerInfoFromBreakups' with appFee=0, so RideFare is the
--   FULL customer-paid fare (commission embedded); 'components = Nothing' for legacy rides
--   whose breakup has no structured tags.
getPaymentFareBreakup ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    Environment.Flow API.Types.UI.RidePayment.FareBreakupRes
  )
getPaymentFareBreakup (mbPersonId, _merchantId) rideId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  ride <- QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  unless (booking.riderId == personId) $ throwError $ InvalidRequest "Person is not the owner of the ride"
  getFareBreakupForRide rideId booking

-- | Auth-free core of 'getPaymentFareBreakup'. Callers own the access check:
--   the rider endpoint verifies ride ownership, the dashboard endpoint
--   (Domain.Action.Dashboard.AppManagement.Payment) verifies merchant/city.
getFareBreakupForRide ::
  Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
  Domain.Types.Booking.Booking ->
  Environment.Flow API.Types.UI.RidePayment.FareBreakupRes
getFareBreakupForRide rideId booking = do
  mbOfferEntity <- QOfferEntity.findByEntityIdAndEntityType rideId.getId DOfferEntity.RIDE
  let discount = maybe 0 (.discountAmount) mbOfferEntity
      currency = booking.estimatedFare.currency
  fareBreakups <-
    SFareBreakupInfo.getFareBreakupsWithFallback
      rideId.getId
      Domain.Types.FareBreakup.RIDE
      (QFareBreakup.findAllByEntityIdAndEntityType rideId.getId Domain.Types.FareBreakup.RIDE)
  let ctx =
        RidePaymentFinance.buildRiderFinanceCtx
          booking.merchantId.getId
          booking.merchantOperatingCityId.getId
          currency
          True
          booking.riderId.getId
          rideId.getId
          Nothing
          Nothing
          (listToMaybe $ catMaybes [booking.fromLocation.address.area, booking.fromLocation.address.street, booking.fromLocation.address.city])
  mbLedgerInfo <- SPayment.buildLedgerInfoFromBreakups fareBreakups discount 0 0 0 ctx
  let toPrice amt = mkPriceAPIEntity (mkPrice (Just currency) amt)
      mkComp c excl tax =
        let tot = excl + tax
         in if tot > 0
              then Just $ API.Types.UI.RidePayment.FareBreakupComponent {component = c, exclTax = toPrice excl, tax = toPrice tax, total = toPrice tot}
              else Nothing
  case mbLedgerInfo of
    Nothing -> pure $ API.Types.UI.RidePayment.FareBreakupRes {components = Nothing, totalAmount = toPrice 0}
    Just li ->
      pure $
        API.Types.UI.RidePayment.FareBreakupRes
          { components =
              Just $
                catMaybes
                  [ mkComp Domain.Types.FareBreakup.RIDE_FARE li.rideFare li.gstAmount,
                    mkComp Domain.Types.FareBreakup.TOLL li.tollFare li.tollVatAmount,
                    mkComp Domain.Types.FareBreakup.PARKING li.parkingCharge li.parkingChargeVat
                  ],
            totalAmount = toPrice (li.rideFare + li.gstAmount + li.tollFare + li.tollVatAmount + li.parkingCharge + li.parkingChargeVat)
          }

mkRefundRequestResp ::
  Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
  DRefundRequest.RefundRequest ->
  Maybe Text ->
  API.Types.UI.RidePayment.RefundRequestResp
mkRefundRequestResp rideId DRefundRequest.RefundRequest {..} evidence =
  API.Types.UI.RidePayment.RefundRequestResp
    { rideId = rideId,
      refundsAmount = flip PriceAPIEntity currency <$> refundsAmount,
      requestedAmount = flip PriceAPIEntity currency <$> requestedAmount,
      transactionAmount = PriceAPIEntity transactionAmount currency,
      ..
    }

data RefundRequestInfoHandler resp = RefundRequestInfoHandler
  { validateRefundRequestOwner :: DRefundRequest.RefundRequest -> Environment.Flow (),
    mkRefundRequestInfoResp :: DRefundRequest.RefundRequest -> Maybe Text -> Maybe PaymentInterface.RefundStatus -> Maybe Text -> resp,
    fetchRefunds :: Kernel.Types.Id.Id DRefunds.Refunds -> Environment.Flow (Maybe DRefunds.Refunds) -- required only for error code and status
  }

-- | Caller resolves which refund_request row to refresh (dashboard does this by refundRequestId).
--   Lock per-orderId so concurrent siblings serialize on the cumulative-budget reads.
fetchPaymentRefundRequestInfo ::
  forall resp.
  RefundRequestInfoHandler resp ->
  Maybe Bool ->
  DRefundRequest.RefundRequest ->
  Environment.Flow resp
fetchPaymentRefundRequestInfo h refreshRefunds refundRequest = do
  let orderId = refundRequest.orderId
  eResult <- Redis.whenWithLockRedisAndReturnValue (refundRequestProccessingKey orderId) 60 $ do
    h.validateRefundRequestOwner refundRequest
    evidence <- fetchEvidenceFromS3 refundRequest
    if refreshRefunds == Just True
      then do
        -- refresh is possible only if refund object already created on payment gateway side
        rideId <- SPayment.getRideIdForOrder orderId >>= fromMaybeM (InternalError $ "No ride mapping found for order: " <> orderId.getId)
        ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
        booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
        mbResult <- SPayment.getRefundStatusForOrder refundRequest.merchantId refundRequest.merchantOperatingCityId booking.paymentMode refundRequest.orderId refundRequest.refundsId
        case mbResult of
          Nothing -> pure $ h.mkRefundRequestInfoResp refundRequest evidence Nothing Nothing
          Just result -> do
            let updStatus = castRefundRequestStatus result.status
                refundId = Kernel.Types.Id.Id result.refundId
            processRefundResult refundRequest updStatus (Just refundId)
            let updRefundRequest = refundRequest{refundsId = Just refundId, status = updStatus}
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

data ValidatedRefundContext = ValidatedRefundContext
  { orderId :: Kernel.Types.Id.Id DPaymentOrder.PaymentOrder,
    ride :: Domain.Types.Ride.Ride,
    booking :: Domain.Types.Booking.Booking,
    refundPurpose :: DRefundRequest.RefundPurpose,
    transaction :: DPaymentTransaction.PaymentTransaction,
    existingRequests :: [DRefundRequest.RefundRequest]
  }

data RefundRequestCreateHandler resp = RefundRequestCreateHandler
  { validateRefundRequester :: Domain.Types.Booking.Booking -> Environment.Flow (),
    mkRefundRequestRow :: ValidatedRefundContext -> Environment.Flow DRefundRequest.RefundRequest,
    postCreate :: DRefundRequest.RefundRequest -> Environment.Flow resp
  }

createPaymentRefundRequest ::
  forall resp.
  RefundRequestCreateHandler resp ->
  Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
  Environment.Flow resp
createPaymentRefundRequest h rideId = do
  orderId <- SPayment.getOrderIdForRide rideId >>= fromMaybeM (InternalError $ "No payment order found for ride " <> rideId.getId)
  eResult <- Redis.whenWithLockRedisAndReturnValue (refundRequestProccessingKey orderId) 60 $ do
    existingRequests <- QRefundRequest.findAllByOrderId orderId
    when (any (\r -> r.status `elem` [DRefundRequest.OPEN, DRefundRequest.APPROVED]) existingRequests) $
      throwError (InvalidRequest $ "A refund request is already in process for this order; please retry after it is resolved. Order: " <> orderId.getId)
    ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
    booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
    h.validateRefundRequester booking
    refundPurpose <- case ride.status of
      Domain.Types.RideStatus.COMPLETED -> pure DRefundRequest.RIDE_FARE
      Domain.Types.RideStatus.CANCELLED -> pure DRefundRequest.CANCELLATION_FEE
      _ -> throwError $ RideInvalidStatus ("Refund request available only for COMPLETED or CANCELLED ride. Ride status: " <> show ride.status)
    unless (ride.paymentStatus == Domain.Types.Ride.Completed) $
      throwError (RideInvalidStatus $ "Ride payment is not completed yet. Payment status: " <> show ride.paymentStatus)
    unless ride.onlinePayment $ throwError (InvalidRequest "Could not refund cash ride")
    -- We need earliest transaction in case if we have more than one transaction (ride fare and ride tip)
    -- Later we can add transactions differentiation based on purpose: RIDE_FARE | RIDE_TIP | CANCELLATION_FEE
    transaction <- HQPaymentTransaction.findEarliestChargedTransactionByOrderId orderId >>= fromMaybeM (InvalidRequest "No transaction found for refund")
    let ctx = ValidatedRefundContext {orderId, ride, booking, refundPurpose, transaction, existingRequests}
    refundRequest <- h.mkRefundRequestRow ctx
    QRefundRequest.create refundRequest
    QRide.updateRefundRequestStatus (Just refundRequest.status) rideId
    h.postCreate refundRequest
  case eResult of
    Left () -> do
      logError $ "Order refund locked for ride: " <> rideId.getId
      throwError (InvalidRequest "Order refund already in progress")
    Right result -> return result

-- | Per-component ledger-driven cap check: for each requested component, already-refunded
--   (SETTLED legs) + requested must not exceed the component's total (fare + VAT) from the
--   ride fare-breakup. Used instead of the cumulative-total check for per-component requests.
validateRefundComponents ::
  Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
  Domain.Types.Booking.Booking ->
  [API.Types.UI.RidePayment.RefundComponentReq] ->
  Environment.Flow ()
validateRefundComponents rideId booking comps = do
  let ctx =
        RidePaymentFinance.buildRiderFinanceCtx
          booking.merchantId.getId
          booking.merchantOperatingCityId.getId
          booking.estimatedFare.currency
          True
          booking.riderId.getId
          rideId.getId
          Nothing
          Nothing
          (listToMaybe $ catMaybes [booking.fromLocation.address.area, booking.fromLocation.address.street, booking.fromLocation.address.city])
  mbOfferEntity <- QOfferEntity.findByEntityIdAndEntityType rideId.getId DOfferEntity.RIDE
  let discount = maybe 0 (.discountAmount) mbOfferEntity
  fareBreakups <-
    SFareBreakupInfo.getFareBreakupsWithFallback
      rideId.getId
      Domain.Types.FareBreakup.RIDE
      (QFareBreakup.findAllByEntityIdAndEntityType rideId.getId Domain.Types.FareBreakup.RIDE)
  mbLi <- SPayment.buildLedgerInfoFromBreakups fareBreakups discount 0 0 0 ctx
  li <- mbLi & fromMaybeM (InvalidRequest $ "Refund requires a structured fare breakup; ride " <> rideId.getId <> " has none")
  refundEntries <- RidePaymentFinance.getRefundLegEntries rideId.getId
  forM_ comps $ \c -> do
    let cap = componentTotal li c.component
        requested = c.amount.amount
        already = RidePaymentFinance.settledRefundByComponent refundEntries c.component
    -- Compare at currency precision: the app only ever sees cent-rounded totals (PriceAPIEntity
    -- rounds at serialization) and echoes them back, so a raw comparison spuriously rejects a
    -- full-component refund whenever the true cap rounds up (e.g. cap 25.7953 shown as 25.80).
    when (roundAmountByCurrency' booking.estimatedFare.currency (already + requested) > roundAmountByCurrency' booking.estimatedFare.currency cap) $
      throwError
        ( InvalidRequest $
            "Refund for " <> show c.component <> " (already " <> show already <> " + requested " <> show requested
              <> ") exceeds the refundable cap "
              <> show cap
        )
  where
    componentTotal li component = case component of
      Domain.Types.FareBreakup.RIDE_FARE -> li.rideFare + li.gstAmount
      Domain.Types.FareBreakup.TOLL -> li.tollFare + li.tollVatAmount
      Domain.Types.FareBreakup.PARKING -> li.parkingCharge + li.parkingChargeVat

castRefundRequestStatus :: TPayment.RefundStatus -> DRefundRequest.RefundRequestStatus
castRefundRequestStatus = \case
  TPayment.REFUND_PENDING -> DRefundRequest.APPROVED -- did not changed
  TPayment.REFUND_FAILURE -> DRefundRequest.FAILED
  TPayment.REFUND_SUCCESS -> DRefundRequest.REFUNDED
  TPayment.MANUAL_REVIEW -> DRefundRequest.APPROVED -- did not changed
  TPayment.REFUND_CANCELED -> DRefundRequest.FAILED
  TPayment.REFUND_REQUIRES_ACTION -> DRefundRequest.APPROVED -- did not changed

-- | Side-effects for the OPEN→APPROVED transition. Called from Dashboard /respond
--   and /initiate after the row's status flip.
processRefundRaised :: DRefundRequest.RefundRequest -> Environment.Flow ()
processRefundRaised refundRequest = do
  rideId <- SPayment.getRideIdForOrder refundRequest.orderId >>= fromMaybeM (InternalError $ "No ride mapping found for order: " <> refundRequest.orderId.getId)
  ride <- QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  let amount = fromMaybe refundRequest.transactionAmount refundRequest.refundsAmount
      ctx =
        RidePaymentFinance.buildRiderFinanceCtx
          booking.merchantId.getId
          booking.merchantOperatingCityId.getId
          refundRequest.currency
          True
          refundRequest.personId.getId
          rideId.getId
          Nothing
          Nothing
          (listToMaybe $ catMaybes [booking.fromLocation.address.area, booking.fromLocation.address.street, booking.fromLocation.address.city])
  (rideFareTotal, splits) <- computeRefundSplits rideId refundRequest ctx
  RidePaymentFinance.createRefundRaisedLedger ctx refundRequest.id splits >>= \case
    Left err -> throwError $ InternalError $ "Failed to write refund raise ledger for refund request " <> refundRequest.id.getId <> ": " <> show err
    Right _ -> pure ()
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  CallBPPInternal.refundLedger merchant.driverOfferApiKey merchant.driverOfferBaseUrl ride.bppRideId.getId $
    CallBPPInternal.RefundLedgerReq
      { refundRequestId = refundRequest.id.getId,
        refundsAmount = amount,
        deductFromDriver = refundRequest.deductFromDriver,
        refundRequestStatus = CallBPPInternal.APPROVED,
        refundComponents = Just (mkRefundLedgerComponents splits),
        rideFareComponentTotal = Just rideFareTotal
      }

-- | Side-effects for the APPROVED→REFUNDED transition. Dispatched by processRefundResult
--   when Stripe confirms success.
processRefundSucceeded :: DRefundRequest.RefundRequest -> Environment.Flow ()
processRefundSucceeded refundRequest = do
  rideId <- SPayment.getRideIdForOrder refundRequest.orderId >>= fromMaybeM (InternalError $ "No ride mapping found for order: " <> refundRequest.orderId.getId)
  -- Re-fire guard: the 3 status-refresh hooks can re-enter this REFUNDED transition, and the
  -- cumulative refund invoice would double-count, so gate the one-shot side-effects (invoice,
  -- BPP call) on the ledger dedup signal. The settle+zero-out is NOT gated: it is internally
  -- idempotent, so a re-fire heals a crash between the settle and the zero-out reversal.
  alreadyRecorded <- RidePaymentFinance.refundSucceededAlreadyRecorded refundRequest.id
  ride <- QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  let amount = fromMaybe refundRequest.transactionAmount refundRequest.refundsAmount
      ctx =
        RidePaymentFinance.buildRiderFinanceCtx
          booking.merchantId.getId
          booking.merchantOperatingCityId.getId
          refundRequest.currency
          True
          refundRequest.personId.getId
          rideId.getId
          Nothing
          Nothing
          (listToMaybe $ catMaybes [booking.fromLocation.address.area, booking.fromLocation.address.street, booking.fromLocation.address.city])
  void $ RidePaymentFinance.createRefundSucceededLedger refundRequest.id
  unless alreadyRecorded $ do
    (rideFareTotal, splits) <- computeRefundSplits rideId refundRequest ctx
    RidePaymentFinance.createRefundInvoice rideId.getId refundRequest.id.getId splits
    merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
    CallBPPInternal.refundLedger merchant.driverOfferApiKey merchant.driverOfferBaseUrl ride.bppRideId.getId $
      CallBPPInternal.RefundLedgerReq
        { refundRequestId = refundRequest.id.getId,
          refundsAmount = amount,
          deductFromDriver = refundRequest.deductFromDriver,
          refundRequestStatus = CallBPPInternal.REFUNDED,
          refundComponents = Just (mkRefundLedgerComponents splits), -- BPP builds its per-component refund + negative Commission invoices from these
          rideFareComponentTotal = Just rideFareTotal
        }

-- | APPROVED→FAILED: void the pending refund legs on both BAP and BPP (no invoice — none until
--   success). Idempotent; processRefundResult only calls this on a genuine status transition.
processRefundFailed :: DRefundRequest.RefundRequest -> Environment.Flow ()
processRefundFailed refundRequest = do
  rideId <- SPayment.getRideIdForOrder refundRequest.orderId >>= fromMaybeM (InternalError $ "No ride mapping found for order: " <> refundRequest.orderId.getId)
  ride <- QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  let amount = fromMaybe refundRequest.transactionAmount refundRequest.refundsAmount
  RidePaymentFinance.voidRefundRaisedLedger refundRequest.id
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  CallBPPInternal.refundLedger merchant.driverOfferApiKey merchant.driverOfferBaseUrl ride.bppRideId.getId $
    CallBPPInternal.RefundLedgerReq
      { refundRequestId = refundRequest.id.getId,
        refundsAmount = amount,
        deductFromDriver = refundRequest.deductFromDriver,
        refundRequestStatus = CallBPPInternal.FAILED,
        refundComponents = Nothing,
        rideFareComponentTotal = Nothing
      }

-- | Apply an async Stripe result to an APPROVED refund_request: update status + notify, then
--   dispatch side-effects — REFUNDED settles + invoices, FAILED voids (BAP + BPP). Idempotent
--   (re-fire with same status is a no-op). Shared by all 3 hooks (approve/retry, webhook, /info).
processRefundResult ::
  DRefundRequest.RefundRequest ->
  DRefundRequest.RefundRequestStatus ->
  Maybe (Kernel.Types.Id.Id DRefunds.Refunds) ->
  Environment.Flow ()
processRefundResult refundRequest updStatus mbRefundsId =
  when (refundRequest.status /= updStatus || (isJust mbRefundsId && refundRequest.refundsId /= mbRefundsId)) $ do
    case mbRefundsId of
      Just rid -> QRefundRequest.updateRefundIdAndStatus (Just rid) updStatus refundRequest.id
      Nothing -> QRefundRequest.updateRefundStatus updStatus refundRequest.id
    rideId <- SPayment.getRideIdForOrder refundRequest.orderId >>= fromMaybeM (InternalError $ "No ride mapping found for order: " <> refundRequest.orderId.getId)
    QRide.updateRefundRequestStatus (Just updStatus) rideId
    let updRefundRequest = refundRequest{status = updStatus, refundsId = mbRefundsId <|> refundRequest.refundsId}
    Notify.notifyRefunds updRefundRequest
    when (updStatus == DRefundRequest.REFUNDED) $ processRefundSucceeded updRefundRequest
    when (updStatus == DRefundRequest.FAILED) $ processRefundFailed updRefundRequest

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
  SPayment.getDuesForPerson person

-- | SIMPLIFIED: Clear pending dues by creating NEW payment order and DEBT_SETTLEMENT invoice
-- Creates a new order for the total due amount and settles parent invoices on success
postPaymentClearDues ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.RidePayment.ClearDuesReq ->
    Environment.Flow API.Types.UI.RidePayment.ClearDuesResp
  )
postPaymentClearDues (mbPersonId, _merchantId) req = ActorInfo.withMbPersonIdActorInfo mbPersonId $ do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  duesResp <- SPayment.getDuesForPerson person
  when (duesResp.totalDueAmount <= 0 || null duesResp.rides) $
    throwError $ InvalidRequest "No pending dues to clear"
  currency <- duesResp.currency & fromMaybeM (InvalidRequest "Currency required when dues present")
  paymentMethodId <- case req.paymentMethodId of
    Just pmId ->
      checkIfPaymentMethodExists person pmId >>= \case
        False -> throwError $ InvalidRequest "Payment method doesn't belong to Customer"
        True -> return pmId
    Nothing -> SPayment.getDefaultPaymentMethodForDues person
  SPayment.clearDuesForPerson person duesResp currency paymentMethodId

-- | Capture PENDING payment for a ride. Settles ledger entries and updates ride status.
--   Returns error if no pending entries found or caller is not the ride owner.
postPaymentRideCapture ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    Environment.Flow APISuccess
  )
postPaymentRideCapture (mbPersonId, _merchantId) rideId = ActorInfo.withMbPersonIdActorInfo mbPersonId $ do
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
postPaymentVerifyVpa (mbPersonId, _merchantId) vpa = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  -- let verifyVPAReq =
  --       VerifyVPAReq
  --         { orderId = Nothing,
  --           customerId = Nothing,
  --           vpa = vpa
  --         }
  --     verifyVpaCall = TPayment.verifyVpa merchantId person.merchantOperatingCityId Nothing TPayment.Normal (Just person.id.getId) person.clientSdkVersion
  -- resp <- withTryCatch "verifyVPAService:vpaVerificationForOffers" $ DPayment.verifyVPAService verifyVPAReq verifyVpaCall
  -- case resp of
  --   Left e -> throwError $ InvalidRequest $ "VPA Verification Failed: " <> show e
  --   Right response -> do
  --     if response.status == "VALID"
  --       then do
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  QPerson.updatePayoutVpa (Just vpa) personId
  when (isNothing person.payoutVpa) $ do
    triggerPendingCashRideCashbackPayoutJob person{payoutVpa = Just vpa}
  pure Success

-- else throwError $ InvalidRequest $ "VPA Verification Failed with status: " <> response.status

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
            when (isNothing person.payoutVpa) $
              triggerPendingCashRideCashbackPayoutJob updatedPerson
            pure response.vpa
          else throwError $ InvalidRequest $ "VPA Verification Failed with status: " <> response.status
  pure $ API.Types.UI.RidePayment.VpaFromNumberResp {vpa = vpa}

triggerPendingCashRideCashbackPayoutJob ::
  Domain.Types.Person.Person ->
  Environment.Flow ()
triggerPendingCashRideCashbackPayoutJob person = do
  let lockKey = "CashRideCashbackPayout:CreateJob:" <> person.id.getId
  Redis.whenWithLockRedisAndReturnValue lockKey 120 schedulePayoutJob >>= \case
    Left _ ->
      logInfo $
        "Skipped cashback payout job trigger; lock already held for person: "
          <> person.id.getId
    Right _ -> pure ()
  where
    schedulePayoutJob = do
      mbPayoutConfig <- getOneConfig (PayoutConfigDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId, vehicleCategory = Just DV.CAR, isPayoutEnabled = Nothing, payoutEntity = Nothing}) (Just (maybeToList <$> CQPayoutCfg.findByCityIdAndVehicleCategory person.merchantOperatingCityId DV.CAR (Just [])))
      case mbPayoutConfig of
        Nothing ->
          logError $ "No payout config found for cashback payout trigger; person=" <> person.id.getId
        Just payoutConfig -> do
          let cashbackPayoutJobData = ExecuteCashRideCashbackPayoutJobData {personId = person.id}
              scheduleAfter = secondsToNominalDiffTime (fromIntegral payoutConfig.scheduleCashbackPayoutAfter)
          createJobIn @_ @'ExecuteCashRideCashbackPayout
            (Just person.merchantId)
            (Just person.merchantOperatingCityId)
            scheduleAfter
            cashbackPayoutJobData
          logInfo $ "Scheduled cashback payout catch-up job after VPA update for person: " <> person.id.getId
