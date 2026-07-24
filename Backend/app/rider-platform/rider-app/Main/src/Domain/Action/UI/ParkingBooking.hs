{-# LANGUAGE TypeApplications #-}

module Domain.Action.UI.ParkingBooking (postMultimodalParkingBook, postMultimodalParkingMarshalCreate, parkingBookingOrderStatusHandler) where

import qualified API.Types.UI.ParkingBooking
import qualified Data.Text
import qualified Domain.Types.Merchant
import qualified Domain.Types.ParkingTransaction as DPT
import qualified Domain.Types.Person
import qualified Domain.Types.Person as SP
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption
import qualified Kernel.External.Payment.Interface as Payment
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified SharedLogic.Utils as SLUtils
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.ParkingTransaction as QPT
import qualified Storage.Queries.Person as QPerson
import qualified Tools.ActorInfo as ActorInfo
import qualified Tools.Payment as TPayment
import qualified Tools.Wallet as TWallet

postMultimodalParkingBook ::
  ( Kernel.Prelude.Maybe Data.Text.Text ->
    API.Types.UI.ParkingBooking.ParkingBookingReq ->
    Environment.Flow API.Types.UI.ParkingBooking.ParkingBookingResponse
  )
postMultimodalParkingBook mbApiKey req = ActorInfo.withRequestIdActorInfo $ do
  -- Verify API key
  apiKey <- mbApiKey & fromMaybeM (MissingHeader "api-key")
  expectedApiKey <- asks (.parkingApiKey)
  unless (apiKey == expectedApiKey) $ throwError (InvalidRequest "Invalid API key")

  -- Get default merchant operating city
  person <- QPerson.findById req.customerId >>= fromMaybeM (PersonNotFound "Person not found")
  merchantOpCity <- CQM.getDefaultMerchantOperatingCity person.merchantId
  let merchantOpCityId = merchantOpCity.id

  parkingTransactionId <- generateGUID
  paymentOrderId <- generateGUID
  orderShortId <- generateShortId
  now <- getCurrentTime

  let parkingTransaction =
        DPT.ParkingTransaction
          { id = Kernel.Types.Id.Id parkingTransactionId,
            amount = req.amount,
            startTime = req.parkingStartTime,
            endTime = req.parkingEndTime,
            parkingLotId = req.entityId,
            paymentOrderId = Kernel.Types.Id.Id paymentOrderId,
            status = DPT.Pending,
            vehicleNumber = req.vehicleNumber,
            merchantId = Just person.merchantId,
            merchantOperatingCityId = Just merchantOpCityId,
            createdAt = now,
            updatedAt = now
          }

  QPT.create parkingTransaction

  isSplitEnabled <- TPayment.getIsSplitEnabled person.merchantId merchantOpCityId Nothing TPayment.ParkingBooking
  isPercentageSplitEnabled <- TPayment.getIsPercentageSplit person.merchantId merchantOpCityId Nothing TPayment.ParkingBooking
  splitSettlementDetails <- TPayment.mkSplitSettlementDetails isSplitEnabled req.amount [] isPercentageSplitEnabled False

  customerEmail <- fromMaybe "noreply@nammayatri.in" <$> mapM decrypt person.email
  customerPhone <- person.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber") >>= decrypt
  staticCustomerId <- SLUtils.getStaticCustomerId person customerPhone
  nwAddress <- asks (.nwAddress)
  udf1 <- SLUtils.getPersonUdf1 person
  offerBasket <- TPayment.mkOfferBasket person.merchantId merchantOpCityId Nothing TPayment.ParkingBooking req.amount 1
  let createOrderReq =
        Payment.CreateOrderReq
          { orderId = paymentOrderId,
            orderShortId = orderShortId.getShortId,
            amount = req.amount,
            customerId = staticCustomerId,
            customerEmail,
            customerPhone,
            customerFirstName = person.firstName,
            customerLastName = person.lastName,
            createMandate = Nothing,
            mandateMaxAmount = Nothing,
            mandateFrequency = Nothing,
            mandateStartDate = Nothing,
            mandateEndDate = Nothing,
            optionsGetUpiDeepLinks = Nothing,
            metadataExpiryInMins = Nothing,
            metadataGatewayReferenceId = Nothing,
            webhookUrl = Just nwAddress,
            splitSettlementDetails = splitSettlementDetails,
            basket = offerBasket,
            paymentRules = Nothing,
            autoRefundPostSuccess = Nothing,
            paymentFilter = Nothing,
            udf1 = udf1
          }

  let commonMerchantId = Kernel.Types.Id.cast @Domain.Types.Merchant.Merchant @DPayment.Merchant person.merchantId
      commonPersonId = Kernel.Types.Id.cast @Domain.Types.Person.Person @DPayment.Person req.customerId
      createOrderCall = TPayment.createOrder person.merchantId merchantOpCityId Nothing TPayment.ParkingBooking (Just req.customerId.getId) person.clientSdkVersion Nothing
  isMetroTestTransaction <- asks (.isMetroTestTransaction)
  let createWalletCall = TWallet.createWallet person.merchantId merchantOpCityId
  orderResp <- DPayment.createOrderService commonMerchantId (Just $ Kernel.Types.Id.cast merchantOpCityId) commonPersonId Nothing Nothing TPayment.ParkingBooking isMetroTestTransaction createOrderReq createOrderCall (Just createWalletCall) False Nothing >>= fromMaybeM (InternalError "Failed to create payment order")

  paymentLink <- case orderResp.payment_links of
    Just links -> fromMaybeM (InternalError "Payment link not found") links.web
    Nothing -> throwError (InternalError "Payment link not found in response")

  return $
    API.Types.UI.ParkingBooking.ParkingBookingResponse
      { parkingLotId = parkingTransactionId,
        orderShortId = orderResp.order_id,
        paymentLink = Kernel.Prelude.showBaseUrl paymentLink,
        orderId = Kernel.Types.Id.Id paymentOrderId
      }

-- | Create a "marshal" service-account Person so that walk-up marshal bookings can reuse the
-- existing postMultimodalParkingBook API (which requires a real NY customerId). Called once when a
-- marshal account is provisioned; the returned customerId is stored and used for every booking.
postMultimodalParkingMarshalCreate ::
  ( Kernel.Prelude.Maybe Data.Text.Text ->
    API.Types.UI.ParkingBooking.MarshalPersonReq ->
    Environment.Flow API.Types.UI.ParkingBooking.MarshalPersonResp
  )
postMultimodalParkingMarshalCreate mbApiKey req = do
  -- Verify API key (same scheme as postMultimodalParkingBook)
  apiKey <- mbApiKey & fromMaybeM (MissingHeader "api-key")
  expectedApiKey <- asks (.parkingApiKey)
  unless (apiKey == expectedApiKey) $ throwError (InvalidRequest "Invalid API key")

  merchantOpCity <- CQM.getDefaultMerchantOperatingCity_ (Kernel.Types.Id.ShortId req.merchantShortId)
  mobileNumberHash <- getDbHash req.mobileNumber
  let lockKey = "marshalPersonCreate:" <> merchantOpCity.merchantId.getId <> ":" <> Kernel.Prelude.show mobileNumberHash
  -- Serialize concurrent requests for the same (mobile, merchant) so the find-or-create below is atomic
  Redis.withLockRedisAndReturnValue lockKey 10 $ do
    mbExisting <- QPerson.findByMobileNumberAndMerchantId req.mobileCountryCode mobileNumberHash merchantOpCity.merchantId
    case mbExisting of
      Just existing -> return $ API.Types.UI.ParkingBooking.MarshalPersonResp {customerId = existing.id}
      Nothing -> do
        personId <- generateGUID
        now <- getCurrentTime
        encMobNum <- encrypt req.mobileNumber
        encEmail <- mapM encrypt req.email
        let person =
              SP.Person
                { id = Kernel.Types.Id.Id personId,
                  firstName = req.firstName,
                  middleName = Nothing,
                  lastName = req.lastName,
                  role = SP.USER,
                  gender = SP.UNKNOWN,
                  identifierType = SP.MOBILENUMBER,
                  email = encEmail,
                  passwordHash = Nothing,
                  mobileNumber = Just encMobNum,
                  mobileCountryCode = Just req.mobileCountryCode,
                  identifier = Nothing,
                  rating = Nothing,
                  totalRatings = 0,
                  totalRatingScore = 0,
                  isValidRating = False,
                  language = Nothing,
                  isNew = True,
                  enabled = True,
                  deviceToken = Nothing,
                  notificationToken = Nothing,
                  description = Nothing,
                  merchantId = merchantOpCity.merchantId,
                  currentCity = merchantOpCity.city,
                  merchantOperatingCityId = merchantOpCity.id,
                  referralCode = Nothing,
                  referredAt = Nothing,
                  hasTakenValidRide = False,
                  hasDisability = Nothing,
                  createdAt = now,
                  updatedAt = now,
                  blocked = False,
                  blockedAt = Nothing,
                  blockedByRuleId = Nothing,
                  aadhaarVerified = False,
                  clientSdkVersion = Nothing,
                  clientBundleVersion = Nothing,
                  clientConfigVersion = Nothing,
                  clientReactNativeVersion = Nothing,
                  clientDevice = Nothing,
                  backendAppVersion = Nothing,
                  whatsappNotificationEnrollStatus = Nothing,
                  shareEmergencyContacts = False,
                  shareTripWithEmergencyContactOption = Nothing,
                  hasCompletedMockSafetyDrill = Nothing,
                  nightSafetyChecks = True,
                  hasCompletedSafetySetup = False,
                  registrationLat = Nothing,
                  registrationLon = Nothing,
                  latestLat = Nothing,
                  latestLon = Nothing,
                  useFakeOtp = Nothing,
                  followsRide = False,
                  falseSafetyAlarmCount = 0,
                  safetyCenterDisabledOnDate = Nothing,
                  referredByCustomer = Nothing,
                  customerReferralCode = Nothing,
                  blockedCount = Just 0,
                  deviceId = Nothing,
                  androidId = Nothing,
                  registeredViaPartnerOrgId = Nothing,
                  juspayCustomerPaymentID = Nothing,
                  enableOtpLessRide = Nothing,
                  totalRidesCount = Just 0,
                  customerNammaTags = Nothing,
                  informPoliceSos = False,
                  payoutVpa = Nothing,
                  frequentLocGeohashes = Just [],
                  liveActivityToken = Nothing,
                  dateOfBirth = Nothing,
                  profilePicture = Nothing,
                  verificationChannel = Nothing,
                  blockedUntil = Nothing,
                  authBlocked = Nothing,
                  lastUsedVehicleServiceTiers = [],
                  lastUsedVehicleCategories = [],
                  imeiNumber = Nothing,
                  comments = Nothing,
                  businessProfileVerified = Nothing,
                  businessEmail = Nothing,
                  paymentMode = Nothing,
                  cloudType = Nothing,
                  operatorBadgeToken = Nothing,
                  clientId = Nothing
                }
        QPerson.create person
        return $ API.Types.UI.ParkingBooking.MarshalPersonResp {customerId = person.id}

-- Webhook Handler for Pass Payment Status Updates
parkingBookingOrderStatusHandler ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) =>
  Kernel.Types.Id.Id DOrder.PaymentOrder ->
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  Payment.TransactionStatus ->
  m (DPayment.PaymentFulfillmentStatus, Maybe Text, Maybe Text)
parkingBookingOrderStatusHandler paymentOrderId _merchantId status = do
  logInfo $ "Parking booking payment webhook handler called for paymentOrderId: " <> paymentOrderId.getId
  mbParkingTransaction <- QPT.findByPaymentOrderId paymentOrderId
  case mbParkingTransaction of
    Just parkingTransaction -> do
      let mbParkingStatus = convertPaymentStatusToParkingTransactionStatus status
      whenJust mbParkingStatus $ \parkingStatus -> QPT.updateStatusById parkingStatus parkingTransaction.id
      case mbParkingStatus of
        Just DPT.Booked -> return (DPayment.FulfillmentSucceeded, Just parkingTransaction.id.getId, Just parkingTransaction.paymentOrderId.getId)
        Just DPT.Failed -> return (DPayment.FulfillmentFailed, Just parkingTransaction.id.getId, Just parkingTransaction.paymentOrderId.getId)
        Just DPT.Refunded -> return (DPayment.FulfillmentRefunded, Just parkingTransaction.id.getId, Just parkingTransaction.paymentOrderId.getId)
        Just DPT.RefundInitiated -> return (DPayment.FulfillmentRefundInitiated, Just parkingTransaction.id.getId, Just parkingTransaction.paymentOrderId.getId)
        Just DPT.RefundPending -> return (DPayment.FulfillmentRefundPending, Just parkingTransaction.id.getId, Just parkingTransaction.paymentOrderId.getId)
        Just DPT.RefundFailed -> return (DPayment.FulfillmentRefundFailed, Just parkingTransaction.id.getId, Just parkingTransaction.paymentOrderId.getId)
        _ -> return (DPayment.FulfillmentPending, Just parkingTransaction.id.getId, Just parkingTransaction.paymentOrderId.getId)
    _ -> do
      logError $ "Parking transaction not found for paymentOrderId: " <> paymentOrderId.getId
      return (DPayment.FulfillmentPending, Nothing, Nothing)
  where
    convertPaymentStatusToParkingTransactionStatus = \case
      Payment.CHARGED -> Just DPT.Booked
      Payment.AUTHENTICATION_FAILED -> Just DPT.Failed
      Payment.AUTHORIZATION_FAILED -> Just DPT.Failed
      Payment.JUSPAY_DECLINED -> Just DPT.Failed
      Payment.CANCELLED -> Just DPT.Failed
      Payment.AUTO_REFUNDED -> Just DPT.Refunded
      _ -> Nothing
