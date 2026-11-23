{-# LANGUAGE TypeApplications #-}

module Domain.Action.UI.ParkingBooking
  ( postMultimodalParkingBook,
    postMultimodalParkingMarshalCreate,
    parkingBookingOrderStatusHandler,
    getParkingZones,
    getParkingActive,
    postParkingBookingsExtend,
    postParkingBookingsStop,
    getParkingBookings,
    cityTimeZone,
  )
where

import qualified API.Types.UI.ParkingBooking
import qualified API.Types.UI.TicketService as TicketAPI
import qualified Data.Text
import qualified Domain.Action.UI.TicketService as TicketService
import qualified Domain.Types.BusinessHour as DBH
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.ParkingTransaction as DPT
import qualified Domain.Types.Person
import qualified Domain.Types.Person as SP
import qualified Domain.Types.ServiceCategory as DSC
import qualified Domain.Types.ServicePeopleCategory as DSPC
import qualified Domain.Types.TicketBooking as DTTB
import qualified Domain.Types.TicketBookingService as DTB
import qualified Domain.Types.TicketPlace as DTP
import qualified Domain.Types.TicketService as DTS
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.External.Payment.Interface as Payment
import qualified Kernel.External.Payment.Interface.Types
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PaymentOrder as DOrder
import qualified SharedLogic.TimeWindowBooking as TWB
import qualified SharedLogic.Utils as SLUtils
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant as CQM
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.ParkingTransaction as QPT
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.ServiceCategory as QSC
import qualified Storage.Queries.ServicePeopleCategory as QSPC
import qualified Storage.Queries.TicketBooking as QTB
import qualified Storage.Queries.TicketBookingService as QTBS
import qualified Storage.Queries.TicketPlace as QTicketPlace
import qualified Storage.Queries.TicketService as QTicketService
import qualified Tools.ActorInfo as ActorInfo
import Tools.Error
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
            udf1 = udf1,
            udf2 = Nothing,
            udf3 = Nothing
          }

  let commonMerchantId = Kernel.Types.Id.cast @Domain.Types.Merchant.Merchant @DPayment.Merchant person.merchantId
      commonPersonId = Kernel.Types.Id.cast @Domain.Types.Person.Person @DPayment.Person req.customerId
      createOrderCall = TPayment.createOrder person.merchantId merchantOpCityId Nothing TPayment.ParkingBooking (Just req.customerId.getId) person.clientSdkVersion Nothing
  isMetroTestTransaction <- asks (.isMetroTestTransaction)
  let createWalletCall = TWallet.createWallet person.merchantId merchantOpCityId
  orderResp <- DPayment.createOrderService commonMerchantId (Just $ Kernel.Types.Id.cast merchantOpCityId) commonPersonId Nothing Nothing TPayment.ParkingBooking isMetroTestTransaction createOrderReq createOrderCall (Just createWalletCall) False Nothing False >>= fromMaybeM (InternalError "Failed to create payment order")

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
  Redis.withWaitAndLockRedis lockKey 10 50000 $ do
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
                  emailVerified = Nothing,
                  emailSource = Nothing,
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
                  firebaseAppInstanceId = Nothing,
                  description = Nothing,
                  merchantId = merchantOpCity.merchantId,
                  currentCity = merchantOpCity.city,
                  merchantOperatingCityId = merchantOpCity.id,
                  referralCode = Nothing,
                  referredAt = Nothing,
                  hasTakenValidRide = False,
                  hasPassTill = Nothing,
                  hasDisability = Nothing,
                  createdAt = now,
                  updatedAt = now,
                  blocked = False,
                  blockedAt = Nothing,
                  blockedByRuleId = Nothing,
                  blockedReason = Nothing,
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
                  clientId = Nothing,
                  acceptedPolicies = Nothing,
                  commsConsent = Nothing
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

type AuthInfo = (Maybe (Id Domain.Types.Person.Person), Id Domain.Types.Merchant.Merchant)

cityTimeZone :: Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Environment.Flow Seconds
cityTimeZone cityId = (.timeDiffFromUtc) <$> (getConfig (RiderConfigDimensions {merchantOperatingCityId = cityId.getId}) Nothing >>= fromMaybeM (RiderConfigDoesNotExist cityId.getId))

-- | Open zones of live parking clusters, with each vehicle type's free spaces for the requested window (default: right now).
-- With lat and lon: zones within the radius, nearest first. Without: every open zone in the city, by name.
getParkingZones :: AuthInfo -> Maybe Int -> Maybe Double -> Maybe Double -> Maybe Int -> Maybe UTCTime -> Environment.Flow [API.Types.UI.ParkingBooking.ParkingZoneResp]
getParkingZones (mbPersonId, merchantId) mbDuration mbLat mbLon mbRadius mbStart = do
  cityId <- case mbPersonId of
    Just personId -> (.merchantOperatingCityId) <$> (QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId))
    Nothing -> (.id) <$> CQM.getDefaultMerchantOperatingCity merchantId
  tz <- cityTimeZone cityId
  now <- getCurrentTime
  whenJust mbDuration $ \d -> unless (d > 0) $ throwError $ InvalidRequest "durationMinutes must be positive"
  let start = fromMaybe now mbStart
      window = (start, maybe (addUTCTime 1 start) (\d -> addUTCTime (fromIntegral d * 60) start) mbDuration)
      radius = fromIntegral $ maybe 2000 (min 10000 . max 100) mbRadius
  clusters <- filter (\p -> p.placeType == DTP.Parking && p.status == DTP.Active) <$> QTicketPlace.getTicketPlaces cityId
  zones <- concat <$> forM clusters (\cluster -> map (cluster,) . filter (\z -> z.bookingMode == DTS.TimeWindow && not z.isClosed) <$> QTicketService.getTicketServicesByPlaceId cluster.id.getId)
  let location (cluster, zone) = (,) <$> (zone.lat <|> cluster.lat) <*> (zone.lon <|> cluster.lon)
  nearby <- case (mbLat, mbLon) of
    (Just lat, Just lon) ->
      pure $ sortOn snd [(z, Just d) | z <- zones, Just (zLat, zLon) <- [location z], let d = getHighPrecMeters (distanceBetweenInMeters (LatLong lat lon) (LatLong zLat zLon)), d <= radius]
    (Nothing, Nothing) -> pure [(z, Nothing) | z <- sortOn ((.service) . snd) zones]
    _ -> throwError $ InvalidRequest "lat and lon must be given together"
  fmap catMaybes . forM nearby $ \(z@(cluster, zone), distance) -> do
    vehicleTypes <- TicketService.parkingVehicleTypes zone
    resps <- mapM (mkVehicleType tz now window zone) vehicleTypes
    pure $ case vehicleTypes of
      [] -> Nothing
      ((businessHourId, _, _) : _) ->
        Just
          API.Types.UI.ParkingBooking.ParkingZoneResp
            { serviceId = zone.id,
              name = zone.service,
              placeId = cluster.id,
              clusterName = cluster.name,
              lat = fst <$> location z,
              lon = snd <$> location z,
              distanceMeters = round <$> distance,
              geomGeoJson = zone.geomGeoJson,
              businessHourId,
              maxDurationMinutes = zone.maxDurationMinutes,
              vehicleTypes = resps
            }

mkVehicleType ::
  Seconds ->
  UTCTime ->
  (UTCTime, UTCTime) ->
  DTS.TicketService ->
  (Id DBH.BusinessHour, DSC.ServiceCategory, Id DSPC.ServicePeopleCategory) ->
  Environment.Flow API.Types.UI.ParkingBooking.ParkingVehicleTypeResp
mkVehicleType tz now (start, end) zone (_, category, peopleCategoryId) = do
  freeSpaces <- forM category.availableSeats $ \capacity -> do
    held <- TicketService.heldParkingWindows zone category.id (TWB.localDay tz end) now
    pure $ max 0 (capacity - TWB.peakOverlap start end held)
  peopleCategory <- QSPC.findServicePeopleCategoryById peopleCategoryId (TWB.localDay tz start)
  pure
    API.Types.UI.ParkingBooking.ParkingVehicleTypeResp
      { categoryId = category.id,
        name = category.name,
        peopleCategoryId,
        capacity = category.availableSeats,
        freeSpaces,
        durationTariff = peopleCategory >>= (.durationTariff)
      }

-- | The caller's upcoming and running paid stays.
getParkingActive :: AuthInfo -> Environment.Flow [API.Types.UI.ParkingBooking.ParkingStayResp]
getParkingActive auth = filter ((`elem` [API.Types.UI.ParkingBooking.Reserved, API.Types.UI.ParkingBooking.Active]) . (.status)) <$> parkingStays auth [DTTB.Booked]

-- | The caller's stays: upcoming, running, awaiting payment, and those that ended in the last 24 hours, newest first.
getParkingBookings :: AuthInfo -> Environment.Flow [API.Types.UI.ParkingBooking.ParkingStayResp]
getParkingBookings auth = parkingStays auth [DTTB.Booked, DTTB.Pending]

parkingStays :: AuthInfo -> [DTTB.BookingStatus] -> Environment.Flow [API.Types.UI.ParkingBooking.ParkingStayResp]
parkingStays (mbPersonId, merchantId) statuses = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  city <- CQM.getDefaultMerchantOperatingCity merchantId
  now <- getCurrentTime
  bookings <- concat <$> mapM (QTB.getAllBookingsByPersonIdAndStatus (Just 50) Nothing personId city.id) statuses
  roots <- concat <$> forM bookings (\b -> map (b,) . filter (\r -> isJust r.vehicleNumber && isNothing r.parentBookingServiceId) <$> QTBS.findAllByBookingId b.id)
  stays <- fmap catMaybes . forM roots $ \(booking, root) -> do
    paidStay <- TicketService.parkingStay root
    let mbStay = case paidStay of
          Just TicketService.ParkingStay {stayStart = start, stayEnd = end, stayPaid} -> Just (start, end, if now < start then API.Types.UI.ParkingBooking.Reserved else if end > now then API.Types.UI.ParkingBooking.Active else API.Types.UI.ParkingBooking.Ended, stayPaid)
          Nothing | booking.status == DTTB.Pending -> (\start end -> (start, end, API.Types.UI.ParkingBooking.PendingPayment, root.amount.amount)) <$> root.startTime <*> root.expiryDate
          Nothing -> Nothing
    case (mbStay, root.vehicleNumber) of
      (Just (startTime, endTime, status, amount), Just vehicleNumber) | endTime > addUTCTime (-86400) now -> do
        place <- QTicketPlace.findById booking.ticketPlaceId >>= fromMaybeM (TicketPlaceNotFound booking.ticketPlaceId.getId)
        zone <- QTicketService.findById root.ticketServiceId >>= fromMaybeM (TicketServiceNotFound root.ticketServiceId.getId)
        category <- maybe (pure Nothing) QSC.findById root.serviceCategoryId
        pure $
          Just
            API.Types.UI.ParkingBooking.ParkingStayResp
              { bookingServiceId = root.id,
                ticketBookingShortId = booking.shortId,
                placeId = place.id,
                placeName = place.name,
                serviceId = root.ticketServiceId,
                zoneName = zone.service,
                categoryId = root.serviceCategoryId,
                vehicleType = (.name) <$> category,
                vehicleNumber,
                startTime,
                endTime,
                amount,
                status
              }
      _ -> pure Nothing
  pure $ sortOn (Down . (.startTime)) stays

-- | Pay to extend a paid parking booking. Creates an extension booking through the normal book flow; the stay ends later once it is paid.
postParkingBookingsExtend :: AuthInfo -> Id DTB.TicketBookingService -> API.Types.UI.ParkingBooking.ParkingExtendReq -> Environment.Flow Kernel.External.Payment.Interface.Types.CreateOrderResp
postParkingBookingsExtend auth bookingServiceId req = do
  row <- QTBS.findById bookingServiceId >>= fromMaybeM (InvalidRequest "Parking booking not found")
  booking <- QTB.findById row.ticketBookingId >>= fromMaybeM (InvalidRequest "Parking booking not found")
  chain@(root, _) <- TicketService.parkingChain row
  (_, stayEnd) <- fromMaybeM (InvalidRequest "Only a paid parking booking can be extended") (TicketService.parkingChainWindow chain)
  service <- QTicketService.findById root.ticketServiceId >>= fromMaybeM (TicketServiceNotFound root.ticketServiceId.getId)
  (businessHourId, category, peopleCategoryId) <-
    find (\(_, c, _) -> Just c.id == root.serviceCategoryId) <$> TicketService.parkingVehicleTypes service
      >>= fromMaybeM (InternalError $ "Vehicle type of this booking is no longer set up in zone " <> service.id.getId)
  tz <- cityTimeZone booking.merchantOperatingCityId
  TicketService.postTicketPlacesBook auth booking.ticketPlaceId $
    TicketAPI.TicketBookingReq
      { visitDate = TWB.localDay tz stayEnd,
        ticketSubPlaceId = Nothing,
        services =
          [ TicketAPI.TicketBookingServicesReq
              { serviceId = service.id,
                businessHourId,
                categories = [TicketAPI.TicketBookingCategoryReq {categoryId = category.id, peopleCategories = [TicketAPI.TicketBookingPeopleCategoryReq {peopleCategoryId, numberOfUnits = 1}]}],
                startTime = Nothing,
                durationMinutes = Just req.durationMinutes,
                vehicleNumber = Nothing,
                extendsBookingServiceId = Just root.id
              }
          ]
      }

-- | End a running parking stay now and free its space. Reports the tariff for the time actually parked; nothing is refunded, as in ParkWise.
postParkingBookingsStop :: AuthInfo -> Id DTB.TicketBookingService -> Environment.Flow API.Types.UI.ParkingBooking.ParkingStopResp
postParkingBookingsStop (mbPersonId, _) bookingServiceId = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  row <- QTBS.findById bookingServiceId >>= fromMaybeM (InvalidRequest "Parking booking not found")
  booking <- QTB.findById row.ticketBookingId >>= fromMaybeM (InvalidRequest "Parking booking not found")
  unless (booking.personId == personId) $ throwError $ InvalidRequest "Parking booking not found"
  chain@(root, paid) <- TicketService.parkingChain row
  (stayStart, stayEnd) <- fromMaybeM (InvalidRequest "Only a paid parking booking can be stopped") (TicketService.parkingChainWindow chain)
  now <- getCurrentTime
  when (stayStart > now) $ throwError $ InvalidRequest "Booking has not started yet"
  unless (stayEnd > now) $ throwError $ InvalidRequest "This booking has already ended"
  forM_ paid $ \r ->
    whenJust r.expiryDate $ \end ->
      when (end > now) $ QTBS.updateExpiryDateById (Just (max now (fromMaybe now r.startTime))) r.id
  service <- QTicketService.findById root.ticketServiceId >>= fromMaybeM (TicketServiceNotFound root.ticketServiceId.getId)
  (_, _, peopleCategoryId) <-
    find (\(_, c, _) -> Just c.id == root.serviceCategoryId) <$> TicketService.parkingVehicleTypes service
      >>= fromMaybeM (InternalError $ "Vehicle type of this booking is no longer set up in zone " <> service.id.getId)
  tz <- cityTimeZone booking.merchantOperatingCityId
  tariff <- TicketService.parkingTariff peopleCategoryId (TWB.localDay tz stayStart)
  pure
    API.Types.UI.ParkingBooking.ParkingStopResp
      { finalAmount = TWB.calculateFee tz tariff stayStart now,
        durationMinutes = ceiling (realToFrac (diffUTCTime now stayStart) / 60 :: Double)
      }
