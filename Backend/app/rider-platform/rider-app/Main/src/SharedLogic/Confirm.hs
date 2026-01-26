{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Confirm where

import Control.Monad.Extra (anyM, maybeM)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Domain.Action.UI.Estimate as UEstimate
import qualified Domain.Action.UI.Quote as DQuote
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingPartiesLink as DBPL
import qualified Domain.Types.BookingStatus as DRB
import Domain.Types.CancellationReason
import qualified Domain.Types.DeliveryDetails as DTDD
import qualified Domain.Types.EstimateStatus as DEstimate
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.Location as DL
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.ParcelType as DParcel
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PersonFlowStatus as DPFS
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.SearchRequest as DSReq
import qualified Domain.Types.Trip as Trip
import qualified Domain.Types.VehicleVariant as DV
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.External.Types
import Kernel.Prelude
import Kernel.Randomizer (getRandomElement)
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import Lib.SessionizerMetrics.Types.Event
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.DisplayBookingId as DBI
import SharedLogic.JobScheduler
import SharedLogic.MerchantPaymentMethod
import qualified SharedLogic.Payment as SPayment
-- import SharedLogic.Type
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.InsuranceConfig as CQInsuranceConfig
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as QMPM
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CMSUC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.Booking as QRideB
import qualified Storage.Queries.BookingPartiesLink as QBPL
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.ParcelDetails as QParcel
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.SearchRequest as QSReq
import qualified Storage.Queries.SearchRequestPartiesLink as QSRPL
import qualified Storage.Queries.Transformers.Booking as QTB
import Tools.Error
import Tools.Event
import TransactionLogs.Types

data DConfirmReq = DConfirmReq
  { personId :: Id DP.Person,
    quote :: DQuote.Quote,
    dashboardAgentId :: Maybe Text,
    paymentMethodId :: Maybe Payment.PaymentMethodId,
    paymentInstrument :: Maybe DMPM.PaymentInstrument,
    merchant :: DM.Merchant
  }

data DConfirmRes = DConfirmRes
  { providerId :: Text,
    providerUrl :: BaseUrl,
    itemId :: Text,
    fromLoc :: DL.Location,
    toLoc :: Maybe DL.Location,
    stops :: [DL.Location],
    vehicleVariant :: DV.VehicleVariant,
    bppQuoteId :: Text,
    booking :: DRB.Booking,
    riderPhone :: Maybe Text,
    riderName :: Maybe Text,
    searchRequestId :: Id DSReq.SearchRequest,
    merchant :: DM.Merchant,
    city :: Context.City,
    maxEstimatedDistance :: Maybe Distance,
    paymentMethodInfo :: Maybe DMPM.PaymentMethodInfo,
    isStripe :: Bool,
    paymentInstrument :: Maybe DMPM.PaymentInstrument,
    confirmResDetails :: Maybe DConfirmResDetails,
    isAdvanceBookingEnabled :: Maybe Bool,
    isInsured :: Maybe Bool,
    insuredAmount :: Maybe Text,
    paymentMode :: Maybe DMPM.PaymentMode
  }
  deriving (Show, Generic)

data DConfirmResDetails = DConfirmResDelivery DTDD.DeliveryDetails
  deriving (Show, Generic)

tryInitTriggerLock :: (Redis.HedisFlow m r) => Id DSReq.SearchRequest -> m Bool
tryInitTriggerLock searchRequestId = do
  let initTriggerLockKey = "Customer:Init:Trigger:SearchRequestId:-" <> searchRequestId.getId
      lockExpiryTime = 10 -- Note: this value should be decided based on the delay between consecutive quotes in on_select api & also considering reallocation.
  Redis.tryLockRedis initTriggerLockKey lockExpiryTime

confirm ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EventStreamFlow m r,
    HasField "shortDurationRetryCfg" r RetryCfg,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl, "nwAddress" ::: BaseUrl, "version" ::: DeploymentVersion],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    EncFlow m r,
    SchedulerFlow r,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "blackListedJobs" r [Text]
  ) =>
  DConfirmReq ->
  m DConfirmRes
confirm DConfirmReq {..} = do
  now <- getCurrentTime
  when (quote.validTill < now) $ throwError (InvalidRequest $ "Quote expired " <> show quote.id) -- init validation check
  (bppQuoteId, mbEsimateId) <- getBppQuoteId now quote.quoteDetails
  searchRequest <- QSReq.findById quote.requestId >>= fromMaybeM (SearchRequestNotFound quote.requestId.getId)
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  when (merchant.onlinePayment && paymentInstrument `notElem` [Just DMPM.Cash, Just DMPM.BoothOnline]) $ do
    when (isNothing paymentMethodId) $ throwError PaymentMethodRequired
    SPayment.updateDefaultPersonPaymentMethodId person paymentMethodId -- Make payment method as default payment method for customer
  activeBooking <- QRideB.findLatestSelfAndPartyBookingByRiderId personId --This query also checks for booking parties
  case activeBooking of
    Just booking | not (isMeterRide quote.quoteDetails) -> DQuote.processActiveBooking booking OnConfirm
    _ -> pure ()
  -- when (searchRequest.validTill < now) $
  --   throwError SearchRequestExpired
  unless (searchRequest.riderId == personId) $ QSReq.updateRiderId personId searchRequest.id
  let fromLocation = searchRequest.fromLocation
      mbToLocation = searchRequest.toLocation
      stops = searchRequest.stops
  let merchantOperatingCityId = searchRequest.merchantOperatingCityId
  city <- CQMOC.findById merchantOperatingCityId >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound merchantOperatingCityId.getId)
  exophone <- findRandomExophone merchantOperatingCityId
  let isScheduled = (maybe False not searchRequest.isMultimodalSearch) && merchant.scheduleRideBufferTime `addUTCTime` now < searchRequest.startTime
  (booking, bookingParties) <- buildBooking merchant personId searchRequest bppQuoteId quote fromLocation mbToLocation exophone now Nothing paymentMethodId paymentInstrument isScheduled searchRequest.disabilityTag searchRequest.configInExperimentVersions person.paymentMode dashboardAgentId
  -- check also for the booking parties
  checkIfActiveRidePresentForParties bookingParties
  when isScheduled $ do
    let scheduledRideReminderTime = addUTCTime (- (merchant.scheduleRideBufferTime + 10 * 60)) booking.startTime
    let scheduleAfter = diffUTCTime scheduledRideReminderTime now
    when (scheduleAfter > 0) $ do
      let dfCalculationJobTs = max 2 scheduleAfter
          scheduledRidePopupToRiderJobData = ScheduledRidePopupToRiderJobData {bookingId = booking.id}
      createJobIn @_ @'ScheduledRidePopupToRider (Just searchRequest.merchantId) (Just merchantOperatingCityId) dfCalculationJobTs (scheduledRidePopupToRiderJobData :: ScheduledRidePopupToRiderJobData)
  isValueAddNP <- CQVAN.isValueAddNP booking.providerId
  riderPhone <-
    if isValueAddNP
      then mapM decrypt person.mobileNumber
      else pure . Just $ prependZero booking.primaryExophone
  let riderName = person.firstName
  triggerBookingCreatedEvent BookingEventData {booking = booking}
  void $ QRideB.createBooking booking
  void $ QBPL.createMany bookingParties
  unless isScheduled $
    void $ QPFS.updateStatus personId DPFS.WAITING_FOR_DRIVER_ASSIGNMENT {bookingId = booking.id, validTill = searchRequest.validTill, fareProductType = Just (QTB.getFareProductType booking.bookingDetails), tripCategory = booking.tripCategory}
  whenJust mbEsimateId $ QEstimate.updateStatus DEstimate.COMPLETED
  confirmResDetails <- case quote.tripCategory of
    Just (Trip.Delivery _) -> Just <$> makeDeliveryDetails booking bookingParties
    _ -> return Nothing

  -- FIXME Currently paymentMethodId used in two different ways. Find better way to differentiate between these two
  (paymentMethodInfo, isStripe) <- case paymentMethodId of
    Nothing -> pure (Nothing, False)
    Just paymentMethodId' -> do
      QMPM.findById (Id paymentMethodId') >>= \case
        Just merchantPaymentMethod -> do
          -- 1. merchantPaymentMethod.id from db
          pure (Just $ mkPaymentMethodInfo merchantPaymentMethod, False)
        Nothing -> do
          -- 2. paymentMethodId which provided by Stripe SDK
          if merchant.onlinePayment && paymentInstrument `notElem` [Just DMPM.Cash, Just DMPM.BoothOnline]
            then pure (Nothing, True)
            else pure (Nothing, False)

  return $
    DConfirmRes
      { booking,
        providerId = quote.providerId,
        providerUrl = quote.providerUrl,
        itemId = booking.bppEstimateId,
        fromLoc = fromLocation,
        toLoc = mbToLocation,
        vehicleVariant = DV.castServiceTierToVariant quote.vehicleServiceTierType,
        bppQuoteId = bppQuoteId,
        searchRequestId = searchRequest.id,
        maxEstimatedDistance = searchRequest.maxDistance,
        paymentMethodInfo = paymentMethodInfo,
        paymentInstrument,
        confirmResDetails,
        isAdvanceBookingEnabled = searchRequest.isAdvanceBookingEnabled,
        isInsured = Just $ booking.isInsured,
        insuredAmount = booking.driverInsuredAmount,
        paymentMode = booking.paymentMode,
        ..
      }
  where
    prependZero :: Text -> Text
    prependZero str = "0" <> str

    isMeterRide = \case
      DQuote.MeterRideDetails _ -> True
      _ -> False

    getBppQuoteId now = \case
      DQuote.OneWayDetails details -> pure (details.quoteId, Nothing)
      DQuote.AmbulanceDetails driverOffer -> getBppQuoteIdFromDriverOffer driverOffer now
      DQuote.DeliveryDetails driverOffer -> getBppQuoteIdFromDriverOffer driverOffer now
      DQuote.RentalDetails rentalDetails -> pure (rentalDetails.id.getId, Nothing)
      DQuote.DriverOfferDetails driverOffer -> getBppQuoteIdFromDriverOffer driverOffer now
      DQuote.OneWaySpecialZoneDetails details -> pure (details.quoteId, Nothing)
      DQuote.InterCityDetails details -> pure (details.id.getId, Nothing)
      DQuote.MeterRideDetails details -> pure (details.quoteId, Nothing)

    getBppQuoteIdFromDriverOffer driverOffer now = do
      estimate <- QEstimate.findById driverOffer.estimateId >>= fromMaybeM EstimateNotFound
      when (UEstimate.isCancelled estimate.status) $ throwError $ EstimateCancelled estimate.id.getId
      when (driverOffer.validTill < now) $ throwError $ QuoteExpired quote.id.getId
      pure (driverOffer.bppQuoteId, Just estimate.id)

    checkIfActiveRidePresentForParties :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [DBPL.BookingPartiesLink] -> m ()
    checkIfActiveRidePresentForParties bookingParties = do
      let partyIds = map (.partyId) bookingParties
      isActiveBookingPresentForAnyParty <- anyM (\partyId -> isJust <$> QRideB.findLatestSelfAndPartyBookingByRiderId partyId) partyIds
      when isActiveBookingPresentForAnyParty $ throwError $ InvalidRequest "ACTIVE_BOOKING_PRESENT_FOR_OTHER_INVOLVED_PARTIES"

    makeDeliveryDetails :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r, EncFlow m r) => DRB.Booking -> [DBPL.BookingPartiesLink] -> m DConfirmResDetails
    makeDeliveryDetails booking bookingParties = do
      senderParty <- fromMaybeM (InternalError "SenderParty not found") $ find (\party -> party.partyType == Trip.DeliveryParty Trip.Sender) bookingParties
      receiverParty <- fromMaybeM (InternalError "ReceiverParty not found") $ find (\party -> party.partyType == Trip.DeliveryParty Trip.Receiver) bookingParties
      senderPerson <- QPerson.findById senderParty.partyId >>= fromMaybeM (PersonDoesNotExist senderParty.partyId.getId)
      encSenderMobileNumber <- senderPerson.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber")
      receiverPerson <- QPerson.findById receiverParty.partyId >>= fromMaybeM (PersonDoesNotExist receiverParty.partyId.getId)
      encReceiverMobileNumber <- receiverPerson.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber")
      senderMobileNumber <- decrypt encSenderMobileNumber
      receiverMobileNumber <- decrypt encReceiverMobileNumber
      toLocation <- case booking.bookingDetails of
        DRB.DeliveryDetails details -> return details.toLocation
        _ -> throwError (InternalError $ "DeliveryBookingDetails not found for booking" <> booking.id.getId)
      (Trip.DeliveryParty initiatedBy) <- booking.initiatedBy & fromMaybeM (InternalError $ "BookingInitiatedBy not found for booking" <> booking.id.getId)
      let (parcelType, parcelQuantity) = case booking.bookingDetails of
            DRB.DeliveryDetails details -> (Just details.parcelType, details.parcelQuantity)
            _ -> (Nothing, Nothing)
      return $
        DConfirmResDelivery $
          DTDD.DeliveryDetails
            { senderDetails =
                DTDD.PersonDetails
                  { DTDD.name = senderParty.partyName,
                    DTDD.phoneNumber = senderMobileNumber,
                    DTDD.countryCode = Nothing,
                    DTDD.address = booking.fromLocation.address
                  },
              receiverDetails =
                DTDD.PersonDetails
                  { DTDD.name = receiverParty.partyName,
                    DTDD.phoneNumber = receiverMobileNumber,
                    DTDD.countryCode = Nothing,
                    DTDD.address = toLocation.address
                  },
              DTDD.initiatedAs = initiatedBy,
              ..
            }

buildBooking ::
  ( EsqDBFlow m r,
    MonadFlow m,
    CacheFlow m r,
    HasFlowEnv m r '["version" ::: DeploymentVersion]
  ) =>
  DM.Merchant ->
  Id DP.Person ->
  DSReq.SearchRequest ->
  Text ->
  DQuote.Quote ->
  DL.Location ->
  Maybe DL.Location ->
  DExophone.Exophone ->
  UTCTime ->
  Maybe Text ->
  Maybe Payment.PaymentMethodId ->
  Maybe DMPM.PaymentInstrument ->
  Bool ->
  Maybe Text ->
  [LYT.ConfigVersionMap] ->
  Maybe DMPM.PaymentMode ->
  Maybe Text ->
  m (DRB.Booking, [DBPL.BookingPartiesLink])
buildBooking merchant riderId searchRequest bppQuoteId quote fromLoc mbToLoc exophone now otpCode paymentMethodId paymentInstrument isScheduled disabilityTag configInExperimentVersions paymentMode dashboardAgentId = do
  id <- generateGUID
  let bookingId = Id id
  displayBookingId <- Just <$> DBI.generateDisplayBookingId merchant.shortId bookingId now
  bookingDetails <- buildBookingDetails
  bookingParties <- buildPartiesLinks id
  deploymentVersion <- asks (.version)
  (isInsured, insuredAmount, driverInsuredAmount) <- isBookingInsured
  return $
    ( DRB.Booking
        { id = bookingId,
          clientId = searchRequest.clientId,
          transactionId = searchRequest.id.getId,
          bppBookingId = Nothing,
          fulfillmentId = Just bppQuoteId,
          quoteId = Just quote.id,
          paymentMethodId,
          paymentUrl = Nothing,
          status = DRB.NEW,
          providerId = quote.providerId,
          primaryExophone = exophone.primaryPhone,
          providerUrl = quote.providerUrl,
          bppEstimateId = quote.itemId,
          isBookingUpdated = False,
          startTime = searchRequest.startTime,
          returnTime = searchRequest.returnTime,
          roundTrip = searchRequest.roundTrip,
          riderId,
          fromLocation = fromLoc,
          initialPickupLocation = fromLoc,
          estimatedFare = quote.estimatedFare,
          discount = quote.discount,
          estimatedTotalFare = quote.estimatedTotalFare,
          estimatedDistance = searchRequest.distance,
          estimatedDuration = searchRequest.estimatedRideDuration,
          estimatedStaticDuration = searchRequest.estimatedRideStaticDuration,
          bookingDetails,
          isPetRide = fromMaybe False searchRequest.isPetRide,
          tripTerms = quote.tripTerms,
          merchantId = searchRequest.merchantId,
          merchantOperatingCityId = searchRequest.merchantOperatingCityId,
          specialLocationTag = quote.specialLocationTag,
          isScheduled = isScheduled,
          createdAt = now,
          updatedAt = now,
          billingCategory = quote.billingCategory,
          serviceTierName = quote.serviceTierName,
          vehicleServiceTierType = quote.vehicleServiceTierType,
          vehicleServiceTierSeatingCapacity = quote.vehicleServiceTierSeatingCapacity,
          vehicleServiceTierAirConditioned = quote.vehicleServiceTierAirConditioned,
          vehicleIconUrl = quote.vehicleIconUrl,
          isAirConditioned = quote.isAirConditioned,
          serviceTierShortDesc = quote.serviceTierShortDesc,
          clientBundleVersion = quote.clientBundleVersion,
          clientSdkVersion = quote.clientSdkVersion,
          clientDevice = quote.clientDevice,
          clientConfigVersion = quote.clientConfigVersion,
          backendConfigVersion = quote.backendConfigVersion,
          backendAppVersion = Just deploymentVersion.getDeploymentVersion,
          paymentStatus = Nothing,
          distanceUnit = searchRequest.distanceUnit,
          specialLocationName = quote.specialLocationName,
          isDashboardRequest = searchRequest.isDashboardRequest,
          tripCategory = quote.tripCategory,
          initiatedBy = searchRequest.initiatedBy,
          hasStops = searchRequest.hasStops,
          isReferredRide = searchRequest.driverIdentifier $> True,
          preferSafetyPlus = quote.isSafetyPlus,
          recentLocationId = searchRequest.recentLocationId,
          isMultimodalSearch = searchRequest.isMultimodalSearch,
          multimodalSearchRequestId = searchRequest.multimodalSearchRequestId,
          vehicleCategory = searchRequest.vehicleCategory,
          dashboardAgentId,
          -- Commission is calculated on BPP side (requires fare policy config).
          -- BAP doesn't have access to fare policy, so commission remains Nothing here.
          -- If commission is needed on BAP, it should flow from BPP via Beckn protocol extension.
          commission = Nothing,
          ..
        },
      bookingParties
    )
  where
    buildPartiesLinks bookingId = case quote.quoteDetails of
      DQuote.DeliveryDetails _ -> makeDeliveryParties bookingId
      _ -> pure []
    buildBookingDetails = case quote.quoteDetails of
      DQuote.OneWayDetails _ -> DRB.OneWayDetails <$> buildOneWayDetails Nothing
      DQuote.AmbulanceDetails _ -> DRB.AmbulanceDetails <$> buildAmbulanceDetails
      DQuote.DeliveryDetails _ -> DRB.DeliveryDetails <$> buildDeliveryDetails
      DQuote.RentalDetails _ -> pure $ DRB.RentalDetails (DRB.RentalBookingDetails {stopLocation = mbToLoc, ..})
      DQuote.DriverOfferDetails driverOffer -> DRB.DriverOfferDetails <$> (buildOneWayDetails driverOffer.isUpgradedToCab)
      DQuote.OneWaySpecialZoneDetails _ -> DRB.OneWaySpecialZoneDetails <$> buildOneWaySpecialZoneDetails
      DQuote.InterCityDetails _ -> DRB.InterCityDetails <$> buildInterCityDetails
      DQuote.MeterRideDetails _ -> DRB.MeterRideDetails <$> buildMeterRideDetails

    buildInterCityDetails = do
      -- we need to throw errors here because of some redundancy of our domain model
      toLocation <- mbToLoc & fromMaybeM (InternalError "toLocation is null for one way search request")
      distance <- searchRequest.distance & fromMaybeM (InternalError "distance is null for one way search request")
      let stops = searchRequest.stops
      pure DRB.InterCityBookingDetails {..}
    buildOneWayDetails isUpgradedToCab = do
      -- we need to throw errors here because of some redundancy of our domain model
      toLocation <- mbToLoc & fromMaybeM (InternalError "toLocation is null for one way search request")
      distance <- searchRequest.distance & fromMaybeM (InternalError "distance is null for one way search request")
      let stops = searchRequest.stops
      pure DRB.OneWayBookingDetails {..}
    buildAmbulanceDetails = do
      -- we need to throw errors here because of some redundancy of our domain model
      toLocation <- mbToLoc & fromMaybeM (InternalError "toLocation is null for one way search request")
      distance <- searchRequest.distance & fromMaybeM (InternalError "distance is null for one way search request")
      pure DRB.AmbulanceBookingDetails {..}
    buildOneWaySpecialZoneDetails = do
      -- we need to throw errors here because of some redundancy of our domain model
      toLocation <- mbToLoc & fromMaybeM (InternalError "toLocation is null for one way search request")
      let stops = searchRequest.stops
      distance <- searchRequest.distance & fromMaybeM (InternalError "distance is null for one way search request")
      pure DRB.OneWaySpecialZoneBookingDetails {..}
    buildDeliveryDetails = do
      toLocation <- mbToLoc & fromMaybeM (InternalError "toLocation is null for one way search request")
      distance <- searchRequest.distance & fromMaybeM (InternalError "distance is null for one way search request")
      parcelDetails <- QParcel.findBySearchRequestId searchRequest.id
      let parcelType = maybe (DParcel.Others "Unknown") (.parcelType) parcelDetails
          parcelQuantity = parcelDetails >>= (.quantity)
      pure DRB.DeliveryBookingDetails {..}
    buildMeterRideDetails = pure DRB.MeterRideBookingDetails {toLocation = Nothing, distanceCovered = Nothing}

    makeDeliveryParties :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Text -> m [DBPL.BookingPartiesLink]
    makeDeliveryParties bookingId = do
      allSearchReqParties <- QSRPL.findAllBySearchRequestId searchRequest.id
      when (length allSearchReqParties < 2) $ throwError $ InternalError "No parties found for search request delivery"
      mapM
        ( \party -> do
            bookingPartyId <- generateGUID
            return $
              DBPL.BookingPartiesLink
                { id = Id bookingPartyId,
                  bookingId = Id bookingId,
                  partyId = party.partyId,
                  partyType = party.partyType,
                  partyName = party.partyName,
                  isActive = True,
                  createdAt = now,
                  updatedAt = now
                }
        )
        allSearchReqParties

    isBookingInsured :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => m (Bool, Maybe Text, Maybe Text)
    isBookingInsured = do
      insuranceConfig <- maybeM (pure Nothing) (\tp -> CQInsuranceConfig.getInsuranceConfig searchRequest.merchantId searchRequest.merchantOperatingCityId tp (DV.castServiceTierToVehicleCategory quote.vehicleServiceTierType)) (pure quote.tripCategory)
      pure $
        maybe
          (False, Nothing, Nothing)
          ( \inc ->
              case inc.allowedVehicleServiceTiers of
                Just allowedTiers -> case quote.vehicleServiceTierType `elem` allowedTiers of
                  True -> (True, inc.insuredAmount, inc.driverInsuredAmount)
                  False -> (False, inc.insuredAmount, inc.driverInsuredAmount)
                Nothing -> (True, inc.insuredAmount, inc.driverInsuredAmount)
          )
          insuranceConfig

findRandomExophone :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m DExophone.Exophone
findRandomExophone merchantOperatingCityId = do
  merchantServiceUsageConfig <- CMSUC.findByMerchantOperatingCityId merchantOperatingCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound $ "merchantOperatingCityId:- " <> merchantOperatingCityId.getId)
  exophones <- CQExophone.findByMerchantOperatingCityIdAndService merchantOperatingCityId merchantServiceUsageConfig.getExophone
  nonEmptyExophones <- case exophones of
    [] -> throwError $ ExophoneNotFound merchantOperatingCityId.getId
    e : es -> pure $ e :| es
  getRandomElement nonEmptyExophones
