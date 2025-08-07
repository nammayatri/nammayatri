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
import qualified Domain.Types.Journey as DJ
import qualified Domain.Types.JourneyLeg as DJL
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
import Kernel.External.Maps.Google.MapsClient.Types (LatLngV2 (..))
import Kernel.External.MultiModal.Interface.Types (MultiModalAgency (..))
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
import qualified Lib.JourneyLeg.Types as JLT
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import Lib.SessionizerMetrics.Types.Event
import qualified Lib.Yudhishthira.Types as LYT
import SharedLogic.JobScheduler
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.InsuranceConfig as CQInsuranceConfig
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CMSUC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.Booking as QRideB
import qualified Storage.Queries.BookingPartiesLink as QBPL
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Journey as QJourney
import qualified Storage.Queries.JourneyExtra as QJourneyExtra
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.ParcelDetails as QParcel
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.SearchRequest as QSReq
import qualified Storage.Queries.SearchRequest as QSearchRequest
import qualified Storage.Queries.SearchRequestPartiesLink as QSRPL
import qualified Storage.Queries.Transformers.Booking as QTB
import Tools.Error
import Tools.Event
import TransactionLogs.Types

data DConfirmReq = DConfirmReq
  { personId :: Id DP.Person,
    quote :: DQuote.Quote,
    paymentMethodId :: Maybe Payment.PaymentMethodId
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
    confirmResDetails :: Maybe DConfirmResDetails,
    isAdvanceBookingEnabled :: Maybe Bool,
    isInsured :: Maybe Bool,
    insuredAmount :: Maybe Text,
    mbJourneyId :: Maybe (Id DJ.Journey)
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
    HasField "jobInfoMap" r (M.Map Text Bool)
  ) =>
  DConfirmReq ->
  m DConfirmRes
confirm DConfirmReq {..} = do
  now <- getCurrentTime
  when (quote.validTill < now) $ throwError (InvalidRequest $ "Quote expired " <> show quote.id) -- init validation check
  (bppQuoteId, mbEsimateId) <- getBppQuoteId now quote.quoteDetails
  searchRequest <- QSReq.findById quote.requestId >>= fromMaybeM (SearchRequestNotFound quote.requestId.getId)
  merchant <- CQM.findById searchRequest.merchantId >>= fromMaybeM (MerchantNotFound searchRequest.merchantId.getId)
  when merchant.onlinePayment $ do
    when (isNothing paymentMethodId) $ throwError PaymentMethodRequired
    QPerson.updateDefaultPaymentMethodId paymentMethodId personId -- Make payment method as default payment method for customer
  activeBooking <- QRideB.findLatestSelfAndPartyBookingByRiderId personId --This query also checks for booking parties
  case activeBooking of
    Just booking | not (isMeterRide quote.quoteDetails) -> DQuote.processActiveBooking booking OnConfirm
    _ -> pure ()
  -- when (searchRequest.validTill < now) $
  --   throwError SearchRequestExpired
  unless (searchRequest.riderId == personId) $ throwError AccessDenied
  let fromLocation = searchRequest.fromLocation
      mbToLocation = searchRequest.toLocation
      stops = searchRequest.stops
  let merchantOperatingCityId = searchRequest.merchantOperatingCityId
  city <- CQMOC.findById merchantOperatingCityId >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound merchantOperatingCityId.getId)
  exophone <- findRandomExophone merchantOperatingCityId
  let isScheduled = (maybe False not searchRequest.isMultimodalSearch) && merchant.scheduleRideBufferTime `addUTCTime` now < searchRequest.startTime
  (booking, bookingParties) <- buildBooking searchRequest bppQuoteId quote fromLocation mbToLocation exophone now Nothing paymentMethodId isScheduled searchRequest.disabilityTag searchRequest.configInExperimentVersions
  -- check also for the booking parties
  checkIfActiveRidePresentForParties bookingParties
  when isScheduled $ do
    let scheduledRideReminderTime = addUTCTime (- (merchant.scheduleRideBufferTime + 10 * 60)) booking.startTime
    let scheduleAfter = diffUTCTime scheduledRideReminderTime now
    when (scheduleAfter > 0) $ do
      let dfCalculationJobTs = max 2 scheduleAfter
          scheduledRidePopupToRiderJobData = ScheduledRidePopupToRiderJobData {bookingId = booking.id}
      createJobIn @_ @'ScheduledRidePopupToRider (Just searchRequest.merchantId) (Just merchantOperatingCityId) dfCalculationJobTs (scheduledRidePopupToRiderJobData :: ScheduledRidePopupToRiderJobData)
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
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
    void $ QPFS.updateStatus searchRequest.riderId DPFS.WAITING_FOR_DRIVER_ASSIGNMENT {bookingId = booking.id, validTill = searchRequest.validTill, fareProductType = Just (QTB.getFareProductType booking.bookingDetails), tripCategory = booking.tripCategory}
  whenJust mbEsimateId $ QEstimate.updateStatus DEstimate.COMPLETED
  confirmResDetails <- case quote.tripCategory of
    Just (Trip.Delivery _) -> Just <$> makeDeliveryDetails booking bookingParties
    _ -> return Nothing
  -- can we do something
  mbJourneyId <- mkJourneyForQuote searchRequest quote personId isScheduled
  logDebug $ "mbJourneyId: " <> show mbJourneyId
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
        paymentMethodInfo = Nothing, -- can be removed later
        confirmResDetails,
        isAdvanceBookingEnabled = searchRequest.isAdvanceBookingEnabled,
        isInsured = Just $ booking.isInsured,
        insuredAmount = booking.driverInsuredAmount,
        mbJourneyId = Just $ mbJourneyId,
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
  DSReq.SearchRequest ->
  Text ->
  DQuote.Quote ->
  DL.Location ->
  Maybe DL.Location ->
  DExophone.Exophone ->
  UTCTime ->
  Maybe Text ->
  Maybe Payment.PaymentMethodId ->
  Bool ->
  Maybe Text ->
  [LYT.ConfigVersionMap] ->
  m (DRB.Booking, [DBPL.BookingPartiesLink])
buildBooking searchRequest bppQuoteId quote fromLoc mbToLoc exophone now otpCode paymentMethodId isScheduled disabilityTag configInExperimentVersions = do
  id <- generateGUID
  bookingDetails <- buildBookingDetails
  bookingParties <- buildPartiesLinks id
  deploymentVersion <- asks (.version)
  let (skipBooking, journeyId) = fromMaybe (Nothing, Nothing) $ (\j -> (Just j.skipBooking, Just (Id j.journeyId))) <$> searchRequest.journeyLegInfo
  (isInsured, insuredAmount, driverInsuredAmount) <- isBookingInsured
  return $
    ( DRB.Booking
        { id = Id id,
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
          riderId = searchRequest.riderId,
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
          journeyLegOrder = searchRequest.journeyLegInfo <&> (.journeyLegOrder),
          isDeleted = Just False,
          isSkipped = skipBooking,
          journeyId,
          journeyLegStatus = Nothing,
          preferSafetyPlus = quote.isSafetyPlus,
          recentLocationId = searchRequest.recentLocationId,
          isMultimodalSearch = searchRequest.isMultimodalSearch,
          journeyRouteDetails = searchRequest.journeyRouteDetails,
          journeyLegId = searchRequest.journeyLegId,
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

mkJourneyForQuote :: (CacheFlow m r, EsqDBFlow m r) => DSReq.SearchRequest -> DQuote.Quote -> Id DP.Person -> Bool -> m (Id DJ.Journey)
mkJourneyForQuote searchRequest quote personId isScheduled = do
  let journeyId = searchRequest.journeyLegInfo <&> (.journeyId)
  case journeyId of
    Just jId -> pure (Id jId)
    Nothing -> do
      case searchRequest.multiModalSearchRequestId of
        Just mmSearchId -> do
          existingJourneys <- QJourney.findByMultiModalSearchId (Just mmSearchId)
          case existingJourneys of
            -- filter based on isNormalRideJourney
            existingJourneys' -> do
              let normalRideJourneys = filter (\journey -> fromMaybe False journey.isNormalRideJourney) existingJourneys'
              case normalRideJourneys of
                (existingJourney : _) -> do
                  updateJourneyWithQuote existingJourney.id quote searchRequest isScheduled
                  return existingJourney.id
                [] -> do
                  createNewJourneyForTaxiQuote searchRequest quote personId
        Nothing -> do
          existingJourneys <- QJourney.findBySearchId searchRequest.id.getId
          case existingJourneys of
            (existingJourney : _) -> do
              updateJourneyWithQuote existingJourney.id quote searchRequest isScheduled
              return existingJourney.id
            [] -> do
              createNewJourneyForTaxiQuote searchRequest quote personId

-- fallback code for backward compatibility
createNewJourneyForTaxiQuote :: (CacheFlow m r, EsqDBFlow m r) => DSReq.SearchRequest -> DQuote.Quote -> Id DP.Person -> m (Id DJ.Journey)
createNewJourneyForTaxiQuote searchRequest quote personId = do
  journeyGuid <- generateGUID
  journeyLegGuid <- generateGUID
  now <- getCurrentTime

  let estimatedMinFare = Just quote.estimatedFare.amount
      estimatedMaxFare = Just quote.estimatedFare.amount

  let journey =
        DJ.Journey
          { id = journeyGuid,
            convenienceCost = 0,
            estimatedDistance = fromMaybe (Distance 0 Meter) searchRequest.distance,
            estimatedDuration = searchRequest.estimatedRideDuration,
            isPaymentSuccess = Just True,
            totalLegs = 1,
            modes = [Trip.Taxi],
            searchRequestId = searchRequest.id.getId,
            multimodalSearchRequestId = searchRequest.multiModalSearchRequestId,
            merchantId = searchRequest.merchantId,
            status = DJ.INPROGRESS,
            riderId = personId,
            startTime = Just searchRequest.startTime,
            endTime = Nothing,
            merchantOperatingCityId = searchRequest.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now,
            recentLocationId = searchRequest.recentLocationId,
            isPublicTransportIncluded = Just False,
            relevanceScore = Nothing,
            hasPreferredServiceTier = Nothing,
            hasPreferredTransitModes = Just False,
            fromLocation = searchRequest.fromLocation,
            toLocation = searchRequest.toLocation,
            paymentOrderShortId = Nothing,
            journeyExpiryTime = Nothing,
            isNormalRideJourney = Just True
          }

  let journeyLeg =
        DJL.JourneyLeg
          { id = journeyLegGuid,
            journeyId = journeyGuid,
            sequenceNumber = 0,
            mode = Trip.Taxi,
            startLocation = LatLngV2 searchRequest.fromLocation.lat searchRequest.fromLocation.lon,
            endLocation = case searchRequest.toLocation of
              Just toLoc -> LatLngV2 toLoc.lat toLoc.lon
              Nothing -> LatLngV2 searchRequest.fromLocation.lat searchRequest.fromLocation.lon,
            distance = searchRequest.distance,
            duration = searchRequest.estimatedRideDuration,
            agency = Just $ MultiModalAgency {name = "NAMMA_YATRI", gtfsId = Nothing},
            fromArrivalTime = Nothing,
            fromDepartureTime = Just searchRequest.startTime,
            toArrivalTime =
              searchRequest.estimatedRideDuration >>= \duration ->
                Just $ addUTCTime (fromIntegral $ getSeconds duration) searchRequest.startTime,
            toDepartureTime = Nothing,
            fromStopDetails = Nothing,
            toStopDetails = Nothing,
            routeDetails = [],
            serviceTypes = Nothing,
            estimatedMinFare = estimatedMinFare,
            estimatedMaxFare = estimatedMaxFare,
            merchantId = Just searchRequest.merchantId,
            merchantOperatingCityId = Just searchRequest.merchantOperatingCityId,
            createdAt = now,
            updatedAt = now,
            legSearchId = Just searchRequest.id.getId,
            isDeleted = Just False,
            isSkipped = Just False,
            changedBusesInSequence = Nothing,
            finalBoardedBusNumber = Nothing,
            entrance = Nothing,
            exit = Nothing,
            status = Nothing,
            osmEntrance = Nothing,
            osmExit = Nothing,
            straightLineEntrance = Nothing,
            straightLineExit = Nothing
          }

  let journeySearchData =
        JLT.JourneySearchData
          { journeyId = journeyGuid.getId,
            journeyLegOrder = 0,
            agency = Nothing,
            skipBooking = False,
            convenienceCost = 0,
            pricingId = Just quote.id.getId,
            onSearchFailed = Nothing,
            isDeleted = Nothing
          }

  QJourney.create journey
  QJourneyLeg.create journeyLeg
  QSearchRequest.updateJourneyLegInfo searchRequest.id (Just journeySearchData)
  pure journeyGuid

updateJourneyWithQuote :: (CacheFlow m r, EsqDBFlow m r) => Id DJ.Journey -> DQuote.Quote -> DSReq.SearchRequest -> Bool -> m ()
updateJourneyWithQuote journeyId quote searchRequest isScheduled = do
  case isScheduled of
    True -> QJourney.updateStatus DJ.CONFIRMED journeyId
    False -> QJourney.updateStatus DJ.INPROGRESS journeyId
  let estimatedDistance = fromMaybe (Distance 0 Meter) searchRequest.distance
      estimatedDuration = fromMaybe (Seconds 0) searchRequest.estimatedRideDuration
      searchRequestId = searchRequest.id
  QJourneyExtra.updateEstimatedDistanceAndDuration journeyId estimatedDistance estimatedDuration
  let journeySearchData =
        JLT.JourneySearchData
          { journeyId = journeyId.getId,
            journeyLegOrder = 0,
            agency = Nothing,
            skipBooking = False,
            convenienceCost = 0,
            pricingId = Just quote.id.getId,
            onSearchFailed = Nothing,
            isDeleted = Nothing
          }
  QSearchRequest.updateJourneyLegInfo searchRequestId (Just journeySearchData)
