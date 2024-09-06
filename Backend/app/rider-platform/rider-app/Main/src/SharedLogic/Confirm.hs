{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Confirm where

import Data.Foldable.Extra (anyM)
import qualified Data.HashMap.Strict as HM
import qualified Domain.Action.UI.Estimate as UEstimate
import qualified Domain.Action.UI.Quote as DQuote
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingPartiesLink as DBPL
import Domain.Types.CancellationReason
import qualified Domain.Types.DeliveryDetails as DTDD
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.Location as DL
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PersonFlowStatus as DPFS
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.SearchRequest as DSReq
import qualified Domain.Types.Trip as Trip
import qualified Domain.Types.VehicleVariant as DV
import Kernel.External.Encryption (decrypt)
import Kernel.External.Maps.Types
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Randomizer (getRandomElement)
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CMSUC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.Booking as QRideB
import qualified Storage.Queries.BookingPartiesLink as QBPL
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.SearchRequest as QSReq
import qualified Storage.Queries.SearchRequestPartiesLink as QSRPL
import qualified Storage.Queries.Transformers.Booking as QTB
import Tools.Error
import Tools.Event
import qualified Tools.Maps as Maps
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
    confirmResDetails :: Maybe DConfirmResDetails
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
    EncFlow m r
  ) =>
  DConfirmReq ->
  m DConfirmRes
confirm DConfirmReq {..} = do
  now <- getCurrentTime
  when (quote.validTill < now) $ throwError (InvalidRequest $ "Quote expired " <> show quote.id) -- init validation check
  bppQuoteId <- getBppQuoteId now quote.quoteDetails
  searchRequest <- QSReq.findById quote.requestId >>= fromMaybeM (SearchRequestNotFound quote.requestId.getId)
  merchant <- CQM.findById searchRequest.merchantId >>= fromMaybeM (MerchantNotFound searchRequest.merchantId.getId)
  when merchant.onlinePayment $ do
    when (isNothing paymentMethodId) $ throwError PaymentMethodRequired
    QPerson.updateDefaultPaymentMethodId paymentMethodId personId -- Make payment method as default payment method for customer
  activeBooking <- QRideB.findLatestSelfAndPartyBookingByRiderId personId --This query also checks for booking parties
  scheduledBookings <- QRideB.findByRiderIdAndStatus personId [DRB.CONFIRMED]
  let searchDist = round $ fromMaybe 0 $ distanceToHighPrecMeters <$> searchRequest.distance
      searchDur = fromMaybe 0 $ (.getSeconds) <$> searchRequest.estimatedRideDuration
  overlap <- anyM (checkOverlap searchDist searchDur searchRequest.startTime searchRequest.toLocation merchant) scheduledBookings
  case (activeBooking, overlap) of
    (_, True) -> throwError $ InvalidRequest "ACTIVE_BOOKING_PRESENT"
    (Just booking, _) -> DQuote.processActiveBooking booking OnConfirm
    _ -> pure ()
  when (searchRequest.validTill < now) $
    throwError SearchRequestExpired
  unless (searchRequest.riderId == personId) $ throwError AccessDenied
  let fromLocation = searchRequest.fromLocation
      mbToLocation = searchRequest.toLocation
  let merchantOperatingCityId = searchRequest.merchantOperatingCityId
  city <- CQMOC.findById merchantOperatingCityId >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound merchantOperatingCityId.getId)
  exophone <- findRandomExophone merchantOperatingCityId
  let isScheduled = merchant.scheduleRideBufferTime `addUTCTime` now < searchRequest.startTime
  (booking, bookingParties) <- buildBooking searchRequest bppQuoteId quote fromLocation mbToLocation exophone now Nothing paymentMethodId isScheduled
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
  void $ QPFS.updateStatus searchRequest.riderId DPFS.WAITING_FOR_DRIVER_ASSIGNMENT {bookingId = booking.id, validTill = searchRequest.validTill, fareProductType = Just (QTB.getFareProductType booking.bookingDetails), tripCategory = booking.tripCategory}
  void $ QEstimate.updateStatusByRequestId DEstimate.COMPLETED quote.requestId
  confirmResDetails <- case quote.tripCategory of
    Just (Trip.Delivery _) -> Just <$> makeDeliveryDetails booking bookingParties
    _ -> return Nothing
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
        ..
      }
  where
    prependZero :: Text -> Text
    prependZero str = "0" <> str

    getBppQuoteId now = \case
      DQuote.OneWayDetails _ -> throwError $ InternalError "FulfillmentId/BPPQuoteId not found in Confirm. This is not possible."
      DQuote.AmbulanceDetails driverOffer -> getBppQuoteIdFromDriverOffer driverOffer now
      DQuote.DeliveryDetails driverOffer -> getBppQuoteIdFromDriverOffer driverOffer now
      DQuote.RentalDetails rentalDetails -> pure rentalDetails.id.getId
      DQuote.DriverOfferDetails driverOffer -> getBppQuoteIdFromDriverOffer driverOffer now
      DQuote.OneWaySpecialZoneDetails details -> pure details.quoteId
      DQuote.InterCityDetails details -> pure details.id.getId
      DQuote.OneWayScheduledDetails details -> pure details.quoteId

    getBppQuoteIdFromDriverOffer driverOffer now = do
      estimate <- QEstimate.findById driverOffer.estimateId >>= fromMaybeM EstimateNotFound
      when (UEstimate.isCancelled estimate.status) $ throwError $ EstimateCancelled estimate.id.getId
      when (driverOffer.validTill < now) $ throwError $ QuoteExpired quote.id.getId
      pure driverOffer.bppQuoteId

    checkOverlap :: (ServiceFlow m r) => Int -> Int -> UTCTime -> Maybe DL.Location -> DM.Merchant -> DRB.Booking -> m Bool
    checkOverlap estimatedDistance estimatedDuration curBookingStartTime currBookingToLocation merchant booking = do
      destToScheduledPickup <- calculateDistanceToScheduledPickup currBookingToLocation booking
      let estimatedDistanceInKm' = destToScheduledPickup + estimatedDistance
          estimatedDistanceInKm = estimatedDistanceInKm' `div` 1000
          estRideEndTimeByDuration = addUTCTime (intToNominalDiffTime estimatedDuration + merchant.scheduleRideBufferTime) curBookingStartTime
          estRideEndTimeByDist = addUTCTime (intToNominalDiffTime (estimatedDistanceInKm * 3 * 60) + merchant.scheduleRideBufferTime) curBookingStartTime -- TODO: need to make avg speed at rider side configurable : current 3min/km
      return $ max estRideEndTimeByDuration estRideEndTimeByDist >= booking.startTime

    calculateDistanceToScheduledPickup :: (ServiceFlow m r) => Maybe DL.Location -> DRB.Booking -> m Int
    calculateDistanceToScheduledPickup currBookingToLocation booking =
      case currBookingToLocation of
        Nothing -> return 0
        Just loc -> do
          let currBookingDest = LatLong {lat = loc.lat, lon = loc.lon}
          let scheduledPickup = LatLong {lat = booking.fromLocation.lat, lon = booking.fromLocation.lon}
          distance <- do
            Maps.getDistanceForScheduledRides booking.merchantId booking.merchantOperatingCityId $
              Maps.GetDistanceReq
                { origin = currBookingDest,
                  destination = scheduledPickup,
                  travelMode = Just Maps.CAR,
                  sourceDestinationMapping = Nothing,
                  distanceUnit = Meter
                }
          return $ distance.distance.getMeters

    makeDeliveryDetails :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r, EncFlow m r) => DRB.Booking -> [DBPL.BookingPartiesLink] -> m DConfirmResDetails
    makeDeliveryDetails booking bookingParties = do
      senderParty <- fromMaybeM (InternalError "SenderParty not found") $ find (\party -> party.partyType == (Trip.DeliveryParty Trip.Sender)) bookingParties
      receiverParty <- fromMaybeM (InternalError "ReceiverParty not found") $ find (\party -> party.partyType == (Trip.DeliveryParty Trip.Receiver)) bookingParties
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
              DTDD.initiatedAs = initiatedBy
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
  m (DRB.Booking, [DBPL.BookingPartiesLink])
buildBooking searchRequest bppQuoteId quote fromLoc mbToLoc exophone now otpCode paymentMethodId isScheduled = do
  id <- generateGUID
  bookingDetails <- buildBookingDetails
  bookingParties <- buildPartiesLinks id
  deploymentVersion <- asks (.version)
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
          initiatedBy = searchRequest.initiatedBy
        },
      bookingParties
    )
  where
    buildPartiesLinks bookingId = case quote.quoteDetails of
      DQuote.DeliveryDetails _ -> makeDeliveryParties bookingId
      _ -> pure []
    buildBookingDetails = case quote.quoteDetails of
      DQuote.OneWayDetails _ -> DRB.OneWayDetails <$> buildOneWayDetails
      DQuote.AmbulanceDetails _ -> DRB.AmbulanceDetails <$> buildAmbulanceDetails
      DQuote.DeliveryDetails _ -> DRB.DeliveryDetails <$> buildDeliveryDetails
      DQuote.RentalDetails _ -> pure $ DRB.RentalDetails (DRB.RentalBookingDetails {stopLocation = mbToLoc, ..})
      DQuote.DriverOfferDetails _ -> DRB.DriverOfferDetails <$> buildOneWayDetails
      DQuote.OneWaySpecialZoneDetails _ -> DRB.OneWaySpecialZoneDetails <$> buildOneWaySpecialZoneDetails
      DQuote.InterCityDetails _ -> DRB.InterCityDetails <$> buildInterCityDetails
      DQuote.OneWayScheduledDetails _ -> DRB.OneWayScheduledDetails <$> buildOneWayScheduledDetails

    buildInterCityDetails = do
      -- we need to throw errors here because of some redundancy of our domain model
      toLocation <- mbToLoc & fromMaybeM (InternalError "toLocation is null for one way search request")
      distance <- searchRequest.distance & fromMaybeM (InternalError "distance is null for one way search request")
      pure DRB.InterCityBookingDetails {..}
    buildOneWayDetails = do
      -- we need to throw errors here because of some redundancy of our domain model
      toLocation <- mbToLoc & fromMaybeM (InternalError "toLocation is null for one way search request")
      distance <- searchRequest.distance & fromMaybeM (InternalError "distance is null for one way search request")
      pure DRB.OneWayBookingDetails {..}
    buildAmbulanceDetails = do
      -- we need to throw errors here because of some redundancy of our domain model
      toLocation <- mbToLoc & fromMaybeM (InternalError "toLocation is null for one way search request")
      distance <- searchRequest.distance & fromMaybeM (InternalError "distance is null for one way search request")
      pure DRB.AmbulanceBookingDetails {..}
    buildOneWaySpecialZoneDetails = do
      -- we need to throw errors here because of some redundancy of our domain model
      toLocation <- mbToLoc & fromMaybeM (InternalError "toLocation is null for one way search request")
      distance <- searchRequest.distance & fromMaybeM (InternalError "distance is null for one way search request")
      pure DRB.OneWaySpecialZoneBookingDetails {..}
    buildDeliveryDetails = do
      toLocation <- mbToLoc & fromMaybeM (InternalError "toLocation is null for one way search request")
      distance <- searchRequest.distance & fromMaybeM (InternalError "distance is null for one way search request")
      pure DRB.DeliveryBookingDetails {..}
    buildOneWayScheduledDetails = do
      -- we need to throw errors here because of some redundancy of our domain model
      toLocation <- mbToLoc & fromMaybeM (InternalError "toLocation is null for one way search request")
      distance <- searchRequest.distance & fromMaybeM (InternalError "distance is null for one way search request")
      pure DRB.OneWayScheduledBookingDetails {..}
    makeDeliveryParties :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Text -> m [DBPL.BookingPartiesLink]
    makeDeliveryParties bookingId = do
      allSearchReqParties <- QSRPL.findAllBySearchRequestId searchRequest.id
      when ((length allSearchReqParties) < 2) $ throwError $ InternalError "No parties found for search request delivery"
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

findRandomExophone :: (CacheFlow m r, EsqDBFlow m r) => Id DMOC.MerchantOperatingCity -> m DExophone.Exophone
findRandomExophone merchantOperatingCityId = do
  merchantServiceUsageConfig <- CMSUC.findByMerchantOperatingCityId merchantOperatingCityId >>= fromMaybeM (MerchantServiceUsageConfigNotFound $ "merchantOperatingCityId:- " <> merchantOperatingCityId.getId)
  exophones <- CQExophone.findByMerchantOperatingCityIdAndService merchantOperatingCityId merchantServiceUsageConfig.getExophone
  nonEmptyExophones <- case exophones of
    [] -> throwError $ ExophoneNotFound merchantOperatingCityId.getId
    e : es -> pure $ e :| es
  getRandomElement nonEmptyExophones
