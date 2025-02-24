{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Booking.API where

-- TODO:Move api entity of booking to UI

import API.Types.RiderPlatform.Management.Endpoints.FRFSTicket (FRFSStationAPI)
import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
import qualified Domain.Action.UI.FareBreakup as DAFareBreakup
import qualified Domain.Action.UI.Location as SLoc
import Domain.Types
import Domain.Types.Booking
import Domain.Types.BookingCancellationReason
import qualified Domain.Types.BppDetails as DBppDetails
import Domain.Types.CancellationReason
import qualified Domain.Types.Exophone as DExophone
import Domain.Types.Extra.Ride (RideAPIEntity (..))
import Domain.Types.FareBreakup as DFareBreakup
import Domain.Types.Journey (Journey, JourneyStatus)
import Domain.Types.Location (Location, LocationAPIEntity)
import Domain.Types.ParcelDetails as DParcel
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.ServiceTierType as DVST
import Domain.Types.Sos as DSos
import qualified Domain.Types.StopInformation as DSI
import qualified Domain.Types.Trip as Trip
import Domain.Types.VehicleVariant (VehicleVariant (..))
import EulerHS.Prelude hiding (elem, find, id, length, map, null)
import Kernel.Beam.Functions
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.BppDetails as CQBPP
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.Sos as CQSos
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.BookingPartiesLink as QBPL
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.QueriesExtra.RideLite as QRideLite
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.StopInformation as QSI
import Tools.Error
import qualified Tools.JSON as J
import qualified Tools.Schema as S
import qualified Tools.SharedRedisKeys as SharedRedisKeys

data JourneyLocation = Taxi LocationAPIEntity | Frfs FRFSStationAPI | Null
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype BookingListResV2 = BookingListResV2
  { list :: [BookingAPIEntityV2]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data BookingAPIEntityV2 = Ride BookingAPIEntity | MultiModalRide JourneyAPIEntity
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data JourneyAPIEntity = JourneyAPIEntity
  { id :: Id Journey,
    fare :: Price,
    fromLocation :: JourneyLocation,
    toLocation :: JourneyLocation,
    startTime :: Maybe UTCTime,
    createdAt :: UTCTime,
    status :: JourneyStatus
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data BookingAPIEntity = BookingAPIEntity
  { id :: Id Booking,
    status :: BookingStatus,
    agencyName :: Text,
    agencyNumber :: Maybe Text,
    estimatedFare :: Money,
    isBookingUpdated :: Bool,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    estimatedFareWithCurrency :: PriceAPIEntity,
    discountWithCurrency :: Maybe PriceAPIEntity,
    estimatedTotalFareWithCurrency :: PriceAPIEntity,
    fromLocation :: LocationAPIEntity,
    initialPickupLocation :: LocationAPIEntity,
    driversPreviousRideDropLocLat :: Maybe Double,
    driversPreviousRideDropLocLon :: Maybe Double,
    rideList :: [RideAPIEntity],
    hasNightIssue :: Bool,
    tripTerms :: [Text],
    estimatedFareBreakup :: [FareBreakupAPIEntity],
    fareBreakup :: [FareBreakupAPIEntity],
    bookingDetails :: BookingAPIDetails,
    tripCategory :: Maybe TripCategory,
    rideScheduledTime :: UTCTime,
    returnTime :: Maybe UTCTime,
    rideStartTime :: Maybe UTCTime,
    rideEndTime :: Maybe UTCTime,
    duration :: Maybe Seconds,
    estimatedDuration :: Maybe Seconds,
    estimatedDistance :: Maybe HighPrecMeters,
    estimatedDistanceWithUnit :: Maybe Distance,
    merchantExoPhone :: Text,
    specialLocationTag :: Maybe Text,
    specialLocationName :: Maybe Text,
    paymentMethodId :: Maybe Payment.PaymentMethodId,
    paymentUrl :: Maybe Text,
    hasDisability :: Maybe Bool,
    sosStatus :: Maybe DSos.SosStatus,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    isValueAddNP :: Bool,
    vehicleServiceTierType :: DVST.ServiceTierType,
    vehicleServiceTierSeatingCapacity :: Maybe Int,
    vehicleServiceTierAirConditioned :: Maybe Double,
    vehicleIconUrl :: Maybe Text,
    isAirConditioned :: Maybe Bool,
    serviceTierName :: Maybe Text,
    serviceTierShortDesc :: Maybe Text,
    isScheduled :: Bool,
    isAlreadyFav :: Maybe Bool,
    favCount :: Maybe Int,
    cancellationReason :: Maybe BookingCancellationReasonAPIEntity,
    estimatedEndTimeRange :: Maybe DRide.EstimatedEndTimeRange
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data BookingCancellationReasonAPIEntity = BookingCancellationReasonAPIEntity
  { additionalInfo :: Maybe Text,
    reasonCode :: Maybe CancellationReasonCode,
    reasonStage :: Maybe CancellationStage,
    source :: CancellationSource
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data BookingStatusAPIEntity = BookingStatusAPIEntity
  { id :: Id Booking,
    isBookingUpdated :: Bool,
    bookingStatus :: BookingStatus,
    rideStatus :: Maybe DRide.RideStatus,
    talkedWithDriver :: Bool,
    estimatedEndTimeRange :: Maybe DRide.EstimatedEndTimeRange,
    driverArrivalTime :: Maybe UTCTime,
    destinationReachedAt :: Maybe UTCTime,
    sosStatus :: Maybe DSos.SosStatus,
    driversPreviousRideDropLocLat :: Maybe Double,
    driversPreviousRideDropLocLon :: Maybe Double,
    stopInfo :: [DSI.StopInformation],
    batchConfig :: Maybe SharedRedisKeys.BatchConfig
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data FavouriteBookingAPIEntity = FavouriteBookingAPIEntity
  { id :: Id DRide.Ride,
    rideRating :: Maybe Int,
    fromLocation :: Location,
    toLocation :: Maybe Location,
    totalFare :: Maybe Money,
    startTime :: Maybe UTCTime,
    vehicleVariant :: Maybe VehicleVariant
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- do not change constructor names without changing fareProductConstructorModifier
data BookingAPIDetails
  = OneWayAPIDetails OneWayBookingAPIDetails
  | RentalAPIDetails RentalBookingAPIDetails
  | DriverOfferAPIDetails OneWayBookingAPIDetails
  | OneWaySpecialZoneAPIDetails OneWaySpecialZoneBookingAPIDetails
  | InterCityAPIDetails InterCityBookingAPIDetails
  | AmbulanceAPIDetails AmbulanceBookingAPIDetails
  | DeliveryAPIDetails DeliveryBookingAPIDetails
  deriving (Show, Generic)

instance ToJSON BookingAPIDetails where
  toJSON = genericToJSON J.fareProductOptions

instance FromJSON BookingAPIDetails where
  parseJSON = genericParseJSON J.fareProductOptions

instance ToSchema BookingAPIDetails where
  declareNamedSchema = genericDeclareNamedSchema S.fareProductSchemaOptions

data RentalBookingAPIDetails = RentalBookingAPIDetails
  { stopLocation :: Maybe LocationAPIEntity,
    otpCode :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data OneWayBookingAPIDetails = OneWayBookingAPIDetails
  { toLocation :: LocationAPIEntity,
    stops :: [LocationAPIEntity],
    estimatedDistance :: HighPrecMeters,
    estimatedDistanceWithUnit :: Distance,
    isUpgradedToCab :: Maybe Bool
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data InterCityBookingAPIDetails = InterCityBookingAPIDetails
  { toLocation :: LocationAPIEntity,
    stops :: [LocationAPIEntity],
    estimatedDistance :: HighPrecMeters,
    otpCode :: Maybe Text,
    estimatedDistanceWithUnit :: Distance
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data AmbulanceBookingAPIDetails = AmbulanceBookingAPIDetails
  { toLocation :: LocationAPIEntity,
    estimatedDistance :: HighPrecMeters,
    estimatedDistanceWithUnit :: Distance
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data OneWaySpecialZoneBookingAPIDetails = OneWaySpecialZoneBookingAPIDetails
  { toLocation :: LocationAPIEntity,
    stops :: [LocationAPIEntity],
    estimatedDistance :: HighPrecMeters,
    estimatedDistanceWithUnit :: Distance,
    otpCode :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data DeliveryBookingAPIDetails = DeliveryBookingAPIDetails
  { toLocation :: LocationAPIEntity,
    estimatedDistance :: HighPrecMeters,
    estimatedDistanceWithUnit :: Distance,
    senderDetails :: DeliveryPersonDetailsAPIEntity,
    receiverDetails :: DeliveryPersonDetailsAPIEntity,
    requestorPartyRoles :: [Trip.PartyRole],
    parcelType :: DParcel.ParcelType,
    parcelQuantity :: Maybe Int
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data DeliveryPersonDetailsAPIEntity = DeliveryPersonDetailsAPIEntity
  { name :: Text,
    phoneNumber :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

makeBookingAPIEntity ::
  (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) =>
  Id Person.Person ->
  Booking ->
  Maybe DRide.Ride ->
  [DRide.Ride] ->
  [FareBreakup] ->
  [FareBreakup] ->
  Maybe DExophone.Exophone ->
  Maybe Payment.PaymentMethodId ->
  Bool ->
  Maybe DSos.SosStatus ->
  DBppDetails.BppDetails ->
  Bool ->
  Bool ->
  Maybe BookingCancellationReasonAPIEntity ->
  m BookingAPIEntity
makeBookingAPIEntity requesterId booking activeRide allRides estimatedFareBreakups fareBreakups mbExophone paymentMethodId hasNightIssue mbSosStatus bppDetails isValueAddNP showPrevDropLocationLatLon mbCancellationReason = do
  bookingDetails <- mkBookingAPIDetails booking requesterId
  rides <- mapM buildRideAPIEntity allRides
  let providerNum = fromMaybe "+91" bppDetails.supportNumber
  return $
    BookingAPIEntity
      { id = booking.id,
        status = booking.status,
        agencyName = bppDetails.name,
        agencyNumber = Just providerNum,
        estimatedFare = booking.estimatedFare.amountInt,
        discount = booking.discount <&> (.amountInt),
        estimatedTotalFare = booking.estimatedTotalFare.amountInt,
        estimatedFareWithCurrency = mkPriceAPIEntity booking.estimatedFare,
        discountWithCurrency = mkPriceAPIEntity <$> booking.discount,
        estimatedTotalFareWithCurrency = mkPriceAPIEntity booking.estimatedTotalFare,
        fromLocation = SLoc.makeLocationAPIEntity booking.fromLocation,
        initialPickupLocation = SLoc.makeLocationAPIEntity booking.initialPickupLocation,
        rideList = rides,
        hasNightIssue = hasNightIssue,
        tripTerms = fromMaybe [] $ booking.tripTerms <&> (.descriptions),
        estimatedFareBreakup = DAFareBreakup.mkFareBreakupAPIEntity <$> estimatedFareBreakups,
        fareBreakup = DAFareBreakup.mkFareBreakupAPIEntity <$> fareBreakups,
        rideScheduledTime = booking.startTime,
        returnTime = booking.returnTime,
        bookingDetails,
        rideStartTime = activeRide >>= (.rideStartTime),
        rideEndTime = activeRide >>= (.rideEndTime),
        estimatedDistance = distanceToHighPrecMeters <$> booking.estimatedDistance,
        estimatedDistanceWithUnit = booking.estimatedDistance,
        estimatedDuration = booking.estimatedDuration,
        duration = getRideDuration activeRide,
        merchantExoPhone = maybe booking.primaryExophone (\exophone -> if not exophone.isPrimaryDown then exophone.primaryPhone else exophone.backupPhone) mbExophone,
        specialLocationTag = booking.specialLocationTag,
        specialLocationName = booking.specialLocationName,
        paymentMethodId = paymentMethodId,
        paymentUrl = booking.paymentUrl,
        createdAt = booking.createdAt,
        updatedAt = booking.updatedAt,
        hasDisability = (Just . isJust) booking.disabilityTag,
        sosStatus = mbSosStatus,
        isBookingUpdated = booking.isBookingUpdated,
        isValueAddNP,
        vehicleServiceTierType = booking.vehicleServiceTierType,
        vehicleServiceTierSeatingCapacity = booking.vehicleServiceTierSeatingCapacity,
        vehicleServiceTierAirConditioned = booking.vehicleServiceTierAirConditioned,
        isAirConditioned = booking.isAirConditioned,
        serviceTierName = booking.serviceTierName,
        serviceTierShortDesc = booking.serviceTierShortDesc,
        driversPreviousRideDropLocLat = if showPrevDropLocationLatLon then fmap (.lat) (activeRide >>= (.driversPreviousRideDropLoc)) else Nothing,
        driversPreviousRideDropLocLon = if showPrevDropLocationLatLon then fmap (.lon) (activeRide >>= (.driversPreviousRideDropLoc)) else Nothing,
        isScheduled = booking.isScheduled,
        cancellationReason = mbCancellationReason,
        isAlreadyFav = activeRide >>= (.isAlreadyFav),
        favCount = activeRide >>= (.favCount),
        tripCategory = booking.tripCategory,
        estimatedEndTimeRange = activeRide >>= (.estimatedEndTimeRange),
        vehicleIconUrl = fmap showBaseUrl booking.vehicleIconUrl
      }
  where
    getRideDuration :: Maybe DRide.Ride -> Maybe Seconds
    getRideDuration mbRide = do
      ride <- mbRide
      startTime <- ride.rideStartTime
      endTime <- ride.rideEndTime
      return $ nominalDiffTimeToSeconds $ diffUTCTime endTime startTime

mkBookingAPIDetails :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => Booking -> Id Person.Person -> m BookingAPIDetails
mkBookingAPIDetails booking requesterId = case booking.bookingDetails of
  OneWayDetails details -> return $ OneWayAPIDetails . mkOneWayAPIDetails $ details
  RentalDetails details -> return $ RentalAPIDetails . mkRentalAPIDetails $ details
  DriverOfferDetails details -> return $ DriverOfferAPIDetails . mkOneWayAPIDetails $ details
  OneWaySpecialZoneDetails details -> return $ OneWaySpecialZoneAPIDetails . mkOneWaySpecialZoneAPIDetails $ details
  InterCityDetails details -> return $ InterCityAPIDetails . mkInterCityAPIDetails $ details
  AmbulanceDetails details -> return $ AmbulanceAPIDetails . mkAmbulanceAPIDetails $ details
  DeliveryDetails details -> DeliveryAPIDetails <$> mkDeliveryAPIDetails details
  where
    mkOneWayAPIDetails OneWayBookingDetails {..} =
      OneWayBookingAPIDetails
        { toLocation = SLoc.makeLocationAPIEntity toLocation,
          estimatedDistance = distanceToHighPrecMeters distance,
          estimatedDistanceWithUnit = distance,
          isUpgradedToCab = isUpgradedToCab,
          stops = map SLoc.makeLocationAPIEntity stops
        }
    mkRentalAPIDetails RentalBookingDetails {..} =
      RentalBookingAPIDetails
        { stopLocation = SLoc.makeLocationAPIEntity <$> stopLocation,
          ..
        }
    mkOneWaySpecialZoneAPIDetails OneWaySpecialZoneBookingDetails {..} =
      OneWaySpecialZoneBookingAPIDetails
        { toLocation = SLoc.makeLocationAPIEntity toLocation,
          estimatedDistance = distanceToHighPrecMeters distance,
          estimatedDistanceWithUnit = distance,
          stops = map SLoc.makeLocationAPIEntity stops,
          ..
        }
    mkInterCityAPIDetails InterCityBookingDetails {..} =
      InterCityBookingAPIDetails
        { toLocation = SLoc.makeLocationAPIEntity toLocation,
          estimatedDistance = distanceToHighPrecMeters distance,
          estimatedDistanceWithUnit = distance,
          stops = map SLoc.makeLocationAPIEntity stops,
          ..
        }
    mkAmbulanceAPIDetails AmbulanceBookingDetails {..} =
      AmbulanceBookingAPIDetails
        { toLocation = SLoc.makeLocationAPIEntity toLocation,
          estimatedDistance = distanceToHighPrecMeters distance,
          estimatedDistanceWithUnit = distance
        }
    -- check later if sender info required --
    mkDeliveryAPIDetails :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => DeliveryBookingDetails -> m DeliveryBookingAPIDetails
    mkDeliveryAPIDetails (DeliveryBookingDetails {..}) = do
      allBookingParties <- QBPL.findAllByBookingId booking.id
      senderParty <- fromMaybeM (InternalError "No sender party found") $ find (\party -> party.partyType == Trip.DeliveryParty Trip.Sender) allBookingParties
      receiverParty <- fromMaybeM (InternalError "No receiver party found") $ find (\party -> party.partyType == Trip.DeliveryParty Trip.Receiver) allBookingParties
      senderPerson <- QP.findById senderParty.partyId >>= fromMaybeM (InternalError "No sender person found for delivery")
      receiverPerson <- QP.findById receiverParty.partyId >>= fromMaybeM (InternalError "No receiver person found for delivery")
      encSenderPhoneNumber <- fromMaybeM (InternalError "No sender phone number found") senderPerson.mobileNumber
      encReceiverPhoneNumber <- fromMaybeM (InternalError "No receiver phone number found") receiverPerson.mobileNumber
      decSenderPhoneNumber <- decrypt encSenderPhoneNumber
      decReceiverPhoneNumber <- decrypt encReceiverPhoneNumber
      -- accumulate all the party roles for this requestorId --
      let requestorPartyRoles =
            [Trip.Initiator | requesterId == booking.riderId]
              ++ [Trip.DeliveryRoleSender | requesterId == senderParty.partyId]
              ++ [Trip.DeliveryRoleReceiver | requesterId == receiverParty.partyId]
      return $
        DeliveryBookingAPIDetails
          { toLocation = SLoc.makeLocationAPIEntity toLocation,
            estimatedDistance = distanceToHighPrecMeters distance,
            estimatedDistanceWithUnit = distance,
            senderDetails = DeliveryPersonDetailsAPIEntity {name = senderParty.partyName, phoneNumber = decSenderPhoneNumber},
            receiverDetails = DeliveryPersonDetailsAPIEntity {name = receiverParty.partyName, phoneNumber = decReceiverPhoneNumber},
            ..
          }

makeFavouriteBookingAPIEntity :: DRide.Ride -> FavouriteBookingAPIEntity
makeFavouriteBookingAPIEntity ride = do
  FavouriteBookingAPIEntity
    { id = ride.id,
      rideRating = ride.rideRating,
      fromLocation = ride.fromLocation,
      toLocation = ride.toLocation,
      totalFare = (.amountInt) <$> ride.totalFare,
      startTime = ride.rideStartTime,
      vehicleVariant = Just ride.vehicleVariant
    }

getActiveSos :: (CacheFlow m r, EsqDBFlow m r) => Maybe DRide.Ride -> Id Person.Person -> m (Maybe DSos.SosStatus)
getActiveSos mbRide personId = do
  case mbRide of
    Nothing -> return Nothing
    Just ride -> do
      sosDetails <- CQSos.findByRideId ride.id
      case sosDetails of
        Nothing -> do
          mockSos :: Maybe DSos.SosMockDrill <- Redis.safeGet $ CQSos.mockSosKey personId
          return $ mockSos <&> (.status)
        Just sos -> return $ Just sos.status

getActiveSos' :: (CacheFlow m r, EsqDBFlow m r) => Maybe QRideLite.RideLite -> Id Person.Person -> m (Maybe DSos.SosStatus)
getActiveSos' mbRide personId = do
  case mbRide of
    Nothing -> return Nothing
    Just ride -> do
      sosDetails <- CQSos.findByRideId ride.id
      case sosDetails of
        Nothing -> do
          mockSos :: Maybe DSos.SosMockDrill <- Redis.safeGet $ CQSos.mockSosKey personId
          return $ mockSos <&> (.status)
        Just sos -> return $ Just sos.status

buildBookingAPIEntity :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => Booking -> Id Person.Person -> m BookingAPIEntity
buildBookingAPIEntity booking personId = do
  mbActiveRide <- runInReplica $ QRide.findActiveByRBId booking.id
  mbRide <- runInReplica $ QRide.findByRBId booking.id
  -- nightIssue <- runInReplica $ QIssue.findNightIssueByBookingId booking.id
  (fareBreakups, estimatedFareBreakups) <- do
    case mbRide of
      Just ride ->
        case booking.status of
          COMPLETED -> do
            updatedFareBreakups <- runInReplica $ QFareBreakup.findAllByEntityIdAndEntityType ride.id.getId DFareBreakup.RIDE
            estimatedFareBreakups <- runInReplica $ QFareBreakup.findAllByEntityIdAndEntityType booking.id.getId DFareBreakup.BOOKING
            let fareBreakups = if null updatedFareBreakups then estimatedFareBreakups else updatedFareBreakups
            return (fareBreakups, estimatedFareBreakups)
          _ -> do
            --------- Need to remove it after fixing the status api polling in frontend ---------
            estimatedFareBreakups <- runInReplica $ QFareBreakup.findAllByEntityIdAndEntityTypeInKV booking.id.getId DFareBreakup.BOOKING
            return ([], estimatedFareBreakups)
      Nothing -> do
        estimatedFareBreakups <- runInReplica $ QFareBreakup.findAllByEntityIdAndEntityTypeInKV booking.id.getId DFareBreakup.BOOKING
        return ([], estimatedFareBreakups)
  mbExoPhone <- CQExophone.findByPrimaryPhone booking.primaryExophone
  bppDetails <- CQBPP.findBySubscriberIdAndDomain booking.providerId Context.MOBILITY >>= fromMaybeM (InternalError $ "BppDetails not found for providerId:-" <> booking.providerId <> "and domain:-" <> show Context.MOBILITY)
  mbSosStatus <- getActiveSos mbActiveRide personId
  isValueAddNP <- CQVAN.isValueAddNP booking.providerId
  let showPrevDropLocationLatLon = maybe False (.showDriversPreviousRideDropLoc) mbRide
  mbCancellationReason <-
    if booking.status == CANCELLED
      then QBCR.findByRideBookingId booking.id
      else return Nothing
  makeBookingAPIEntity personId booking mbActiveRide (maybeToList mbRide) estimatedFareBreakups fareBreakups mbExoPhone booking.paymentMethodId False mbSosStatus bppDetails isValueAddNP showPrevDropLocationLatLon (makeCancellationReasonAPIEntity <$> mbCancellationReason)
  where
    makeCancellationReasonAPIEntity :: BookingCancellationReason -> BookingCancellationReasonAPIEntity
    makeCancellationReasonAPIEntity BookingCancellationReason {..} = BookingCancellationReasonAPIEntity {..}

--Note :- if you are adding and extra field in BookingStatusAPIEntity then add it in BookingAPIEntity as well
buildBookingStatusAPIEntity :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Booking -> m BookingStatusAPIEntity
buildBookingStatusAPIEntity booking = do
  mbActiveRide <- runInReplica $ QRideLite.findActiveByRBIdLite booking.id
  batchConfig <- maybe (SharedRedisKeys.getBatchConfig booking.transactionId) (\_ -> pure Nothing) mbActiveRide
  stopsInfo <- if (fromMaybe False booking.hasStops) then maybe (pure []) (\ride -> QSI.findAllByRideId ride.id) mbActiveRide else return []
  let showPrevDropLocationLatLon = maybe False (.showDriversPreviousRideDropLoc) mbActiveRide
      driversPreviousRideDropLocLat = if showPrevDropLocationLatLon then fmap (.lat) (mbActiveRide >>= (.driversPreviousRideDropLoc)) else Nothing
      driversPreviousRideDropLocLon = if showPrevDropLocationLatLon then fmap (.lon) (mbActiveRide >>= (.driversPreviousRideDropLoc)) else Nothing
      rideStatus = fmap (.status) mbActiveRide
      estimatedEndTimeRange = mbActiveRide >>= (.estimatedEndTimeRange)
      driverArrivalTime = mbActiveRide >>= (.driverArrivalTime)
      destinationReachedTime = mbActiveRide >>= (.destinationReachedAt)
      talkedWithDriver = fromMaybe False (mbActiveRide >>= (.talkedWithDriver))
  sosStatus <- getActiveSos' mbActiveRide booking.riderId
  return $ BookingStatusAPIEntity booking.id booking.isBookingUpdated booking.status rideStatus talkedWithDriver estimatedEndTimeRange driverArrivalTime destinationReachedTime sosStatus driversPreviousRideDropLocLat driversPreviousRideDropLocLon stopsInfo batchConfig

favouritebuildBookingAPIEntity :: DRide.Ride -> FavouriteBookingAPIEntity
favouritebuildBookingAPIEntity ride = makeFavouriteBookingAPIEntity ride

-- TODO move to Domain.Types.Ride.Extra
buildRideAPIEntity :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => DRide.Ride -> m RideAPIEntity
buildRideAPIEntity DRide.Ride {..} = do
  stopsInfo <- if (fromMaybe False hasStops) then QSI.findAllByRideId id else return []
  let oneYearAgo = - (365 * 24 * 60 * 60)
      driverRegisteredAt' = fromMaybe (addUTCTime oneYearAgo createdAt) driverRegisteredAt
      driverRating' = driverRating <|> Just (toCentesimal 500) -- TODO::remove this default value
      vehicleColor' = fromMaybe "NA" vehicleColor -- TODO::remove this default value
  return $
    RideAPIEntity
      { shortRideId = shortId,
        driverNumber = Just driverMobileNumber,
        driverRatings = driverRating',
        driverRegisteredAt = Just driverRegisteredAt',
        rideOtp = otp,
        computedPrice = totalFare <&> (.amountInt),
        computedPriceWithCurrency = mkPriceAPIEntity <$> totalFare,
        chargeableRideDistance = distanceToHighPrecMeters <$> chargeableDistance,
        chargeableRideDistanceWithUnit = chargeableDistance,
        traveledRideDistance = traveledDistance,
        vehicleColor = vehicleColor',
        allowedEditLocationAttempts = fromMaybe 0 allowedEditLocationAttempts,
        allowedEditPickupLocationAttempts = fromMaybe 0 allowedEditPickupLocationAttempts,
        talkedWithDriver = fromMaybe False talkedWithDriver,
        ..
      }
