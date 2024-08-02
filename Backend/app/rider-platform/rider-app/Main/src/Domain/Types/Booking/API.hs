{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Booking.API where

import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
-- TODO:Move api entity of booking to UI

import qualified Domain.Action.UI.FareBreakup as DAFareBreakup
import qualified Domain.Action.UI.Location as SLoc
import Domain.Types.Booking
import Domain.Types.BookingCancellationReason
import qualified Domain.Types.BppDetails as DBppDetails
import Domain.Types.CancellationReason
import qualified Domain.Types.Exophone as DExophone
import Domain.Types.Extra.Ride (RideAPIEntity (..))
import Domain.Types.FareBreakup as DFareBreakup
import Domain.Types.Location (Location, LocationAPIEntity)
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as DRide
import Domain.Types.Sos as DSos
import qualified Domain.Types.VehicleServiceTier as DVST
import EulerHS.Prelude hiding (elem, id, length, null)
import Kernel.Beam.Functions
import Kernel.External.Encryption
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
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.JSON as J
import qualified Tools.Schema as S

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
    rideScheduledTime :: UTCTime,
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
    vehicleServiceTierType :: DVST.VehicleServiceTierType,
    vehicleServiceTierSeatingCapacity :: Maybe Int,
    vehicleServiceTierAirConditioned :: Maybe Double,
    isAirConditioned :: Maybe Bool,
    serviceTierName :: Maybe Text,
    serviceTierShortDesc :: Maybe Text,
    isScheduled :: Bool,
    isAlreadyFav :: Maybe Bool,
    favCount :: Maybe Int,
    cancellationReason :: Maybe BookingCancellationReasonAPIEntity
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
    rideStatus :: Maybe DRide.RideStatus
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data FavouriteBookingAPIEntity = FavouriteBookingAPIEntity
  { id :: Id DRide.Ride,
    rideRating :: Maybe Int,
    fromLocation :: Location,
    toLocation :: Maybe Location,
    totalFare :: Maybe Money,
    startTime :: Maybe UTCTime
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
    estimatedDistance :: HighPrecMeters,
    estimatedDistanceWithUnit :: Distance
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data InterCityBookingAPIDetails = InterCityBookingAPIDetails
  { toLocation :: LocationAPIEntity,
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
    estimatedDistance :: HighPrecMeters,
    estimatedDistanceWithUnit :: Distance,
    otpCode :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

makeBookingAPIEntity ::
  EncFlow m r =>
  Booking ->
  Maybe DRide.Ride ->
  [DRide.Ride] ->
  [FareBreakup] ->
  [FareBreakup] ->
  Maybe DExophone.Exophone ->
  Maybe Payment.PaymentMethodId ->
  Maybe Bool ->
  Bool ->
  Maybe DSos.SosStatus ->
  DBppDetails.BppDetails ->
  Bool ->
  Bool ->
  Maybe BookingCancellationReasonAPIEntity ->
  m BookingAPIEntity
makeBookingAPIEntity booking activeRide allRides estimatedFareBreakups fareBreakups mbExophone paymentMethodId hasDisability hasNightIssue mbSosStatus bppDetails isValueAddNP showPrevDropLocationLatLon mbCancellationReason = do
  let bookingDetails = mkBookingAPIDetails booking.bookingDetails
      providerNum = fromMaybe "+91" bppDetails.supportNumber
  allRides' <- Kernel.Prelude.mapM makeRideAPIEntity allRides
  pure
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
        rideList = allRides',
        hasNightIssue = hasNightIssue,
        tripTerms = fromMaybe [] $ booking.tripTerms <&> (.descriptions),
        estimatedFareBreakup = DAFareBreakup.mkFareBreakupAPIEntity <$> estimatedFareBreakups,
        fareBreakup = DAFareBreakup.mkFareBreakupAPIEntity <$> fareBreakups,
        rideScheduledTime = booking.startTime,
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
        hasDisability = hasDisability,
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
        favCount = activeRide >>= (.favCount)
      }
  where
    getRideDuration :: Maybe DRide.Ride -> Maybe Seconds
    getRideDuration mbRide = do
      ride <- mbRide
      startTime <- ride.rideStartTime
      endTime <- ride.rideEndTime
      return $ nominalDiffTimeToSeconds $ diffUTCTime endTime startTime

mkBookingAPIDetails :: BookingDetails -> BookingAPIDetails
mkBookingAPIDetails = \case
  OneWayDetails details -> OneWayAPIDetails . mkOneWayAPIDetails $ details
  RentalDetails details -> RentalAPIDetails . mkRentalAPIDetails $ details
  DriverOfferDetails details -> DriverOfferAPIDetails . mkOneWayAPIDetails $ details
  OneWaySpecialZoneDetails details -> OneWaySpecialZoneAPIDetails . mkOneWaySpecialZoneAPIDetails $ details
  InterCityDetails details -> InterCityAPIDetails . mkInterCityAPIDetails $ details
  AmbulanceDetails details -> AmbulanceAPIDetails . mkAmbulanceAPIDetails $ details
  where
    mkOneWayAPIDetails OneWayBookingDetails {..} =
      OneWayBookingAPIDetails
        { toLocation = SLoc.makeLocationAPIEntity toLocation,
          estimatedDistance = distanceToHighPrecMeters distance,
          estimatedDistanceWithUnit = distance
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
          ..
        }
    mkInterCityAPIDetails InterCityBookingDetails {..} =
      InterCityBookingAPIDetails
        { toLocation = SLoc.makeLocationAPIEntity toLocation,
          estimatedDistance = distanceToHighPrecMeters distance,
          estimatedDistanceWithUnit = distance,
          ..
        }
    mkAmbulanceAPIDetails AmbulanceBookingDetails {..} =
      AmbulanceBookingAPIDetails
        { toLocation = SLoc.makeLocationAPIEntity toLocation,
          estimatedDistance = distanceToHighPrecMeters distance,
          estimatedDistanceWithUnit = distance
        }

makeFavouriteBookingAPIEntity :: DRide.Ride -> FavouriteBookingAPIEntity
makeFavouriteBookingAPIEntity ride = do
  FavouriteBookingAPIEntity
    { id = ride.id,
      rideRating = ride.rideRating,
      fromLocation = ride.fromLocation,
      toLocation = ride.toLocation,
      totalFare = (.amountInt) <$> ride.totalFare,
      startTime = ride.rideStartTime
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
      Nothing -> return ([], [])
  mbExoPhone <- CQExophone.findByPrimaryPhone booking.primaryExophone
  bppDetails <- CQBPP.findBySubscriberIdAndDomain booking.providerId Context.MOBILITY >>= fromMaybeM (InternalError $ "BppDetails not found for providerId:-" <> booking.providerId <> "and domain:-" <> show Context.MOBILITY)
  mbSosStatus <- getActiveSos mbActiveRide personId
  person <- runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  isValueAddNP <- CQVAN.isValueAddNP booking.providerId
  let showPrevDropLocationLatLon = maybe False (.showDriversPreviousRideDropLoc) mbRide
  mbCancellationReason <-
    if booking.status == CANCELLED
      then QBCR.findByRideBookingId booking.id
      else return Nothing
  makeBookingAPIEntity booking mbActiveRide (maybeToList mbRide) estimatedFareBreakups fareBreakups mbExoPhone booking.paymentMethodId person.hasDisability False mbSosStatus bppDetails isValueAddNP showPrevDropLocationLatLon (makeCancellationReasonAPIEntity <$> mbCancellationReason)
  where
    makeCancellationReasonAPIEntity :: BookingCancellationReason -> BookingCancellationReasonAPIEntity
    makeCancellationReasonAPIEntity BookingCancellationReason {..} = BookingCancellationReasonAPIEntity {..}

buildBookingStatusAPIEntity :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Booking -> m BookingStatusAPIEntity
buildBookingStatusAPIEntity booking = do
  mbActiveRide <- runInReplica $ QRide.findActiveByRBId booking.id
  rideStatus <- maybe (pure Nothing) (\ride -> pure $ Just ride.status) mbActiveRide
  return $ BookingStatusAPIEntity booking.id booking.isBookingUpdated booking.status rideStatus

favouritebuildBookingAPIEntity :: DRide.Ride -> FavouriteBookingAPIEntity
favouritebuildBookingAPIEntity ride = makeFavouriteBookingAPIEntity ride

-- TODO move to Domain.Types.Ride.Extra
makeRideAPIEntity :: EncFlow m r => DRide.Ride -> m RideAPIEntity
makeRideAPIEntity DRide.Ride {..} = do
  decryptedDriverNumber <- mapM decrypt driverPhoneNumber
  let driverMobileNumber' = if status `elem` [DRide.NEW, DRide.INPROGRESS] then decryptedDriverNumber else Just "xxxx"
      oneYearAgo = - (365 * 24 * 60 * 60)
      driverRegisteredAt' = fromMaybe (addUTCTime oneYearAgo createdAt) driverRegisteredAt
      driverRating' = driverRating <|> Just (toCentesimal 500) -- TODO::remove this default value
      vehicleColor' = fromMaybe "NA" vehicleColor -- TODO::remove this default value
  pure
    RideAPIEntity
      { shortRideId = shortId,
        driverNumber = driverMobileNumber',
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
        ..
      }
