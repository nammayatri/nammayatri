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

import qualified Domain.Action.UI.FareBreakup as DFareBreakup
import qualified Domain.Action.UI.Location as SLoc
import qualified Domain.Action.UI.MerchantPaymentMethod as DMPM
import Domain.Types.Booking
import qualified Domain.Types.BppDetails as DBppDetails
import qualified Domain.Types.Exophone as DExophone
import Domain.Types.FareBreakup
import Domain.Types.Location (LocationAPIEntity)
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as Person
import Domain.Types.Ride (Ride (..), RideAPIEntity (..))
import qualified Domain.Types.Ride as DRide
import Domain.Types.Sos as DSos
import qualified Domain.Types.VehicleServiceTier as DVST
import EulerHS.Prelude hiding (id, null)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.BppDetails as CQBPP
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Sos as CQSos
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
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
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    estimatedFareWithCurrency :: PriceAPIEntity,
    discountWithCurrency :: Maybe PriceAPIEntity,
    estimatedTotalFareWithCurrency :: PriceAPIEntity,
    fromLocation :: LocationAPIEntity,
    initialPickupLocation :: LocationAPIEntity,
    rideList :: [RideAPIEntity],
    hasNightIssue :: Bool,
    tripTerms :: [Text],
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
    paymentMethod :: Maybe DMPM.PaymentMethodAPIEntity,
    paymentUrl :: Maybe Text,
    hasDisability :: Maybe Bool,
    sosStatus :: Maybe DSos.SosStatus,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    isValueAddNP :: Bool,
    editPickupAttemptsLeft :: Int,
    vehicleServiceTierType :: DVST.VehicleServiceTierType,
    serviceTierName :: Maybe Text,
    serviceTierShortDesc :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- do not change constructor names without changing fareProductConstructorModifier
data BookingAPIDetails
  = OneWayAPIDetails OneWayBookingAPIDetails
  | RentalAPIDetails RentalBookingAPIDetails
  | DriverOfferAPIDetails OneWayBookingAPIDetails
  | OneWaySpecialZoneAPIDetails OneWaySpecialZoneBookingAPIDetails
  | InterCityAPIDetails InterCityBookingAPIDetails
  deriving (Show, Generic)

instance ToJSON BookingAPIDetails where
  toJSON = genericToJSON J.fareProductOptions

instance FromJSON BookingAPIDetails where
  parseJSON = genericParseJSON J.fareProductOptions

instance ToSchema BookingAPIDetails where
  declareNamedSchema = genericDeclareNamedSchema S.fareProductSchemaOptions

newtype RentalBookingAPIDetails = RentalBookingAPIDetails
  { stopLocation :: Maybe LocationAPIEntity
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
  Booking ->
  Maybe Ride ->
  [Ride] ->
  [FareBreakup] ->
  Maybe DExophone.Exophone ->
  Maybe DMPM.MerchantPaymentMethod ->
  Maybe Bool ->
  Bool ->
  Maybe DSos.SosStatus ->
  DBppDetails.BppDetails ->
  Bool ->
  BookingAPIEntity
makeBookingAPIEntity booking activeRide allRides fareBreakups mbExophone mbPaymentMethod hasDisability hasNightIssue mbSosStatus bppDetails isValueAddNP = do
  let bookingDetails = mkBookingAPIDetails booking.bookingDetails
      providerNum = fromMaybe "+91" bppDetails.supportNumber
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
      rideList = allRides <&> makeRideAPIEntity,
      hasNightIssue = hasNightIssue,
      tripTerms = fromMaybe [] $ booking.tripTerms <&> (.descriptions),
      fareBreakup = DFareBreakup.mkFareBreakupAPIEntity <$> fareBreakups,
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
      paymentMethod = DMPM.mkPaymentMethodAPIEntity <$> mbPaymentMethod,
      paymentUrl = booking.paymentUrl,
      createdAt = booking.createdAt,
      updatedAt = booking.updatedAt,
      hasDisability = hasDisability,
      sosStatus = mbSosStatus,
      editPickupAttemptsLeft = fromMaybe 0 (activeRide >>= (.allowedEditLocationAttempts)),
      isValueAddNP,
      vehicleServiceTierType = booking.vehicleServiceTierType,
      serviceTierName = booking.serviceTierName,
      serviceTierShortDesc = booking.serviceTierShortDesc
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
      where
        mkOneWayAPIDetails OneWayBookingDetails {..} =
          OneWayBookingAPIDetails
            { toLocation = SLoc.makeLocationAPIEntity toLocation,
              estimatedDistance = distanceToHighPrecMeters distance,
              estimatedDistanceWithUnit = distance
            }
        mkRentalAPIDetails RentalBookingDetails {..} =
          RentalBookingAPIDetails
            { stopLocation = SLoc.makeLocationAPIEntity <$> stopLocation
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
              estimatedDistanceWithUnit = distance
            }

getActiveSos :: KvDbFlow m r => Maybe DRide.Ride -> Id Person.Person -> m (Maybe DSos.SosStatus)
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

buildBookingAPIEntity :: (KvDbFlow m r, EsqDBReplicaFlow m r) => Booking -> Id Person.Person -> m BookingAPIEntity
buildBookingAPIEntity booking personId = do
  mbActiveRide <- runInReplica $ QRide.findActiveByRBId booking.id
  mbRide <- runInReplica $ QRide.findByRBId booking.id
  -- nightIssue <- runInReplica $ QIssue.findNightIssueByBookingId booking.id
  fareBreakups <- bool (pure []) (runInReplica $ QFareBreakup.findAllByBookingId booking.id) (booking.status == COMPLETED)
  mbExoPhone <- CQExophone.findByPrimaryPhone booking.primaryExophone
  bppDetails <- CQBPP.findBySubscriberIdAndDomain booking.providerId Context.MOBILITY >>= fromMaybeM (InternalError $ "BppDetails not found for providerId:-" <> booking.providerId <> "and domain:-" <> show Context.MOBILITY)
  let merchantOperatingCityId = booking.merchantOperatingCityId
  mbSosStatus <- getActiveSos mbActiveRide personId
  mbPaymentMethod <- forM booking.paymentMethodId $ \paymentMethodId -> do
    CQMPM.findByIdAndMerchantOperatingCityId paymentMethodId merchantOperatingCityId
      >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
  person <- runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  isValueAddNP <- CQVAN.isValueAddNP booking.providerId
  return $ makeBookingAPIEntity booking mbActiveRide (maybeToList mbRide) fareBreakups mbExoPhone mbPaymentMethod person.hasDisability False mbSosStatus bppDetails isValueAddNP

-- TODO move to Domain.Types.Ride.Extra
makeRideAPIEntity :: Ride -> RideAPIEntity
makeRideAPIEntity Ride {..} =
  let driverMobileNumber' = if status == DRide.NEW then Just driverMobileNumber else Just "xxxx"
      oneYearAgo = - (365 * 24 * 60 * 60)
      driverRegisteredAt' = fromMaybe (addUTCTime oneYearAgo createdAt) driverRegisteredAt
      driverRating' = driverRating <|> Just (toCentesimal 500) -- TODO::remove this default value
      vehicleColor' = fromMaybe "NA" vehicleColor -- TODO::remove this default value
   in RideAPIEntity
        { shortRideId = shortId,
          driverNumber = driverMobileNumber',
          driverRatings = driverRating',
          driverRegisteredAt = Just driverRegisteredAt',
          rideOtp = otp,
          computedPrice = totalFare <&> (.amountInt),
          computedPriceWithCurrency = mkPriceAPIEntity <$> totalFare,
          chargeableRideDistance = distanceToHighPrecMeters <$> chargeableDistance,
          chargeableRideDistanceWithUnit = chargeableDistance,
          vehicleColor = vehicleColor',
          ..
        }
