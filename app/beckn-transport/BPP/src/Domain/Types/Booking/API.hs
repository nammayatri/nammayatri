module Domain.Types.Booking.API where

import Beckn.External.Encryption
import Beckn.Prelude
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.OpenApi
import Domain.Types.Booking.BookingLocation
import qualified Domain.Types.Booking.BookingLocation as DLoc
import Domain.Types.Booking.Type
import Domain.Types.FarePolicy.FareBreakup
import qualified Domain.Types.FarePolicy.FareBreakup as DFareBreakup
import qualified Domain.Types.FarePolicy.RentalFarePolicy as DRentalFP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideDetails as RD
import Domain.Types.Vehicle
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.FarePolicy.RentalFarePolicy as QRentalFP
import qualified Storage.Queries.FarePolicy.FareBreakup as QFareBreakup
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.JSON as J
import qualified Tools.Schema as S

data BookingAPIEntity = BookingAPIEntity
  { id :: Id Booking,
    status :: BookingStatus,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    fromLocation :: DLoc.BookingLocationAPIEntity,
    rideList :: [DRide.RideAPIEntity],
    tripTerms :: [Text],
    fareBreakup :: [FareBreakupAPIEntity],
    riderName :: Maybe Text,
    bookingDetails :: BookingDetailsAPIEntity,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- do not change constructor names without changing fareProductConstructorModifier
data BookingDetailsAPIEntity = OneWayDetailsAPIEntity OneWayBookingDetailsAPIEntity | RentalDetailsAPIEntity RentalBookingDetailsAPIEntity
  deriving (Show, Generic)

instance ToJSON BookingDetailsAPIEntity where
  toJSON = genericToJSON J.fareProductOptions

instance FromJSON BookingDetailsAPIEntity where
  parseJSON = genericParseJSON J.fareProductOptions

instance ToSchema BookingDetailsAPIEntity where
  declareNamedSchema = genericDeclareNamedSchema S.fareProductSchemaOptions

newtype OneWayBookingDetailsAPIEntity = OneWayBookingDetailsAPIEntity
  { toLocation :: BookingLocationAPIEntity
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data RentalBookingDetailsAPIEntity = RentalBookingDetailsAPIEntity
  { baseDistance :: Kilometers,
    baseDuration :: Hours
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

buildBookingAPIEntity :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => Booking -> m BookingAPIEntity
buildBookingAPIEntity booking = do
  let rbStatus = booking.status
  rideAPIEntityList <- mapM buildRideAPIEntity =<< QRide.findAllRideAPIEntityDataByRBId booking.id
  fareBreakups <- QFareBreakup.findAllByBookingId booking.id
  (bookingDetails, tripTerms) <- buildBookingAPIDetails booking.bookingDetails
  return $
    BookingAPIEntity
      { id = booking.id,
        status = rbStatus,
        estimatedFare = booking.estimatedFare,
        discount = booking.discount,
        estimatedTotalFare = booking.estimatedTotalFare,
        fromLocation = DLoc.makeBookingLocationAPIEntity booking.fromLocation,
        rideList = rideAPIEntityList,
        fareBreakup = DFareBreakup.mkFareBreakupAPIEntity <$> fareBreakups,
        riderName = booking.riderName,
        bookingDetails,
        tripTerms,
        createdAt = booking.createdAt,
        updatedAt = booking.updatedAt
      }
  where
    makeRideAPIEntity :: DRide.Ride -> RD.RideDetails -> Maybe Text -> DRide.RideAPIEntity
    makeRideAPIEntity ride rideDetails driverNumber = do
      let initial = "" :: Text
      DRide.RideAPIEntity
        { id = ride.id,
          shortRideId = ride.shortId,
          status = ride.status,
          driverName = rideDetails.driverName,
          driverNumber,
          vehicleNumber = rideDetails.vehicleNumber,
          vehicleColor = fromMaybe initial rideDetails.vehicleColor,
          vehicleVariant = fromMaybe SEDAN rideDetails.vehicleVariant,
          vehicleModel = fromMaybe initial rideDetails.vehicleModel,
          computedFare = ride.fare,
          computedTotalFare = ride.totalFare,
          actualRideDistance = roundToIntegral ride.traveledDistance,
          rideRating = ride.rideRating <&> (.ratingValue),
          createdAt = ride.createdAt,
          updatedAt = ride.updatedAt,
          chargeableDistance = ride.chargeableDistance
        }
    buildRideAPIEntity :: (EsqDBFlow m r, EncFlow m r) => (DRide.Ride, RD.RideDetails) -> m DRide.RideAPIEntity
    buildRideAPIEntity (ride, rideDetails) = do
      driverNumber <- RD.getDriverNumber rideDetails
      return $ makeRideAPIEntity ride rideDetails driverNumber

    buildBookingAPIDetails :: (CacheFlow m r, EsqDBFlow m r) => BookingDetails -> m (BookingDetailsAPIEntity, [Text])
    buildBookingAPIDetails = \case
      OneWayDetails OneWayBookingDetails {..} -> do
        let details =
              OneWayDetailsAPIEntity
                OneWayBookingDetailsAPIEntity
                  { toLocation = makeBookingLocationAPIEntity toLocation
                  }
        pure (details, [])
      RentalDetails (RentalBookingDetails rentalFarePolicyId) -> do
        DRentalFP.RentalFarePolicy {..} <-
          QRentalFP.findById rentalFarePolicyId
            >>= fromMaybeM NoRentalFarePolicy
        let details = RentalDetailsAPIEntity RentalBookingDetailsAPIEntity {..}
        pure (details, descriptions)
