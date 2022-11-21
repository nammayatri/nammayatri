module Domain.Types.Booking.API where

import Beckn.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
import Domain.Types.Booking.BookingLocation (BookingLocationAPIEntity)
import qualified Domain.Types.Booking.BookingLocation as SLoc
import Domain.Types.Booking.Type
import Domain.Types.FarePolicy.FareBreakup
import qualified Domain.Types.FarePolicy.FareBreakup as DFareBreakup
import qualified Domain.Types.RentalSlab as DRentalSlab
import Domain.Types.Ride (RideAPIEntity)
import qualified Domain.Types.Ride as DRide
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.Ride as QRide
import qualified Tools.JSON as J
import qualified Tools.Schema as S

data BookingAPIEntity = BookingAPIEntity
  { id :: Id Booking,
    status :: BookingStatus,
    agencyName :: Text,
    agencyNumber :: Text,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    fromLocation :: BookingLocationAPIEntity,
    rideList :: [RideAPIEntity],
    tripTerms :: [Text],
    fareBreakup :: [FareBreakupAPIEntity],
    bookingDetails :: BookingAPIDetails,
    rideStartTime :: Maybe UTCTime,
    rideEndTime :: Maybe UTCTime,
    duration :: Maybe Seconds,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- do not change constructor names without changing fareProductConstructorModifier
data BookingAPIDetails
  = OneWayAPIDetails OneWayBookingAPIDetails
  | RentalAPIDetails DRentalSlab.RentalSlabAPIEntity
  | DriverOfferAPIDetails OneWayBookingAPIDetails
  deriving (Show, Generic)

instance ToJSON BookingAPIDetails where
  toJSON = genericToJSON J.fareProductOptions

instance FromJSON BookingAPIDetails where
  parseJSON = genericParseJSON J.fareProductOptions

instance ToSchema BookingAPIDetails where
  declareNamedSchema = genericDeclareNamedSchema S.fareProductSchemaOptions

newtype OneWayBookingAPIDetails = OneWayBookingAPIDetails
  { toLocation :: BookingLocationAPIEntity
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

buildBookingAPIEntity :: EsqDBReplicaFlow m r => Booking -> m BookingAPIEntity
buildBookingAPIEntity booking = do
  mbRide <- QRide.findActiveByRBId booking.id

  rideAPIEntityList <-
    QRide.findAllByRBId booking.id
      <&> fmap DRide.makeRideAPIEntity
  fareBreakups <- QFareBreakup.findAllByBookingId booking.id
  let bookingDetails = mkBookingAPIDetails booking.bookingDetails
  return $
    BookingAPIEntity
      { id = booking.id,
        status = booking.status,
        agencyName = booking.providerName,
        agencyNumber = booking.providerMobileNumber,
        estimatedFare = booking.estimatedFare,
        discount = booking.discount,
        estimatedTotalFare = booking.estimatedTotalFare,
        fromLocation = SLoc.makeBookingLocationAPIEntity booking.fromLocation,
        rideList = rideAPIEntityList,
        tripTerms = fromMaybe [] $ booking.tripTerms <&> (.descriptions),
        fareBreakup = DFareBreakup.mkFareBreakupAPIEntity <$> fareBreakups,
        bookingDetails,
        rideStartTime = mbRide >>= (.rideStartTime),
        rideEndTime = mbRide >>= (.rideEndTime),
        duration = getRideDuration mbRide,
        createdAt = booking.createdAt,
        updatedAt = booking.updatedAt
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
      RentalDetails DRentalSlab.RentalSlab {..} -> RentalAPIDetails DRentalSlab.RentalSlabAPIEntity {..}
      DriverOfferDetails details -> DriverOfferAPIDetails . mkOneWayAPIDetails $ details
      where
        mkOneWayAPIDetails OneWayBookingDetails {..} =
          OneWayBookingAPIDetails
            { toLocation = SLoc.makeBookingLocationAPIEntity toLocation
            }
