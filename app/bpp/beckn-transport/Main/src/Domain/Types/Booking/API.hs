module Domain.Types.Booking.API where

import Data.OpenApi
import Domain.Types.Booking.BookingLocation
import qualified Domain.Types.Booking.BookingLocation as DLoc
import Domain.Types.Booking.Type
import Domain.Types.FarePolicy.FareBreakup
import qualified Domain.Types.FarePolicy.FareBreakup as DFareBreakup
import Domain.Types.Ride
import qualified Domain.Types.Ride as DRide
import EulerHS.Prelude hiding (id)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
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

makeBookingAPIEntity :: Booking -> BookingStatus -> [RideAPIEntity] -> [FareBreakup] -> BookingDetailsAPIEntity -> [Text] -> BookingAPIEntity
makeBookingAPIEntity booking rbStatus rideList fareBreakups bookingDetails tripTerms = do
  BookingAPIEntity
    { id = booking.id,
      status = rbStatus,
      estimatedFare = booking.estimatedFare,
      discount = booking.discount,
      estimatedTotalFare = booking.estimatedTotalFare,
      fromLocation = DLoc.makeBookingLocationAPIEntity booking.fromLocation,
      rideList,
      fareBreakup = DFareBreakup.mkFareBreakupAPIEntity <$> fareBreakups,
      riderName = booking.riderName,
      bookingDetails,
      tripTerms,
      createdAt = booking.createdAt,
      updatedAt = booking.updatedAt
    }
