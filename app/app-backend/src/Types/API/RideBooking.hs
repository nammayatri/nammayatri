module Types.API.RideBooking where

import Beckn.Types.Amount
import Beckn.Types.Id
import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
import Data.Time (UTCTime)
import Domain.Types.BookingLocation (BookingLocationAPIEntity)
import Domain.Types.FareBreakup (FareBreakupAPIEntity)
import qualified Domain.Types.RentalSlab as DRentalSlab
import Domain.Types.Ride (RideAPIEntity)
import Domain.Types.RideBooking (RideBooking, RideBookingStatus)
import EulerHS.Prelude hiding (id)
import qualified Tools.JSON as J
import qualified Tools.Schema as S

data RideBookingStatusRes = RideBookingStatusRes
  { id :: Id RideBooking,
    status :: RideBookingStatus,
    agencyName :: Text,
    agencyNumber :: Text,
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount,
    fromLocation :: BookingLocationAPIEntity,
    rideList :: [RideAPIEntity],
    tripTerms :: [Text],
    fareBreakup :: [FareBreakupAPIEntity],
    bookingDetails :: RideBookingAPIDetails,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype RideBookingListRes = RideBookingListRes
  { list :: [RideBookingStatusRes]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- do not change constructor names without changing fareProductConstructorModifier
data RideBookingAPIDetails = OneWayAPIDetails OneWayRideBookingAPIDetails | RentalAPIDetails DRentalSlab.RentalSlabAPIEntity
  deriving (Show, Generic)

instance ToJSON RideBookingAPIDetails where
  toJSON = genericToJSON J.fareProductOptions

instance FromJSON RideBookingAPIDetails where
  parseJSON = genericParseJSON J.fareProductOptions

instance ToSchema RideBookingAPIDetails where
  declareNamedSchema = genericDeclareNamedSchema S.fareProductSchemaOptions

newtype OneWayRideBookingAPIDetails = OneWayRideBookingAPIDetails
  { toLocation :: BookingLocationAPIEntity
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
