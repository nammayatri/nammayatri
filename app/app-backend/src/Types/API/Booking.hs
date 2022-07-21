module Types.API.Booking where

import Beckn.Types.Amount
import Beckn.Types.Id
import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
import Data.Time (UTCTime)
import Domain.Types.Booking (Booking, BookingStatus)
import Domain.Types.BookingLocation (BookingLocationAPIEntity)
import Domain.Types.FareBreakup (FareBreakupAPIEntity)
import qualified Domain.Types.RentalSlab as DRentalSlab
import Domain.Types.Ride (RideAPIEntity)
import EulerHS.Prelude hiding (id)
import qualified Tools.JSON as J
import qualified Tools.Schema as S

data BookingStatusRes = BookingStatusRes
  { id :: Id Booking,
    status :: BookingStatus,
    agencyName :: Text,
    agencyNumber :: Text,
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount,
    fromLocation :: BookingLocationAPIEntity,
    rideList :: [RideAPIEntity],
    tripTerms :: [Text],
    fareBreakup :: [FareBreakupAPIEntity],
    bookingDetails :: BookingAPIDetails,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype BookingListRes = BookingListRes
  { list :: [BookingStatusRes]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- do not change constructor names without changing fareProductConstructorModifier
data BookingAPIDetails = OneWayAPIDetails OneWayBookingAPIDetails | RentalAPIDetails DRentalSlab.RentalSlabAPIEntity
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
