module SharedLogic.Booking where

import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
import Domain.Types.Booking.BookingLocation (BookingLocationAPIEntity)
import qualified Domain.Types.RentalSlab as DRentalSlab
import EulerHS.Prelude hiding (id)
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Tools.JSON as J
import qualified Tools.Schema as S

data OneWaySpecialZoneBookingAPIDetails = OneWaySpecialZoneBookingAPIDetails
  { toLocation :: BookingLocationAPIEntity,
    estimatedDistance :: HighPrecMeters,
    otpCode :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data OneWayBookingAPIDetails = OneWayBookingAPIDetails
  { toLocation :: BookingLocationAPIEntity,
    estimatedDistance :: HighPrecMeters
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data BookingAPIDetails
  = OneWayAPIDetails OneWayBookingAPIDetails
  | RentalAPIDetails DRentalSlab.RentalSlabAPIEntity
  | DriverOfferAPIDetails OneWayBookingAPIDetails
  | OneWaySpecialZoneAPIDetails OneWaySpecialZoneBookingAPIDetails
  deriving (Show, Generic)

instance ToJSON BookingAPIDetails where
  toJSON = genericToJSON J.fareProductOptions

instance FromJSON BookingAPIDetails where
  parseJSON = genericParseJSON J.fareProductOptions

instance ToSchema BookingAPIDetails where
  declareNamedSchema = genericDeclareNamedSchema S.fareProductSchemaOptions
