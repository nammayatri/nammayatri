module Domain.Types.Booking.BookingLocation where

import Beckn.External.Maps.HasCoordinates
import Beckn.Prelude
import Beckn.Types.Id
import Domain.Types.LocationAddress

data BookingLocation = BookingLocation
  { id :: Id BookingLocation,
    lat :: Double,
    lon :: Double,
    address :: LocationAddress,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq, HasCoordinates)

data BookingLocationAPIEntity = BookingLocationAPIEntity
  { lat :: Double,
    lon :: Double,
    street :: Maybe Text,
    door :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    building :: Maybe Text,
    areaCode :: Maybe Text,
    area :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

makeBookingLocationAPIEntity :: BookingLocation -> BookingLocationAPIEntity
makeBookingLocationAPIEntity BookingLocation {..} = do
  let LocationAddress {..} = address
  BookingLocationAPIEntity
    { ..
    }
