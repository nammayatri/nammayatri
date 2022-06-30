module Domain.Types.BookingLocation where

import Beckn.Product.MapSearch.GoogleMaps (HasCoordinates)
import Beckn.Types.Id
import Data.Aeson
import Data.OpenApi (ToSchema)
import Data.Time
import EulerHS.Prelude hiding (id, state)

data BookingLocation = BookingLocation
  { id :: Id BookingLocation,
    lat :: Double,
    lon :: Double,
    street :: Maybe Text,
    door :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    building :: Maybe Text,
    areaCode :: Maybe Text,
    area :: Maybe Text,
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
makeBookingLocationAPIEntity BookingLocation {..} =
  BookingLocationAPIEntity
    { ..
    }
