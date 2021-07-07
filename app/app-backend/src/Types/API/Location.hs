module Types.API.Location where

import qualified Beckn.Types.MapSearch as MapSearch
import Data.Swagger
import EulerHS.Prelude hiding (state)
import qualified Types.Storage.Location as SL

data LocationInfo = LocationInfo
  { locationType :: Maybe SL.LocationType,
    lat :: Maybe Double,
    long :: Maybe Double,
    ward :: Maybe Text,
    district :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    pincode :: Maybe Text,
    address :: Maybe Text,
    bound :: Maybe Text
  }
  deriving (Generic, ToSchema, ToJSON, Show, FromJSON)

newtype GetLocationRes = GetLocationRes
  { location :: Maybe LocationInfo
  }
  deriving (Generic, ToJSON, ToSchema, FromJSON)

data LatLong = LatLong
  { lat :: Double,
    lon :: Double
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data Request = Request
  { waypoints :: [LatLong],
    mode :: Maybe MapSearch.TravelMode,
    calcPoints :: Maybe Bool
  }
  deriving (Show, Generic, ToJSON, FromJSON)

type Response = MapSearch.Response
