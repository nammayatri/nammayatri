module Types.API.Location where

import Beckn.Types.MapSearch as MapSearch
import qualified Beckn.Types.Storage.Location as SL
import Data.Swagger
import EulerHS.Prelude
import Servant.Swagger

data UpdateLocationReq = UpdateLocationReq
  { locationType :: Maybe SL.LocationType,
    lat :: Maybe Double,
    long :: Maybe Double,
    ward :: Maybe Text,
    district :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    pincode :: Maybe Text,
    address :: Maybe Text
  }
  deriving (Generic, ToSchema, ToJSON, Show, FromJSON)

newtype UpdateLocationRes = UpdateLocationRes
  { status :: Text
  }
  deriving (Generic, ToJSON, ToSchema)

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
    durationInS :: Maybe Integer, -- seconds
    distanceInM :: Maybe Float, -- meters
    bbox :: Maybe BoundingBoxWithoutCRS,
    waypoints :: Maybe GeospatialGeometry,
    snapped_waypoints :: Maybe GeospatialGeometry
  }
  deriving (Generic, ToJSON, Show, FromJSON)

newtype GetLocationRes = GetLocationRes
  { location :: Maybe LocationInfo
  }
  deriving (Generic, ToJSON, FromJSON)

data LatLong = LatLong
  { lat :: Double,
    lon :: Double
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data Request = Request
  { waypoints :: [LatLong],
    mode :: Maybe TravelMode,
    calcPoints :: Maybe Bool
  }
  deriving (Show, Generic, ToJSON, FromJSON)

type Response = MapSearch.Response
