module Types.API.Location where

import qualified Beckn.Types.MapSearch as MapSearch
import EulerHS.Prelude

data UpdateLocationReq = UpdateLocationReq
  { lat :: Double,
    long :: Double
  }
  deriving (Generic, ToJSON, Show, FromJSON)

newtype UpdateLocationRes = UpdateLocationRes
  { status :: Text
  }
  deriving (Generic, ToJSON)

data LocationInfo = LocationInfo
  { lat :: Double,
    long :: Double
  }
  deriving (Generic, ToJSON, Show, FromJSON)

newtype GetLocationRes = GetLocationRes
  { location :: LocationInfo
  }
  deriving (Generic, ToJSON, FromJSON)

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
