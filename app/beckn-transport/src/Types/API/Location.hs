module Types.API.Location where

import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.MapSearch as MapSearch
import Data.Time (UTCTime)
import EulerHS.Prelude

type UpdateLocationReq = NonEmpty Waypoint

-- Short field names for lesser json array size:
data Waypoint = Waypoint
  { pt :: LatLong, -- point
    ts :: UTCTime, -- timestamp
    acc :: Maybe Double -- accuracy, optional for now
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type UpdateLocationRes = APISuccess

data Status = PreRide | ActualRide
  deriving (Generic, ToJSON, Show, FromJSON)

data GetLocationRes = GetLocationRes
  { currPoint :: LatLong,
    totalDistance :: Double,
    status :: Status,
    lastUpdate :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON)

type Request = MapSearch.Request

type Response = MapSearch.Response
