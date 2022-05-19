module Types.API.Location where

import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.Common (HighPrecMeters)
import Beckn.Types.MapSearch as MapSearch
import Beckn.Utils.GenericPretty (PrettyShow)
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import EulerHS.Prelude

type UpdateLocationReq = NonEmpty Waypoint

-- Short field names for lesser json array size:
data Waypoint = Waypoint
  { pt :: LatLong, -- point
    ts :: UTCTime, -- timestamp
    acc :: Maybe Double -- accuracy, optional for now
  }
  deriving (Generic, ToJSON, Show, FromJSON, ToSchema, PrettyShow)

type UpdateLocationRes = APISuccess

data Status = PreRide | ActualRide
  deriving (Generic, ToJSON, Show, FromJSON, ToSchema)

data GetLocationRes = GetLocationRes
  { currPoint :: LatLong,
    totalDistance :: HighPrecMeters,
    status :: Status,
    lastUpdate :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

type Request = MapSearch.Request

type Response = MapSearch.Response
