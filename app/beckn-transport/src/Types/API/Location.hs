module Types.API.Location where

import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.MapSearch as MapSearch
import Data.Time (UTCTime)
import EulerHS.Prelude

data UpdateLocationReq = UpdateLocationReq
  { lastUpdate :: Maybe UTCTime,
    waypoints :: NonEmpty LatLong
  }
  deriving (Generic, ToJSON, Show, FromJSON)

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
