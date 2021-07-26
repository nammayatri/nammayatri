module Types.API.Location where

import qualified Beckn.Types.MapSearch as MapSearch
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (state)

data Status = PreRide | ActualRide
  deriving (Generic, ToJSON, Show, FromJSON)

data GetLocationRes = GetLocationRes
  { currPoint :: MapSearch.LatLong,
    totalDistance :: Double,
    status :: Status,
    lastUpdate :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON)

type Request = MapSearch.Request

type Response = MapSearch.Response
