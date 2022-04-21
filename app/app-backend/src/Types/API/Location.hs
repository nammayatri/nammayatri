module Types.API.Location where

import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import qualified Beckn.Types.MapSearch as MapSearch
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import EulerHS.Prelude hiding (state)

data Status = PreRide | ActualRide
  deriving (Generic, ToJSON, Show, FromJSON, ToSchema)

data GetLocationRes = GetLocationRes
  { currPoint :: MapSearch.LatLong,
    totalDistance :: Double,
    status :: Status,
    lastUpdate :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

type Request = MapSearch.Request

type Response = GoogleMaps.DirectionsResp
