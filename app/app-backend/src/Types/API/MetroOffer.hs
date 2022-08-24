module Types.API.MetroOffer where

import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.MapSearch
import Data.OpenApi (ToSchema)
import Data.Time
import Domain.Types.SearchRequest (SearchRequest)
import EulerHS.Prelude hiding (id)

data MetroOffer = MetroOffer
  { rideSearchId :: Id SearchRequest, -- search case id now
    description :: Text,
    rides :: [MetroRide],
    createdAt :: UTCTime
  }
  deriving (Show, Generic, ToSchema, FromJSON, ToJSON)

data MetroRide = MetroRide
  { schedule :: [ScheduleElement],
    departureStation :: MetroStation,
    arrivalStation :: MetroStation,
    price :: Money
  }
  deriving (Show, Generic, ToSchema, FromJSON, ToJSON)

data ScheduleElement = ScheduleElement
  { departureTime :: UTCTime,
    arrivalTime :: UTCTime
  }
  deriving (Show, Generic, ToSchema, FromJSON, ToJSON)

data MetroStation = MetroStation
  { name :: Text,
    stationCode :: Text,
    point :: LatLong
  }
  deriving (Show, Generic, ToSchema, FromJSON, ToJSON)
