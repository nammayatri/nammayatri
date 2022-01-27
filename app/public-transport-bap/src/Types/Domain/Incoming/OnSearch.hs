module Types.Domain.Incoming.OnSearch where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Id
import Domain.Types.Search

data Quote = Quote
  { txnId :: Id Search,
    bppId :: Text,
    bppUrl :: BaseUrl,
    description :: Text,
    fare :: Amount,
    departureTime :: UTCTime,
    arrivalTime :: UTCTime,
    bppDepartureLocId :: Text,
    bppArrivalLocId :: Text,
    createdAt :: UTCTime,
    routeCode :: Text
  }
  deriving (Generic, ToJSON)

data TransportStation = TransportStation
  { name :: Text,
    bppLocationId :: Text,
    lat :: Double,
    lon :: Double
  }
  deriving (Generic)
