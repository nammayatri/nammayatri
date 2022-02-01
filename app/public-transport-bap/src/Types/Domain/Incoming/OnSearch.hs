module Types.Domain.Incoming.OnSearch where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Id
import Domain.Search

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
    createdAt :: UTCTime
  }
  deriving (Generic, ToJSON)

data PublicTranport = PublicTranport
  { name :: Text,
    bppLocationId :: Text,
    lat :: Double,
    lon :: Double
  }
  deriving (Generic)