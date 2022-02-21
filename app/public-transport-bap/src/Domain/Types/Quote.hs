{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Quote where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Id
import Beckn.Utils.GenericPretty (PrettyShow)
import Domain.Types.Search
import Domain.Types.TransportStation

data Quote = Quote
  { id :: Id Quote,
    searchId :: Id Search,
    bppId :: Text,
    bppUrl :: BaseUrl,
    description :: Text,
    fare :: Amount,
    departureTime :: UTCTime,
    arrivalTime :: UTCTime,
    departureStationId :: Id TransportStation,
    arrivalStationId :: Id TransportStation,
    createdAt :: UTCTime,
    routeCode :: Text
  }
  deriving (Generic, Show, PrettyShow, ToJSON, FromJSON)

data QuoteAPIEntity = QuoteAPIEntity
  { id :: Id Quote,
    description :: Text,
    fare :: Amount,
    departureTime :: UTCTime,
    arrivalTime :: UTCTime,
    departureStation :: TransportStationAPIEntity,
    arrivalStation :: TransportStationAPIEntity,
    createdAt :: UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, PrettyShow, Show)

makeQuoteAPIEntity :: Quote -> TransportStation -> TransportStation -> QuoteAPIEntity
makeQuoteAPIEntity Quote {..} departureStation arrivalStation = do
  let departureStationAPIEntity = makeTransportStationAPIEntity departureStation
      arrivalStationAPIEntity = makeTransportStationAPIEntity arrivalStation
  QuoteAPIEntity
    { departureStation = departureStationAPIEntity,
      arrivalStation = arrivalStationAPIEntity,
      ..
    }
