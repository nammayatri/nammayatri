{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Quote where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Id
import Domain.FerryStation
import Domain.Search

data Quote = Quote
  { id :: Id Quote,
    searchId :: Id Search,
    bppId :: Text,
    bppUrl :: BaseUrl,
    description :: Text,
    fare :: Amount,
    departureTime :: UTCTime,
    arrivalTime :: UTCTime,
    departureStationId :: Id FerryStation,
    arrivalStationId :: Id FerryStation,
    createdAt :: UTCTime
  }
  deriving (Generic)

data QuoteAPIEntity = QuoteAPIEntity
  { id :: Id Quote,
    description :: Text,
    fare :: Amount,
    departureTime :: UTCTime,
    arrivalTime :: UTCTime,
    departureStation :: FerryStationAPIEntity,
    arrivalStation :: FerryStationAPIEntity,
    createdAt :: UTCTime
  }
  deriving (Generic, ToJSON)

makeQuoteAPIEntity :: Quote -> FerryStation -> FerryStation -> QuoteAPIEntity
makeQuoteAPIEntity Quote {..} departureStation arrivalStation = do
  let departureStationAPIEntity = makeFerryStationAPIEntity departureStation
      arrivalStationAPIEntity = makeFerryStationAPIEntity arrivalStation
  QuoteAPIEntity
    { departureStation = departureStationAPIEntity,
      arrivalStation = arrivalStationAPIEntity,
      ..
    }