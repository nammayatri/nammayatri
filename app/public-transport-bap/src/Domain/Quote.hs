{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Quote where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Id
import Domain.PublicTranport
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
    departureStationId :: Id PublicTranport,
    arrivalStationId :: Id PublicTranport,
    createdAt :: UTCTime
  }
  deriving (Generic, ToJSON)

data QuoteAPIEntity = QuoteAPIEntity
  { id :: Id Quote,
    description :: Text,
    fare :: Amount,
    departureTime :: UTCTime,
    arrivalTime :: UTCTime,
    departureStation :: PublicTranportAPIEntity,
    arrivalStation :: PublicTranportAPIEntity,
    createdAt :: UTCTime
  }
  deriving (Generic, ToJSON, ToSchema)

makeQuoteAPIEntity :: Quote -> PublicTranport -> PublicTranport -> QuoteAPIEntity
makeQuoteAPIEntity Quote {..} departureStation arrivalStation = do
  let departureStationAPIEntity = makePublicTranportAPIEntity departureStation
      arrivalStationAPIEntity = makePublicTranportAPIEntity arrivalStation
  QuoteAPIEntity
    { departureStation = departureStationAPIEntity,
      arrivalStation = arrivalStationAPIEntity,
      ..
    }