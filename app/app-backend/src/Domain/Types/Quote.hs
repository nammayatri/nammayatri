{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Quote where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Id
import qualified Domain.Types.SearchRequest as DSearchRequest

data BPPQuote

data Quote = Quote
  { id :: Id Quote,
    bppQuoteId :: Id BPPQuote,
    requestId :: Id DSearchRequest.SearchRequest,
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount,
    providerId :: Text,
    providerUrl :: BaseUrl,
    providerName :: Text,
    providerMobileNumber :: Text,
    providerCompletedRidesCount :: Int,
    distanceToNearestDriver :: Double,
    vehicleVariant :: Text,
    createdAt :: UTCTime
  }
  deriving (Generic, Show)

data QuoteAPIEntity = QuoteAPIEntity
  { id :: Id Quote,
    vehicleVariant :: Text,
    estimatedFare :: Amount,
    estimatedTotalFare :: Amount,
    discount :: Maybe Amount,
    agencyName :: Text,
    agencyNumber :: Text,
    agencyCompletedRidesCount :: Int,
    nearestDriverDistance :: Double,
    createdAt :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

makeQuoteAPIEntity :: Quote -> QuoteAPIEntity
makeQuoteAPIEntity Quote {..} =
  QuoteAPIEntity
    { agencyName = providerName,
      agencyNumber = providerMobileNumber,
      agencyCompletedRidesCount = providerCompletedRidesCount,
      nearestDriverDistance = distanceToNearestDriver,
      ..
    }
