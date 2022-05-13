{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Quote where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Id
import qualified Domain.Types.SearchRequest as DSearchRequest

data FareProductType = ONE_WAY | RENTAL deriving (Generic, Show, Read, Eq, FromJSON, ToJSON, ToSchema)

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
    vehicleVariant :: Text,
    createdAt :: UTCTime,
    quoteDetails :: QuoteDetails
  }
  deriving (Generic, Show)

data QuoteDetails = OneWayDetails OneWayQuoteDetails | RentalDetails RentalQuoteDetails
  deriving (Show)

newtype OneWayQuoteDetails = OneWayQuoteDetails
  { distanceToNearestDriver :: Double
  }
  deriving (Show)

data RentalQuoteDetails = RentalQuoteDetails
  { baseDistance :: Double,
    baseDurationHr :: Int,
    quoteTerms :: [QuoteTerms]
  }
  deriving (Show)

data QuoteTerms = QuoteTerms
  { id :: Id QuoteTerms,
    description :: Text
  }
  deriving (Show)

getDistanceToNearestDriver :: QuoteDetails -> Maybe Double
getDistanceToNearestDriver = \case
  OneWayDetails oneWayDetails -> Just oneWayDetails.distanceToNearestDriver
  RentalDetails _ -> Nothing

getBaseDistance :: QuoteDetails -> Maybe Double
getBaseDistance = \case
  OneWayDetails _ -> Nothing
  RentalDetails rentalDetails -> Just rentalDetails.baseDistance

getBaseDurationHr :: QuoteDetails -> Maybe Int
getBaseDurationHr = \case
  OneWayDetails _ -> Nothing
  RentalDetails rentalDetails -> Just rentalDetails.baseDurationHr

getDescriptions :: QuoteDetails -> [Text]
getDescriptions = \case
  OneWayDetails _ -> []
  RentalDetails rentalDetails -> rentalDetails.quoteTerms <&> (.description)

getFareProductType :: QuoteDetails -> FareProductType
getFareProductType = \case
  OneWayDetails _ -> ONE_WAY
  RentalDetails _ -> RENTAL

data QuoteAPIEntity = QuoteAPIEntity
  { id :: Id Quote,
    fareProductType :: FareProductType,
    vehicleVariant :: Text,
    estimatedFare :: Amount,
    estimatedTotalFare :: Amount,
    discount :: Maybe Amount,
    agencyName :: Text,
    agencyNumber :: Text,
    agencyCompletedRidesCount :: Int,
    nearestDriverDistance :: Maybe Double,
    baseDistance :: Maybe Double,
    baseDurationHr :: Maybe Int,
    descriptions :: [Text],
    createdAt :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

makeQuoteAPIEntity :: Quote -> QuoteAPIEntity
makeQuoteAPIEntity Quote {..} =
  QuoteAPIEntity
    { fareProductType = getFareProductType quoteDetails,
      agencyName = providerName,
      agencyNumber = providerMobileNumber,
      agencyCompletedRidesCount = providerCompletedRidesCount,
      nearestDriverDistance = getDistanceToNearestDriver quoteDetails,
      baseDistance = getBaseDistance quoteDetails,
      baseDurationHr = getBaseDurationHr quoteDetails,
      descriptions = getDescriptions quoteDetails,
      ..
    }
