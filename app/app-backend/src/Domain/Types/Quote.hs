{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Quote where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Id
import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
import qualified Domain.Types.SearchRequest as DSearchRequest
import qualified Tools.JSON as J

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
    quoteTerms :: [QuoteTerms],
    quoteDetails :: QuoteDetails
  }
  deriving (Generic, Show)

data QuoteDetails = OneWayDetails OneWayQuoteDetails | RentalDetails RentalQuoteDetails
  deriving (Show, Generic)

instance ToJSON QuoteDetails where
  toJSON = genericToJSON J.taggedValueOptions

instance FromJSON QuoteDetails where
  parseJSON = genericParseJSON J.taggedValueOptions

instance ToSchema QuoteDetails where
  declareNamedSchema = genericDeclareNamedSchema J.taggedValueSchemaOptions

-- Can I use distanceToNearestDriver instead of nearestDriverDistance in QuoteAPIEntity for consistency and less boilerplate?
newtype OneWayQuoteDetails = OneWayQuoteDetails
  { distanceToNearestDriver :: Double
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data RentalQuoteDetails = RentalQuoteDetails
  { baseDistance :: Double,
    baseDurationHr :: Int
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data QuoteTerms = QuoteTerms
  { id :: Id QuoteTerms,
    description :: Text
  }
  deriving (Show)

data QuoteAPIEntity = QuoteAPIEntity
  { id :: Id Quote,
    vehicleVariant :: Text,
    estimatedFare :: Amount,
    estimatedTotalFare :: Amount,
    discount :: Maybe Amount,
    agencyName :: Text,
    agencyNumber :: Text,
    agencyCompletedRidesCount :: Int,
    descriptions :: [Text],
    quoteDetails :: QuoteDetails,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

makeQuoteAPIEntity :: Quote -> QuoteAPIEntity
makeQuoteAPIEntity Quote {..} = do
  QuoteAPIEntity
    { agencyName = providerName,
      agencyNumber = providerMobileNumber,
      agencyCompletedRidesCount = providerCompletedRidesCount,
      descriptions = quoteTerms <&> (.description),
      ..
    }
