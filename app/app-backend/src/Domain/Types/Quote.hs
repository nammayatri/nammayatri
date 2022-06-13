{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Quote where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import Data.Aeson
import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
import qualified Data.OpenApi as OpenApi
import qualified Domain.Types.RentalSlab as DRentalSlab
import qualified Domain.Types.SearchRequest as DSearchRequest
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleVariant (VehicleVariant)

-- move this in separate file
data FareProductType = ONE_WAY | RENTAL deriving (Generic, Show, Read, Eq, FromJSON, ToJSON, ToSchema)

data Quote = Quote
  { id :: Id Quote,
    requestId :: Id DSearchRequest.SearchRequest,
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount,
    providerId :: Text,
    providerUrl :: BaseUrl,
    providerName :: Text,
    providerMobileNumber :: Text,
    providerCompletedRidesCount :: Int,
    vehicleVariant :: VehicleVariant,
    tripTerms :: Maybe DTripTerms.TripTerms,
    quoteDetails :: QuoteDetails,
    createdAt :: UTCTime
  }
  deriving (Generic, Show)

data QuoteDetails = OneWayDetails OneWayQuoteDetails | RentalDetails DRentalSlab.RentalSlab
  deriving (Generic, Show)

newtype OneWayQuoteDetails = OneWayQuoteDetails
  { distanceToNearestDriver :: HighPrecMeters
  }
  deriving (Generic, Show)

data QuoteAPIEntity = QuoteAPIEntity
  { id :: Id Quote,
    vehicleVariant :: VehicleVariant,
    estimatedFare :: Amount,
    estimatedTotalFare :: Amount,
    discount :: Maybe Amount,
    agencyName :: Text,
    agencyNumber :: Text,
    agencyCompletedRidesCount :: Int,
    tripTerms :: [Text],
    quoteDetails :: QuoteAPIDetails,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data QuoteAPIDetails = OneWayAPIDetails OneWayQuoteAPIDetails | RentalAPIDetails RentalQuoteAPIDetails
  deriving (Show, Generic)

instance ToJSON QuoteAPIDetails where
  toJSON = genericToJSON fareProductOptions

instance FromJSON QuoteAPIDetails where
  parseJSON = genericParseJSON fareProductOptions

instance ToSchema QuoteAPIDetails where
  declareNamedSchema = genericDeclareNamedSchema fareProductSchemaOptions

fareProductOptions :: Options
fareProductOptions =
  defaultOptions
    { sumEncoding = fareProductTaggedObject,
      constructorTagModifier = fareProductConstructorModifier
    }

fareProductSchemaOptions :: OpenApi.SchemaOptions
fareProductSchemaOptions =
  OpenApi.defaultSchemaOptions
    { OpenApi.sumEncoding = fareProductTaggedObject,
      OpenApi.constructorTagModifier = fareProductConstructorModifier
    }

fareProductTaggedObject :: SumEncoding
fareProductTaggedObject =
  defaultTaggedObject
    { tagFieldName = "fareProductType"
    }

fareProductConstructorModifier :: String -> String
fareProductConstructorModifier = \case
  "OneWayDetails" -> "ONE_WAY"
  "RentalDetails" -> "RENTAL"
  x -> x

-- Can I use distanceToNearestDriver instead of nearestDriverDistance in QuoteAPIEntity for consistency and less boilerplate?
newtype OneWayQuoteAPIDetails = OneWayQuoteAPIDetails
  { distanceToNearestDriver :: HighPrecMeters
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data RentalQuoteAPIDetails = RentalQuoteAPIDetails
  { baseDistance :: Kilometers,
    baseDuration :: Hours
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

mkQuoteAPIDetails :: QuoteDetails -> QuoteAPIDetails
mkQuoteAPIDetails = \case
  RentalDetails DRentalSlab.RentalSlab {..} -> RentalAPIDetails RentalQuoteAPIDetails {..}
  OneWayDetails OneWayQuoteDetails {..} -> OneWayAPIDetails OneWayQuoteAPIDetails {..}

makeQuoteAPIEntity :: Quote -> QuoteAPIEntity
makeQuoteAPIEntity Quote {..} = do
  QuoteAPIEntity
    { agencyName = providerName,
      agencyNumber = providerMobileNumber,
      agencyCompletedRidesCount = providerCompletedRidesCount,
      tripTerms = fromMaybe [] $ tripTerms <&> (.descriptions),
      quoteDetails = mkQuoteAPIDetails quoteDetails,
      ..
    }
