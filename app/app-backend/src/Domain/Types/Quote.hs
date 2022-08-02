{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Quote (module Domain.Types.Quote, module Types.Common) where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.GenericPretty
import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
import qualified Domain.Types.DriverOffer as DDriverOffer
import qualified Domain.Types.RentalSlab as DRentalSlab
import qualified Domain.Types.SearchRequest as DSearchRequest
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleVariant (VehicleVariant)
import qualified Tools.JSON as J
import qualified Tools.Schema as S
import Types.Common

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
  deriving (Generic, Show, PrettyShow)

data QuoteDetails
  = OneWayDetails OneWayQuoteDetails
  | RentalDetails DRentalSlab.RentalSlab
  | DriverOfferDetails DDriverOffer.DriverOffer
  deriving (Generic, Show)
  deriving (PrettyShow) via Showable QuoteDetails

newtype OneWayQuoteDetails = OneWayQuoteDetails
  { distanceToNearestDriver :: HighPrecMeters
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema, PrettyShow)

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

-- do not change constructor names without changing fareProductConstructorModifier
data QuoteAPIDetails
  = OneWayAPIDetails OneWayQuoteAPIDetails
  | RentalAPIDetails DRentalSlab.RentalSlabAPIEntity
  | DriverOfferAPIDetails DDriverOffer.DriverOfferAPIEntity
  deriving (Show, Generic)

instance ToJSON QuoteAPIDetails where
  toJSON = genericToJSON J.fareProductOptions

instance FromJSON QuoteAPIDetails where
  parseJSON = genericParseJSON J.fareProductOptions

instance ToSchema QuoteAPIDetails where
  declareNamedSchema = genericDeclareNamedSchema S.fareProductSchemaOptions

newtype OneWayQuoteAPIDetails = OneWayQuoteAPIDetails
  { distanceToNearestDriver :: HighPrecMeters
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

mkQuoteAPIDetails :: QuoteDetails -> QuoteAPIDetails
mkQuoteAPIDetails = \case
  RentalDetails DRentalSlab.RentalSlab {..} -> RentalAPIDetails DRentalSlab.RentalSlabAPIEntity {..}
  OneWayDetails OneWayQuoteDetails {..} -> OneWayAPIDetails OneWayQuoteAPIDetails {..}
  DriverOfferDetails DDriverOffer.DriverOffer {..} -> DriverOfferAPIDetails DDriverOffer.DriverOfferAPIEntity {..}

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
