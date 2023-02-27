{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.RecurringQuote where

import Data.OpenApi
  ( ToSchema (..),
    genericDeclareNamedSchema,
  )
import qualified Domain.Types.DriverOffer as DDriverOffer
import qualified Domain.Types.RentalSlab as DRentalSlab
import qualified Domain.Types.SearchRequest as DSearchRequest
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleVariant (VehicleVariant)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.GenericPretty
import qualified Tools.JSON as J
import qualified Tools.Schema as S

data RecurringQuote = RecurringQuote
  { id :: Id RecurringQuote,
    requestId :: Id DSearchRequest.SearchRequest,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    providerId :: Text,
    providerUrl :: BaseUrl,
    tripTerms :: Maybe DTripTerms.TripTerms,
    quoteDetails :: RecurringQuoteDetails,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, PrettyShow)

newtype RecurringQuoteDetails = RecurringQuoteDetails
  { distanceToNearestDriver :: HighPrecMeters
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema, PrettyShow)

data RecurringQuoteAPIEntity = RecurringQuoteAPIEntity
  { id :: Id RecurringQuote,
    vehicleVariant :: VehicleVariant,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    agencyName :: Text,
    agencyNumber :: Text,
    tripTerms :: [Text],
    quoteDetails :: RecurringQuoteAPIDetails,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

newtype RecurringQuoteAPIDetails = RecurringQuoteAPIDetails
  { distanceToNearestDriver :: HighPrecMeters
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

mkQuoteAPIDetails :: RecurringQuoteDetails -> RecurringQuoteAPIDetails
mkQuoteAPIDetails RecurringQuoteDetails {..} =
  RecurringQuoteAPIDetails {..}

makeQuoteAPIEntity :: RecurringQuote -> RecurringQuoteAPIEntity
makeQuoteAPIEntity RecurringQuote {..} = do
  RecurringQuoteAPIEntity
    { tripTerms = fromMaybe [] $ tripTerms <&> (.descriptions),
      quoteDetails = mkQuoteAPIDetails quoteDetails,
      ..
    }
