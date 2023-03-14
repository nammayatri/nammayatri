{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.RecurringQuote where

import qualified Domain.Types.SearchRequest as DSearchRequest
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleVariant (VehicleVariant)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.GenericPretty

data RecurringQuote = RecurringQuote
  { id :: Id RecurringQuote,
    requestId :: Id DSearchRequest.SearchRequest,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    providerId :: Text,
    providerUrl :: BaseUrl,
    vehicleVariant :: VehicleVariant,
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
