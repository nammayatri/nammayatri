module Domain.Types.SelectedQuote where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Id
import Beckn.Utils.GenericPretty
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleVariant (VehicleVariant)
import Types.Common

data SelectedQuote = SelectedQuote
  { id :: Id SelectedQuote,
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
    createdAt :: UTCTime,
    -- note: next fields are valid only for selected auto quotes,
    -- factor them out when something else is added
    driverName :: Text,
    durationToPickup :: Int, -- Seconds?
    distanceToPickup :: Double,
    validTill :: UTCTime,
    bppQuoteId :: Id BPPQuote,
    rating :: Maybe Double,
    quoteId :: Id DQuote.Quote
  }
  deriving (Generic, Show, PrettyShow)

data SelectedQuoteAPIEntity = SelectedQuoteAPIEntity
  { id :: Id SelectedQuote,
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount,
    providerId :: Text,
    providerName :: Text,
    providerMobileNumber :: Text,
    providerCompletedRidesCount :: Int,
    vehicleVariant :: VehicleVariant,
    tripTerms :: [Text],
    createdAt :: UTCTime,
    -- note: next fields are valid only for selected auto quotes,
    -- factor them out when something else is added
    driverName :: Text,
    durationToPickup :: Int, -- Seconds?
    distanceToPickup :: Double,
    validTill :: UTCTime,
    rating :: Maybe Double
  }
  deriving (Generic, Show, PrettyShow, ToJSON, FromJSON, ToSchema)

mkSelQuoteAPIEntity :: SelectedQuote -> SelectedQuoteAPIEntity
mkSelQuoteAPIEntity SelectedQuote {..} =
  SelectedQuoteAPIEntity
    { tripTerms = fromMaybe [] $ tripTerms <&> (.descriptions),
      ..
    }
