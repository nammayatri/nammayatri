{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.Quote where

import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
import qualified Domain.Types.DriverOffer as DDriverOffer
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.RentalSlab as DRentalSlab
import qualified Domain.Types.SearchRequest as DSearchRequest
import qualified Domain.Types.SpecialZoneQuote as DSpecialZoneQuote
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleVariant (VehicleVariant)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.GenericPretty
import qualified Tools.JSON as J
import qualified Tools.Schema as S

data Quote = Quote
  { id :: Id Quote,
    requestId :: Id DSearchRequest.SearchRequest,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    providerId :: Text,
    providerUrl :: BaseUrl,
    providerName :: Text,
    providerMobileNumber :: Text,
    itemId :: Text,
    providerCompletedRidesCount :: Int,
    vehicleVariant :: VehicleVariant,
    tripTerms :: Maybe DTripTerms.TripTerms,
    quoteDetails :: QuoteDetails,
    merchantId :: Id DMerchant.Merchant,
    specialLocationTag :: Maybe Text,
    -- publicTransportQuoteId :: Maybe Text, -- used from here
    createdAt :: UTCTime
  }
  deriving (Generic, Show, PrettyShow)

data QuoteDetails
  = OneWayDetails OneWayQuoteDetails
  | RentalDetails DRentalSlab.RentalSlab
  | DriverOfferDetails DDriverOffer.DriverOffer
  | OneWaySpecialZoneDetails DSpecialZoneQuote.SpecialZoneQuote
  deriving (Generic, Show)
  deriving (PrettyShow) via Showable QuoteDetails

newtype OneWayQuoteDetails = OneWayQuoteDetails
  { distanceToNearestDriver :: HighPrecMeters
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema, PrettyShow)

data QuoteAPIEntity = QuoteAPIEntity
  { id :: Id Quote,
    vehicleVariant :: VehicleVariant,
    estimatedFare :: Money,
    estimatedTotalFare :: Money,
    discount :: Maybe Money,
    agencyName :: Text,
    agencyNumber :: Text,
    agencyCompletedRidesCount :: Int,
    tripTerms :: [Text],
    quoteDetails :: QuoteAPIDetails,
    specialLocationTag :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- do not change constructor names without changing fareProductConstructorModifier
data QuoteAPIDetails
  = OneWayAPIDetails OneWayQuoteAPIDetails
  | RentalAPIDetails DRentalSlab.RentalSlabAPIEntity
  | DriverOfferAPIDetails DDriverOffer.DriverOfferAPIEntity
  | OneWaySpecialZoneAPIDetails DSpecialZoneQuote.SpecialZoneQuoteAPIEntity
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

newtype OneWaySpecialZoneQuoteAPIDetails = OneWaySpecialZoneQuoteAPIDetails
  { quoteId :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

mkQuoteAPIDetails :: QuoteDetails -> QuoteAPIDetails
mkQuoteAPIDetails = \case
  RentalDetails DRentalSlab.RentalSlab {..} -> RentalAPIDetails DRentalSlab.RentalSlabAPIEntity {..}
  OneWayDetails OneWayQuoteDetails {..} -> OneWayAPIDetails OneWayQuoteAPIDetails {..}
  DriverOfferDetails DDriverOffer.DriverOffer {..} -> DriverOfferAPIDetails DDriverOffer.DriverOfferAPIEntity {..}
  OneWaySpecialZoneDetails DSpecialZoneQuote.SpecialZoneQuote {..} -> OneWaySpecialZoneAPIDetails DSpecialZoneQuote.SpecialZoneQuoteAPIEntity {..}

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
