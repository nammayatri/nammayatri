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

import Control.Applicative
import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
import qualified Domain.Action.UI.SpecialZoneQuote as USpecialZoneQuote
import qualified Domain.Types.BppDetails as DBppDetails
import qualified Domain.Types.DriverOffer as DDriverOffer
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.RentalDetails as DRentalDetails
import qualified Domain.Types.SearchRequest as DSearchRequest
import qualified Domain.Types.SpecialZoneQuote as DSpecialZoneQuote
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleServiceTier as DVST
import Domain.Types.VehicleVariant as Variant
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.GenericPretty
import qualified Tools.JSON as J
import qualified Tools.Schema as S

data Quote = Quote
  { id :: Id Quote,
    requestId :: Id DSearchRequest.SearchRequest,
    estimatedFare :: Price,
    discount :: Maybe Price,
    estimatedTotalFare :: Price,
    providerId :: Text,
    providerUrl :: BaseUrl,
    itemId :: Text,
    vehicleServiceTierType :: DVST.VehicleServiceTierType,
    serviceTierName :: Maybe Text,
    serviceTierShortDesc :: Maybe Text,
    tripTerms :: Maybe DTripTerms.TripTerms,
    quoteDetails :: QuoteDetails,
    merchantId :: Id DMerchant.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    specialLocationTag :: Maybe Text,
    clientBundleVersion :: Maybe Version,
    clientSdkVersion :: Maybe Version,
    clientConfigVersion :: Maybe Version,
    clientDevice :: Maybe Device,
    backendConfigVersion :: Maybe Version,
    backendAppVersion :: Maybe Text,
    createdAt :: UTCTime,
    validTill :: UTCTime
  }
  deriving (Generic, Show)

data QuoteDetails
  = OneWayDetails OneWayQuoteDetails
  | InterCityDetails DSpecialZoneQuote.SpecialZoneQuote
  | RentalDetails DRentalDetails.RentalDetails
  | DriverOfferDetails DDriverOffer.DriverOffer
  | OneWaySpecialZoneDetails DSpecialZoneQuote.SpecialZoneQuote
  deriving (Generic, Show)
  deriving (PrettyShow) via Showable QuoteDetails

newtype OneWayQuoteDetails = OneWayQuoteDetails
  { distanceToNearestDriver :: Distance
  }
  deriving (Generic, Show, PrettyShow)

data QuoteAPIEntity = QuoteAPIEntity
  { id :: Id Quote,
    vehicleVariant :: Variant.VehicleVariant,
    serviceTierName :: Maybe Text,
    serviceTierShortDesc :: Maybe Text,
    estimatedFare :: Money,
    estimatedTotalFare :: Money,
    discount :: Maybe Money,
    estimatedFareWithCurrency :: PriceAPIEntity,
    estimatedTotalFareWithCurrency :: PriceAPIEntity,
    discountWithCurrency :: Maybe PriceAPIEntity,
    agencyName :: Text,
    agencyNumber :: Maybe Text,
    tripTerms :: [Text],
    quoteDetails :: QuoteAPIDetails,
    specialLocationTag :: Maybe Text,
    agencyCompletedRidesCount :: Maybe Int,
    createdAt :: UTCTime,
    isValueAddNP :: Bool,
    validTill :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- do not change constructor names without changing fareProductConstructorModifier
data QuoteAPIDetails
  = OneWayAPIDetails OneWayQuoteAPIDetails
  | InterCityAPIDetails USpecialZoneQuote.InterCityQuoteAPIEntity
  | RentalAPIDetails DRentalDetails.RentalDetailsAPIEntity
  | DriverOfferAPIDetails DDriverOffer.DriverOfferAPIEntity
  | OneWaySpecialZoneAPIDetails USpecialZoneQuote.SpecialZoneQuoteAPIEntity
  deriving (Show, Generic)

instance ToJSON QuoteAPIDetails where
  toJSON = genericToJSON J.fareProductOptions

instance FromJSON QuoteAPIDetails where
  parseJSON = genericParseJSON J.fareProductOptions

instance ToSchema QuoteAPIDetails where
  declareNamedSchema = genericDeclareNamedSchema S.fareProductSchemaOptions

data OneWayQuoteAPIDetails = OneWayQuoteAPIDetails
  { distanceToNearestDriver :: HighPrecMeters,
    distanceToNearestDriverWithUnit :: Distance
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

newtype OneWaySpecialZoneQuoteAPIDetails = OneWaySpecialZoneQuoteAPIDetails
  { quoteId :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

mkQuoteAPIDetails :: QuoteDetails -> QuoteAPIDetails
mkQuoteAPIDetails = \case
  RentalDetails details -> RentalAPIDetails $ DRentalDetails.mkRentalDetailsAPIEntity details
  OneWayDetails OneWayQuoteDetails {..} ->
    OneWayAPIDetails
      OneWayQuoteAPIDetails
        { distanceToNearestDriver = distanceToHighPrecMeters distanceToNearestDriver,
          distanceToNearestDriverWithUnit = distanceToNearestDriver
        }
  DriverOfferDetails DDriverOffer.DriverOffer {..} ->
    let distanceToPickup' = (distanceToHighPrecMeters <$> distanceToPickup) <|> (Just . HighPrecMeters $ toCentesimal 0) -- TODO::remove this default value
        distanceToPickupWithUnit' = distanceToPickup <|> Just (Distance 0 Meter) -- TODO::remove this default value
        durationToPickup' = durationToPickup <|> Just 0 -- TODO::remove this default value
        rating' = rating <|> Just (toCentesimal 500) -- TODO::remove this default value
     in DriverOfferAPIDetails DDriverOffer.DriverOfferAPIEntity {distanceToPickup = distanceToPickup', distanceToPickupWithUnit = distanceToPickupWithUnit', durationToPickup = durationToPickup', rating = rating', ..}
  OneWaySpecialZoneDetails DSpecialZoneQuote.SpecialZoneQuote {..} -> OneWaySpecialZoneAPIDetails USpecialZoneQuote.SpecialZoneQuoteAPIEntity {..}
  InterCityDetails DSpecialZoneQuote.SpecialZoneQuote {..} -> InterCityAPIDetails USpecialZoneQuote.InterCityQuoteAPIEntity {..}

mkQAPIEntityList :: [Quote] -> [DBppDetails.BppDetails] -> [Bool] -> [QuoteAPIEntity]
mkQAPIEntityList (q : qRemaining) (bpp : bppRemaining) (isValueAddNP : remVNP) =
  makeQuoteAPIEntity q bpp isValueAddNP : mkQAPIEntityList qRemaining bppRemaining remVNP
mkQAPIEntityList [] [] [] = []
mkQAPIEntityList _ _ _ = [] -- This should never happen as all the list are of same length

makeQuoteAPIEntity :: Quote -> DBppDetails.BppDetails -> Bool -> QuoteAPIEntity
makeQuoteAPIEntity (Quote {..}) bppDetails isValueAddNP =
  let agencyCompletedRidesCount = Just 0
      providerNum = fromMaybe "+91" bppDetails.supportNumber
   in QuoteAPIEntity
        { agencyName = bppDetails.name,
          agencyNumber = Just providerNum,
          tripTerms = maybe [] (.descriptions) tripTerms,
          quoteDetails = mkQuoteAPIDetails quoteDetails,
          estimatedFare = estimatedFare.amountInt,
          estimatedTotalFare = estimatedTotalFare.amountInt,
          discount = discount <&> (.amountInt),
          estimatedFareWithCurrency = mkPriceAPIEntity estimatedFare,
          estimatedTotalFareWithCurrency = mkPriceAPIEntity estimatedTotalFare,
          discountWithCurrency = mkPriceAPIEntity <$> discount,
          vehicleVariant = DVST.castServiceTierToVariant vehicleServiceTierType, -- to maintain backward compatibility
          ..
        }
