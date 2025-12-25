{-# OPTIONS_GHC -Wno-orphans #-}

module SharedLogic.Quote where

import Control.Applicative
import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
import qualified Domain.Action.UI.DriverOffer as UDriverOffer
import qualified Domain.Action.UI.InterCityDetails as DInterCityDetails
import qualified Domain.Action.UI.RentalDetails as DRentalDetails
import qualified Domain.Action.UI.SpecialZoneQuote as USpecialZoneQuote
import Domain.Types.BppDetails as DBppDetails
import qualified Domain.Types.DriverOffer as DDriverOffer
import Domain.Types.Quote
import Domain.Types.QuoteBreakup
import Domain.Types.ServiceTierType as DVST
import qualified Domain.Types.SpecialZoneQuote as DSpecialZoneQuote
import qualified Domain.Types.Trip as DTC
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Tools.JSON as J
import qualified Tools.Schema as S

data QuoteAPIEntity = QuoteAPIEntity
  { id :: Id Quote,
    vehicleVariant :: DVST.ServiceTierType,
    serviceTierName :: Maybe Text,
    serviceTierShortDesc :: Maybe Text,
    estimatedFare :: Money,
    estimatedTotalFare :: Money,
    estimatedPickupDuration :: Maybe Seconds,
    discount :: Maybe Money,
    estimatedFareWithCurrency :: PriceAPIEntity,
    estimatedTotalFareWithCurrency :: PriceAPIEntity,
    discountWithCurrency :: Maybe PriceAPIEntity,
    agencyName :: Text,
    agencyNumber :: Maybe Text,
    tripTerms :: [Text],
    quoteDetails :: QuoteAPIDetails,
    specialLocationTag :: Maybe Text,
    quoteFareBreakup :: [QuoteBreakupAPIEntity],
    agencyCompletedRidesCount :: Maybe Int,
    vehicleServiceTierAirConditioned :: Maybe Double,
    isAirConditioned :: Maybe Bool,
    vehicleServiceTierSeatingCapacity :: Maybe Int,
    tripCategory :: Maybe DTC.TripCategory,
    createdAt :: UTCTime,
    isValueAddNP :: Bool,
    validTill :: UTCTime,
    vehicleIconUrl :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data QuoteBreakupAPIEntity = QuoteBreakupAPIEntity
  { title :: Text,
    priceWithCurrency :: PriceAPIEntity
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

makeQuoteAPIEntity :: Quote -> DBppDetails.BppDetails -> Bool -> QuoteAPIEntity
makeQuoteAPIEntity (Quote {..}) bppDetails isValueAddNP =
  let agencyCompletedRidesCount = Just 0
      providerNum = fromMaybe "+91" bppDetails.supportNumber
   in QuoteAPIEntity
        { agencyName = bppDetails.name,
          agencyNumber = Just providerNum,
          tripTerms = maybe [] (.descriptions) tripTerms,
          quoteDetails = mkQuoteAPIDetails (tollChargesInfo <&> (mkPriceAPIEntity . (.tollCharges))) quoteDetails,
          estimatedFare = estimatedFare.amountInt,
          estimatedTotalFare = estimatedTotalFare.amountInt,
          discount = discount <&> (.amountInt),
          estimatedFareWithCurrency = mkPriceAPIEntity estimatedFare,
          estimatedTotalFareWithCurrency = mkPriceAPIEntity estimatedTotalFare,
          discountWithCurrency = mkPriceAPIEntity <$> discount,
          vehicleVariant = vehicleServiceTierType,
          quoteFareBreakup = mkQuoteBreakupAPIEntity <$> quoteBreakupList,
          vehicleIconUrl = showBaseUrl <$> vehicleIconUrl,
          ..
        }

mkQuoteBreakupAPIEntity :: QuoteBreakup -> QuoteBreakupAPIEntity
mkQuoteBreakupAPIEntity QuoteBreakup {..} = do
  QuoteBreakupAPIEntity
    { title = title,
      priceWithCurrency = mkPriceAPIEntity price
    }

instance ToJSON QuoteAPIDetails where
  toJSON = genericToJSON J.fareProductOptions

instance FromJSON QuoteAPIDetails where
  parseJSON = genericParseJSON J.fareProductOptions

instance ToSchema QuoteAPIDetails where
  declareNamedSchema = genericDeclareNamedSchema S.fareProductSchemaOptions

mkQuoteAPIDetails :: Maybe PriceAPIEntity -> QuoteDetails -> QuoteAPIDetails
mkQuoteAPIDetails tollCharges = \case
  MeterRideDetails MeterRideQuoteDetails {..} -> MeterRideAPIDetails MeterRideQuoteAPIDetails {..}
  RentalDetails details -> RentalAPIDetails $ DRentalDetails.mkRentalDetailsAPIEntity details tollCharges
  OneWayDetails OneWayQuoteDetails {..} ->
    OneWayAPIDetails
      OneWayQuoteAPIDetails
        { distanceToNearestDriver = distanceToHighPrecMeters distanceToNearestDriver,
          distanceToNearestDriverWithUnit = distanceToNearestDriver,
          ..
        }
  AmbulanceDetails DDriverOffer.DriverOffer {..} ->
    let distanceToPickup' = distanceToHighPrecMeters <$> distanceToPickup
     in DriverOfferAPIDetails UDriverOffer.DriverOfferAPIEntity {distanceToPickup = distanceToPickup', distanceToPickupWithUnit = distanceToPickup, durationToPickup = durationToPickup, rating = rating, isUpgradedToCab = fromMaybe False isUpgradedToCab, ..}
  DeliveryDetails DDriverOffer.DriverOffer {..} ->
    -- TODO::is delivery entity required
    let distanceToPickup' = distanceToHighPrecMeters <$> distanceToPickup
     in DriverOfferAPIDetails UDriverOffer.DriverOfferAPIEntity {distanceToPickup = distanceToPickup', distanceToPickupWithUnit = distanceToPickup, durationToPickup = durationToPickup, rating = rating, isUpgradedToCab = fromMaybe False isUpgradedToCab, ..}
  DriverOfferDetails DDriverOffer.DriverOffer {..} ->
    let distanceToPickup' = (distanceToHighPrecMeters <$> distanceToPickup) <|> (Just . HighPrecMeters $ toCentesimal 0) -- TODO::remove this default value
        distanceToPickupWithUnit' = distanceToPickup <|> Just (Distance 0 Meter) -- TODO::remove this default value
        durationToPickup' = durationToPickup <|> Just 0 -- TODO::remove this default value
        rating' = rating <|> Just (toCentesimal 500) -- TODO::remove this default value
     in DriverOfferAPIDetails UDriverOffer.DriverOfferAPIEntity {distanceToPickup = distanceToPickup', distanceToPickupWithUnit = distanceToPickupWithUnit', durationToPickup = durationToPickup', rating = rating', isUpgradedToCab = fromMaybe False isUpgradedToCab, ..}
  OneWaySpecialZoneDetails DSpecialZoneQuote.SpecialZoneQuote {..} -> OneWaySpecialZoneAPIDetails USpecialZoneQuote.SpecialZoneQuoteAPIEntity {..}
  InterCityDetails details -> InterCityAPIDetails $ DInterCityDetails.mkInterCityDetailsAPIEntity details tollCharges

mkQAPIEntityList :: [Quote] -> [DBppDetails.BppDetails] -> [Bool] -> [QuoteAPIEntity]
mkQAPIEntityList (q : qRemaining) (bpp : bppRemaining) (isValueAddNP : remVNP) =
  makeQuoteAPIEntity q bpp isValueAddNP : mkQAPIEntityList qRemaining bppRemaining remVNP
mkQAPIEntityList [] [] [] = []
mkQAPIEntityList _ _ _ = [] -- This should never happen as all the list are of same length
