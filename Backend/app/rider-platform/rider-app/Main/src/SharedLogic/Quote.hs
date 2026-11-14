{-# OPTIONS_GHC -Wno-orphans #-}

module SharedLogic.Quote where

import qualified BecknV2.OnDemand.FareBreakup.Core as FB
import qualified BecknV2.OnDemand.Settlement as PaymentCharge
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
import qualified Lib.Types.SpecialLocation as SL
import qualified SharedLogic.Offer as SOffer
import qualified SharedLogic.Settlement as Settlement
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
    fareSettlementType :: Maybe SL.FareSettlementType,
    quoteFareBreakup :: [QuoteBreakupAPIEntity],
    agencyCompletedRidesCount :: Maybe Int,
    vehicleServiceTierAirConditioned :: Maybe Double,
    isAirConditioned :: Maybe Bool,
    vehicleServiceTierSeatingCapacity :: Maybe Int,
    vehicleServiceTierLuggageCapacity :: Maybe Int,
    tripCategory :: Maybe DTC.TripCategory,
    bookingDepositWithCurrency :: Maybe PriceAPIEntity,
    customerOffers :: Maybe SOffer.CumulativeOfferResp,
    createdAt :: UTCTime,
    isValueAddNP :: Bool,
    validTill :: UTCTime,
    vehicleIconUrl :: Maybe Text,
    area :: Maybe Text,
    navigationInstruction :: Maybe Text,
    componentGrossMultipliers :: Maybe (FB.RideTaxRates PaymentCharge.GrossMultiplier)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data QuoteBreakupAPIEntity = QuoteBreakupAPIEntity
  { title :: Text,
    priceWithCurrency :: PriceAPIEntity
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

makeQuoteAPIEntity :: Maybe PaymentCharge.PaymentChargeDetails -> Quote -> DBppDetails.BppDetails -> Bool -> QuoteAPIEntity
makeQuoteAPIEntity mbPaymentCharge (Quote {..}) bppDetails isValueAddNP =
  let agencyCompletedRidesCount = Just 0
      providerNum = fromMaybe "+91" bppDetails.supportNumber
      gross fare = mkPrice (Just fare.currency) $ case DTC.tripCategoryToPricingPolicy <$> tripCategory of
        Just DTC.EstimateBased {} -> fare.amount
        _ -> Settlement.grossTotalTaxInclusive mbPaymentCharge fare.amount
      grossComponent component charge = mkPrice (Just charge.currency) (roundAmountByCurrency' charge.currency (Settlement.grossTaxExclusive component mbPaymentCharge riderTaxRates charge.amount))
   in QuoteAPIEntity
        { agencyName = bppDetails.name,
          agencyNumber = Just providerNum,
          tripTerms = [],
          quoteDetails = mkQuoteAPIDetails (grossComponent (.rideFare)) (tollChargesInfo <&> (mkPriceAPIEntity . grossComponent (.toll) . (.tollCharges))) quoteDetails,
          estimatedFare = (gross estimatedFare).amountInt,
          estimatedTotalFare = (gross estimatedTotalFare).amountInt,
          discount = discount <&> (.amountInt),
          estimatedFareWithCurrency = mkRoundedPriceAPIEntity (gross estimatedFare),
          estimatedTotalFareWithCurrency = mkRoundedPriceAPIEntity (gross estimatedTotalFare),
          discountWithCurrency = mkRoundedPriceAPIEntity <$> discount,
          bookingDepositWithCurrency = mkPriceAPIEntity . mkPrice (Just estimatedFare.currency) <$> bookingDeposit,
          vehicleVariant = vehicleServiceTierType,
          customerOffers = Nothing,
          quoteFareBreakup = mkQuoteBreakupAPIEntity <$> FB.withoutHiddenLines (.title) quoteBreakupList,
          vehicleIconUrl = showBaseUrl <$> vehicleIconUrl,
          componentGrossMultipliers = PaymentCharge.grossMultipliers mbPaymentCharge <$> riderTaxRates,
          ..
        }

mkQuoteBreakupAPIEntity :: QuoteBreakup -> QuoteBreakupAPIEntity
mkQuoteBreakupAPIEntity QuoteBreakup {..} = do
  QuoteBreakupAPIEntity
    { title = title,
      priceWithCurrency = mkRoundedPriceAPIEntity price
    }

instance ToJSON QuoteAPIDetails where
  toJSON = genericToJSON J.fareProductOptions

instance FromJSON QuoteAPIDetails where
  parseJSON = genericParseJSON J.fareProductOptions

instance ToSchema QuoteAPIDetails where
  declareNamedSchema = genericDeclareNamedSchema S.fareProductSchemaOptions

mkQuoteAPIDetails :: (Price -> Price) -> Maybe PriceAPIEntity -> QuoteDetails -> QuoteAPIDetails
mkQuoteAPIDetails grossRate tollCharges = \case
  MeterRideDetails MeterRideQuoteDetails {..} -> MeterRideAPIDetails MeterRideQuoteAPIDetails {..}
  RentalDetails details -> RentalAPIDetails $ DRentalDetails.mkRentalDetailsAPIEntity grossRate details tollCharges
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
  InterCityDetails details -> InterCityAPIDetails $ DInterCityDetails.mkInterCityDetailsAPIEntity grossRate details tollCharges
  EasyBookingDetails details -> EasyBookingAPIDetails $ DRentalDetails.mkRentalDetailsAPIEntity grossRate details tollCharges

mkQAPIEntityList :: Applicative m => Maybe PaymentCharge.PaymentChargeDetails -> [Quote] -> [DBppDetails.BppDetails] -> [Bool] -> m [QuoteAPIEntity]
mkQAPIEntityList mbPaymentCharge quotes bppDetailList isValueAddNPList = pure (zipWith3 (makeQuoteAPIEntity mbPaymentCharge) quotes bppDetailList isValueAddNPList)
