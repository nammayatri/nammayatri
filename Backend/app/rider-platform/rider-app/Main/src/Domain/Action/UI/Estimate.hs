{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Estimate where

import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.FareBreakup.Core as FB
import qualified BecknV2.OnDemand.FareBreakup.Tax as Tax
import qualified BecknV2.OnDemand.Settlement as PaymentCharge
import Data.Aeson
import Domain.Types.BppDetails
import Domain.Types.Estimate
import Domain.Types.EstimateStatus
import qualified Domain.Types.ServiceTierType as DVST
import Domain.Types.Trip (TripCategory)
import qualified Domain.Types.VehicleVariant as Vehicle
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Offer as SOffer
import qualified SharedLogic.Settlement as Settlement

data EstimateAPIEntity = EstimateAPIEntity
  { id :: Id Estimate,
    vehicleVariant :: Vehicle.VehicleVariant,
    serviceTierType :: DVST.ServiceTierType,
    serviceTierName :: Maybe Text,
    serviceTierShortDesc :: Maybe Text,
    estimatedFare :: Money,
    estimatedTotalFare :: Money,
    discount :: Maybe Money,
    estimatedFareWithCurrency :: PriceAPIEntity,
    estimatedTotalFareWithCurrency :: PriceAPIEntity,
    discountWithCurrency :: Maybe PriceAPIEntity,
    totalFareRange :: FareRangeAPIEntity,
    agencyName :: Text,
    agencyNumber :: Text,
    agencyCompletedRidesCount :: Int,
    tripTerms :: [Text],
    tripCategory :: Maybe TripCategory,
    estimateFareBreakup :: [EstimateBreakupAPIEntity],
    estimatedPickupDuration :: Maybe Seconds,
    nightShiftRate :: Maybe NightShiftRateAPIEntity, -- TODO: doesn't make sense, to be removed
    nightShiftInfo :: Maybe NightShiftInfoAPIEntity,
    businessDiscountInfo :: Maybe BusinessDiscountInfoAPIEntity,
    personalDiscountInfo :: Maybe PersonalDiscountInfoAPIEntity,
    tollChargesInfo :: Maybe TollChargesInfoAPIEntity,
    waitingCharges :: WaitingChargesAPIEntity,
    driversLatLong :: [LatLong],
    tipOptions :: Maybe [Int],
    specialLocationTag :: Maybe Text,
    createdAt :: UTCTime,
    providerName :: Text,
    providerLogoUrl :: Maybe Text,
    providerDescription :: Maybe Text,
    providerId :: Text,
    isValueAddNP :: Bool,
    isCustomerPrefferedSearchRoute :: Maybe Bool,
    isBlockedRoute :: Maybe Bool,
    vehicleServiceTierAirConditioned :: Maybe Double,
    isAirConditioned :: Maybe Bool,
    driverCancellationNotAllowed :: Maybe Bool,
    vehicleServiceTierSeatingCapacity :: Maybe Int,
    vehicleServiceTierLuggageCapacity :: Maybe Int,
    validTill :: UTCTime,
    vehicleIconUrl :: Maybe Text,
    boostSearchPreSelectionServiceTierConfig :: Maybe [DVST.ServiceTierType],
    smartTipSuggestion :: Maybe HighPrecMoney,
    smartTipReason :: Maybe Text,
    negativeFareSuggestion :: Maybe HighPrecMoney,
    isReferredRide :: Bool,
    isInsured :: Maybe Bool,
    insuredAmount :: Maybe Text,
    offer :: Maybe SOffer.CumulativeOfferResp,
    area :: Maybe Text,
    navigationInstruction :: Maybe Text,
    componentGrossMultipliers :: Maybe (FB.RideTaxRates PaymentCharge.GrossMultiplier),
    taxRates :: Maybe Tax.RiderTaxRates
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data NightShiftRateAPIEntity = NightShiftRateAPIEntity
  { nightShiftMultiplier :: Maybe Centesimal, -- TODO: this field works wrong, value in it not always make sense, it have to be removed later
    nightShiftStart :: TimeOfDay,
    nightShiftEnd :: TimeOfDay
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data EstimateBreakupAPIEntity = EstimateBreakupAPIEntity
  { title :: Text,
    price :: Money,
    priceWithCurrency :: PriceAPIEntity
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Build the API entity using already-resolved BppDetails + isValueAddNP.
-- Use this when the caller has a preloaded provider lookup so per-estimate
-- Redis hits (CQBppDetails.findBySubscriberIdAndDomain + QNP.isValueAddNP)
-- can be skipped.
mkEstimateAPIEntity :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Bool -> Maybe SOffer.CumulativeOfferResp -> Maybe PaymentCharge.PaymentChargeDetails -> BppDetails -> Bool -> Estimate -> m EstimateAPIEntity
mkEstimateAPIEntity isReferredRide mbOffer mbPaymentCharge bppDetails valueAddNPRes (Estimate {..}) = do
  let mbBaseFareEB = find (\x -> x.title == show Enums.BASE_FARE) estimateBreakupList
      mbBaseDistanceFareEB = maybeToList $ addBaseDisatanceFareEB mbBaseFareEB -- TODO::Remove it after UI stops consuming it,
      gross fare = mkPrice (Just fare.currency) (roundAmountByCurrency' fare.currency (Settlement.grossTotalTaxInclusive mbPaymentCharge fare.amount))
      grossComponent component charge = mkPrice (Just charge.currency) (roundAmountByCurrency' charge.currency (Settlement.grossTaxExclusive component mbPaymentCharge riderTaxRates charge.amount))
      grossTip = roundAmountByCurrency' estimatedFare.currency . Settlement.grossTaxInclusive mbPaymentCharge
      grossOffer o = let g = Settlement.grossTotalTaxInclusive mbPaymentCharge o.postOfferAmount in o {SOffer.postOfferAmount = g, SOffer.estimatedPostOfferAmount = g}
      offer = (\offers -> (offers {SOffer.offerListResp = map grossOffer offers.offerListResp} :: SOffer.CumulativeOfferResp)) <$> mbOffer
  return
    EstimateAPIEntity
      { agencyName = providerName,
        agencyNumber = providerMobileNumber,
        agencyCompletedRidesCount = providerCompletedRidesCount,
        tripTerms = [],
        estimateFareBreakup = (mkEstimateBreakupAPIEntity <$> (FB.withoutHiddenLines (.title) estimateBreakupList <> mbBaseDistanceFareEB)),
        driversLatLong = driversLocation,
        nightShiftRate = mkNightShiftRateAPIEntity <$> nightShiftInfo,
        providerId = providerId,
        providerName = bppDetails.name,
        providerLogoUrl = bppDetails.logoUrl,
        providerDescription = bppDetails.description,
        isValueAddNP = valueAddNPRes,
        estimatedFare = (gross estimatedFare).amountInt,
        estimatedTotalFare = (gross estimatedTotalFare).amountInt,
        discount = discount <&> (.amountInt),
        estimatedFareWithCurrency = mkPriceAPIEntity (gross estimatedFare),
        estimatedTotalFareWithCurrency = mkPriceAPIEntity (gross estimatedTotalFare),
        discountWithCurrency = mkRoundedPriceAPIEntity <$> discount,
        nightShiftInfo = mkNightShiftInfoAPIEntity <$> nightShiftInfo,
        businessDiscountInfo = mkBusinessDiscountInfoAPIEntity <$> businessDiscountInfo,
        personalDiscountInfo = mkPersonalDiscountInfoAPIEntity <$> personalDiscountInfo,
        tollChargesInfo = (\info -> mkTollChargesInfoAPIEntity TollChargesInfo {tollCharges = grossComponent (.toll) info.tollCharges, tollNames = info.tollNames}) <$> tollChargesInfo,
        waitingCharges = mkWaitingChargesAPIEntity WaitingCharges {waitingChargePerMin = grossComponent (.rideFare) <$> waitingCharges.waitingChargePerMin},
        tipOptions = map (roundToIntegral . grossTip . fromIntegral) <$> tipOptions,
        smartTipSuggestion = grossTip <$> smartTipSuggestion,
        negativeFareSuggestion = grossTip <$> negativeFareSuggestion,
        totalFareRange = mkFareRangeAPIEntity FareRange {minFare = gross totalFareRange.minFare, maxFare = gross totalFareRange.maxFare},
        vehicleVariant = Vehicle.castServiceTierToVariant vehicleServiceTierType,
        serviceTierType = vehicleServiceTierType,
        vehicleIconUrl = showBaseUrl <$> vehicleIconUrl,
        isInsured = Just isInsured,
        componentGrossMultipliers = PaymentCharge.grossMultipliers mbPaymentCharge <$> riderTaxRates,
        taxRates = riderTaxRates,
        ..
      }
  where
    addBaseDisatanceFareEB :: Maybe EstimateBreakup -> Maybe EstimateBreakup
    addBaseDisatanceFareEB = fmap (\baseFareEB -> EstimateBreakup {id = baseFareEB.id, estimateId = baseFareEB.estimateId, title = "BASE_DISTANCE_FARE", price = baseFareEB.price})

mkNightShiftRateAPIEntity :: NightShiftInfo -> NightShiftRateAPIEntity
mkNightShiftRateAPIEntity NightShiftInfo {..} = do
  NightShiftRateAPIEntity
    { nightShiftMultiplier = oldNightShiftCharge,
      ..
    }

mkEstimateBreakupAPIEntity :: EstimateBreakup -> EstimateBreakupAPIEntity
mkEstimateBreakupAPIEntity EstimateBreakup {..} = do
  EstimateBreakupAPIEntity
    { title = title,
      price = price.value.amountInt,
      priceWithCurrency = mkRoundedPriceAPIEntity price.value
    }

data TollChargesInfoAPIEntity = TollChargesInfoAPIEntity
  { tollChargesWithCurrency :: PriceAPIEntity,
    tollNames :: [Text]
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

mkTollChargesInfoAPIEntity :: TollChargesInfo -> TollChargesInfoAPIEntity
mkTollChargesInfoAPIEntity TollChargesInfo {..} = do
  let tollChargesWithCurrency = mkPriceAPIEntity tollCharges
  TollChargesInfoAPIEntity {..}

data NightShiftInfoAPIEntity = NightShiftInfoAPIEntity
  { nightShiftCharge :: Money,
    nightShiftChargeWithCurrency :: PriceAPIEntity,
    oldNightShiftCharge :: Maybe Centesimal, -- TODO: this field works wrong, value in it not always make sense, it have to be removed later
    nightShiftStart :: TimeOfDay,
    nightShiftEnd :: TimeOfDay
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BusinessDiscountInfoAPIEntity = BusinessDiscountInfoAPIEntity
  { businessDiscount :: Money,
    businessDiscountWithCurrency :: PriceAPIEntity,
    businessDiscountPercentage :: Double
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data PersonalDiscountInfoAPIEntity = PersonalDiscountInfoAPIEntity
  { personalDiscount :: Money,
    personalDiscountWithCurrency :: PriceAPIEntity,
    personalDiscountPercentage :: Double
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

mkBusinessDiscountInfoAPIEntity :: BusinessDiscountInfo -> BusinessDiscountInfoAPIEntity
mkBusinessDiscountInfoAPIEntity BusinessDiscountInfo {..} = do
  let businessDiscountWithCurrency = mkRoundedPriceAPIEntity businessDiscount
  BusinessDiscountInfoAPIEntity {businessDiscount = businessDiscount.amountInt, ..}

mkPersonalDiscountInfoAPIEntity :: PersonalDiscountInfo -> PersonalDiscountInfoAPIEntity
mkPersonalDiscountInfoAPIEntity PersonalDiscountInfo {..} = do
  let personalDiscountWithCurrency = mkRoundedPriceAPIEntity personalDiscount
  PersonalDiscountInfoAPIEntity {personalDiscount = personalDiscount.amountInt, ..}

mkNightShiftInfoAPIEntity :: NightShiftInfo -> NightShiftInfoAPIEntity
mkNightShiftInfoAPIEntity NightShiftInfo {..} = do
  let nightShiftChargeWithCurrency = mkRoundedPriceAPIEntity nightShiftCharge
  NightShiftInfoAPIEntity {nightShiftCharge = nightShiftCharge.amountInt, ..}

data WaitingChargesAPIEntity = WaitingChargesAPIEntity
  { waitingChargePerMin :: Maybe HighPrecMoney,
    waitingChargePerMinWithCurrency :: Maybe PriceAPIEntity
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

mkWaitingChargesAPIEntity :: WaitingCharges -> WaitingChargesAPIEntity
mkWaitingChargesAPIEntity WaitingCharges {waitingChargePerMin} =
  WaitingChargesAPIEntity
    { waitingChargePerMin = waitingChargePerMin <&> (.amount),
      waitingChargePerMinWithCurrency = mkPriceAPIEntity <$> waitingChargePerMin
    }

data FareRangeAPIEntity = FareRangeAPIEntity
  { minFare :: Money,
    maxFare :: Money,
    minFareWithCurrency :: PriceAPIEntity,
    maxFareWithCurrency :: PriceAPIEntity
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

mkFareRangeAPIEntity :: FareRange -> FareRangeAPIEntity
mkFareRangeAPIEntity FareRange {..} =
  FareRangeAPIEntity
    { minFare = minFare.amountInt,
      maxFare = maxFare.amountInt,
      minFareWithCurrency = mkPriceAPIEntity minFare,
      maxFareWithCurrency = mkPriceAPIEntity maxFare
    }

isCancelled ::
  EstimateStatus ->
  Bool
isCancelled status = status == CANCELLED || status == DRIVER_QUOTE_CANCELLED
