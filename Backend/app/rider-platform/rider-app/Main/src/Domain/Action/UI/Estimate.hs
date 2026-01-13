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
import Data.Aeson
import Domain.Types.BppDetails
import Domain.Types.Estimate
import Domain.Types.EstimateStatus
import qualified Domain.Types.ServiceTierType as DVST
import Domain.Types.Trip (TripCategory)
import qualified Domain.Types.VehicleVariant as Vehicle
import Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.BppDetails as CQBppDetails
import qualified Storage.CachedQueries.ValueAddNP as QNP
import Tools.Error

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
    vehicleServiceTierSeatingCapacity :: Maybe Int,
    validTill :: UTCTime,
    vehicleIconUrl :: Maybe Text,
    boostSearchPreSelectionServiceTierConfig :: Maybe [DVST.ServiceTierType],
    smartTipSuggestion :: Maybe HighPrecMoney,
    smartTipReason :: Maybe Text,
    isReferredRide :: Bool,
    isInsured :: Maybe Bool,
    estimateTags :: Maybe [Text],
    insuredAmount :: Maybe Text
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

mkEstimateAPIEntity :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Bool -> Estimate -> m EstimateAPIEntity
mkEstimateAPIEntity isReferredRide (Estimate {..}) = do
  valueAddNPRes <- QNP.isValueAddNP providerId
  (bppDetails :: BppDetails) <- CQBppDetails.findBySubscriberIdAndDomain providerId Context.MOBILITY >>= fromMaybeM (InternalError $ "BppDetails not found " <> providerId)
  let mbBaseFareEB = find (\x -> x.title == show Enums.BASE_FARE) estimateBreakupList
      mbBaseDistanceFareEB = maybeToList $ addBaseDisatanceFareEB mbBaseFareEB -- TODO::Remove it after UI stops consuming it,
  return
    EstimateAPIEntity
      { agencyName = providerName,
        agencyNumber = providerMobileNumber,
        agencyCompletedRidesCount = providerCompletedRidesCount,
        tripTerms = fromMaybe [] $ tripTerms <&> (.descriptions),
        estimateFareBreakup = mkEstimateBreakupAPIEntity <$> (estimateBreakupList <> mbBaseDistanceFareEB),
        driversLatLong = driversLocation,
        nightShiftRate = mkNightShiftRateAPIEntity <$> nightShiftInfo,
        providerId = providerId,
        providerName = bppDetails.name,
        providerLogoUrl = bppDetails.logoUrl,
        providerDescription = bppDetails.description,
        isValueAddNP = valueAddNPRes,
        estimatedFare = estimatedFare.amountInt,
        estimatedTotalFare = estimatedTotalFare.amountInt,
        discount = discount <&> (.amountInt),
        estimatedFareWithCurrency = mkPriceAPIEntity estimatedFare,
        estimatedTotalFareWithCurrency = mkPriceAPIEntity estimatedTotalFare,
        discountWithCurrency = mkPriceAPIEntity <$> discount,
        nightShiftInfo = mkNightShiftInfoAPIEntity <$> nightShiftInfo,
        businessDiscountInfo = mkBusinessDiscountInfoAPIEntity <$> businessDiscountInfo,
        personalDiscountInfo = mkPersonalDiscountInfoAPIEntity <$> personalDiscountInfo,
        tollChargesInfo = mkTollChargesInfoAPIEntity <$> tollChargesInfo,
        waitingCharges = mkWaitingChargesAPIEntity waitingCharges,
        totalFareRange = mkFareRangeAPIEntity totalFareRange,
        vehicleVariant = Vehicle.castServiceTierToVariant vehicleServiceTierType,
        serviceTierType = vehicleServiceTierType,
        vehicleIconUrl = showBaseUrl <$> vehicleIconUrl,
        isInsured = Just isInsured,
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
      priceWithCurrency = mkPriceAPIEntity price.value
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
  let businessDiscountWithCurrency = mkPriceAPIEntity businessDiscount
  BusinessDiscountInfoAPIEntity {businessDiscount = businessDiscount.amountInt, ..}

mkPersonalDiscountInfoAPIEntity :: PersonalDiscountInfo -> PersonalDiscountInfoAPIEntity
mkPersonalDiscountInfoAPIEntity PersonalDiscountInfo {..} = do
  let personalDiscountWithCurrency = mkPriceAPIEntity personalDiscount
  PersonalDiscountInfoAPIEntity {personalDiscount = personalDiscount.amountInt, ..}

mkNightShiftInfoAPIEntity :: NightShiftInfo -> NightShiftInfoAPIEntity
mkNightShiftInfoAPIEntity NightShiftInfo {..} = do
  let nightShiftChargeWithCurrency = mkPriceAPIEntity nightShiftCharge
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
