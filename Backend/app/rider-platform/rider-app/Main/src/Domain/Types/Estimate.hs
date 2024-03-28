{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.Estimate where

import qualified BecknV2.OnDemand.Enums as Enums
import Data.Aeson
import Domain.Types.BppDetails
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.SearchRequest as DSearchRequest
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleVariant (VehicleVariant)
import Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.GenericPretty
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import qualified Storage.CachedQueries.BppDetails as CQBppDetails
import qualified Storage.CachedQueries.ValueAddNP as QNP
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)
import Tools.Error

data Estimate = Estimate
  { id :: Id Estimate,
    requestId :: Id DSearchRequest.SearchRequest,
    merchantId :: Maybe (Id DM.Merchant),
    merchantOperatingCityId :: Maybe (Id DMOC.MerchantOperatingCity),
    bppEstimateId :: Id BPPEstimate,
    estimatedFare :: Price,
    discount :: Maybe Price,
    estimatedTotalFare :: Price,
    totalFareRange :: FareRange,
    estimatedDuration :: Maybe Seconds,
    estimatedDistance :: Maybe HighPrecMeters,
    device :: Maybe Text,
    providerId :: Text,
    providerUrl :: BaseUrl,
    providerName :: Text,
    providerMobileNumber :: Text,
    providerCompletedRidesCount :: Int,
    vehicleVariant :: VehicleVariant,
    itemId :: Text,
    tripTerms :: Maybe DTripTerms.TripTerms,
    estimateBreakupList :: [EstimateBreakup],
    nightShiftInfo :: Maybe NightShiftInfo,
    status :: EstimateStatus,
    waitingCharges :: WaitingCharges,
    driversLocation :: [LatLong],
    specialLocationTag :: Maybe Text,
    updatedAt :: UTCTime,
    createdAt :: UTCTime,
    validTill :: UTCTime,
    serviceTierName :: Maybe Text
  }
  deriving (Generic, Show)

data BPPEstimate

data NightShiftInfo = NightShiftInfo
  { nightShiftCharge :: Price,
    oldNightShiftCharge :: Maybe Centesimal, -- TODO: this field works wrong, value in it not always make sense, it have to be removed later
    nightShiftStart :: TimeOfDay,
    nightShiftEnd :: TimeOfDay
  }
  deriving (Generic, Show)

data NightShiftInfoAPIEntity = NightShiftInfoAPIEntity
  { nightShiftCharge :: Money,
    nightShiftChargeWithCurrency :: PriceAPIEntity,
    oldNightShiftCharge :: Maybe Centesimal, -- TODO: this field works wrong, value in it not always make sense, it have to be removed later
    nightShiftStart :: TimeOfDay,
    nightShiftEnd :: TimeOfDay
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data EstimateBreakup = EstimateBreakup
  { id :: Id EstimateBreakup,
    estimateId :: Id Estimate,
    title :: Text,
    price :: EstimateBreakupPrice
  }
  deriving (Generic, Show, PrettyShow)

newtype EstimateBreakupPrice = EstimateBreakupPrice
  { value :: Price
  }
  deriving (Generic, Show, PrettyShow)

data EstimateBreakupPriceAPIEntity = EstimateBreakupPriceAPIEntity
  { currency :: Currency,
    value :: Money
  }
  deriving (Generic, FromJSON, ToJSON, Show, PrettyShow, ToSchema)

data FareRange = FareRange
  { minFare :: Price,
    maxFare :: Price
  }
  deriving (Generic, Show, PrettyShow)

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

newtype WaitingCharges = WaitingCharges
  { waitingChargePerMin :: Maybe Price
  }
  deriving (Generic, Show)

data WaitingChargesAPIEntity = WaitingChargesAPIEntity
  { waitingChargePerMin :: Maybe Money,
    waitingChargePerMinWithCurrency :: Maybe PriceAPIEntity
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

mkWaitingChargesAPIEntity :: WaitingCharges -> WaitingChargesAPIEntity
mkWaitingChargesAPIEntity WaitingCharges {waitingChargePerMin} =
  WaitingChargesAPIEntity
    { waitingChargePerMin = waitingChargePerMin <&> (.amountInt),
      waitingChargePerMinWithCurrency = mkPriceAPIEntity <$> waitingChargePerMin
    }

data EstimateAPIEntity = EstimateAPIEntity
  { id :: Id Estimate,
    vehicleVariant :: VehicleVariant,
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
    estimateFareBreakup :: [EstimateBreakupAPIEntity],
    nightShiftRate :: Maybe NightShiftRateAPIEntity, -- TODO: doesn't make sense, to be removed
    nightShiftInfo :: Maybe NightShiftInfoAPIEntity,
    waitingCharges :: WaitingChargesAPIEntity,
    driversLatLong :: [LatLong],
    specialLocationTag :: Maybe Text,
    createdAt :: UTCTime,
    providerName :: Text,
    providerLogoUrl :: Maybe Text,
    providerDescription :: Maybe Text,
    providerId :: Text,
    isValueAddNP :: Bool,
    validTill :: UTCTime,
    serviceTierName :: Maybe Text
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

mkNightShiftInfoAPIEntity :: NightShiftInfo -> NightShiftInfoAPIEntity
mkNightShiftInfoAPIEntity NightShiftInfo {..} = do
  let nightShiftChargeWithCurrency = mkPriceAPIEntity nightShiftCharge
  NightShiftInfoAPIEntity {nightShiftCharge = nightShiftCharge.amountInt, ..}

mkEstimateAPIEntity :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Estimate -> m EstimateAPIEntity
mkEstimateAPIEntity Estimate {..} = do
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
        waitingCharges = mkWaitingChargesAPIEntity waitingCharges,
        totalFareRange = mkFareRangeAPIEntity totalFareRange,
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

data EstimateStatus = NEW | DRIVER_QUOTE_REQUESTED | CANCELLED | GOT_DRIVER_QUOTE | DRIVER_QUOTE_CANCELLED | COMPLETED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''EstimateStatus)

$(mkHttpInstancesForEnum ''EstimateStatus)

isCancelled ::
  EstimateStatus ->
  Bool
isCancelled status = status == CANCELLED || status == DRIVER_QUOTE_CANCELLED
