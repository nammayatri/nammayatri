{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.FarePolicy where

import BecknV2.OnDemand.Tags as Tags
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Merchant as DPM
import Data.Coerce (coerce)
import qualified Data.List.NonEmpty as NE
import Data.Ord (comparing)
import Data.Text as T hiding (find)
import Domain.Types.Common (UsageSafety (..))
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.FarePolicy as FarePolicyD
import qualified Domain.Types.FareProduct as FareProductD
import Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Vehicle.Variant (Variant (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.FareProduct as FareProduct
import qualified Storage.CachedQueries.FarePolicy as QFP
import qualified Storage.CachedQueries.FareProduct as QFareProduct
import Tools.Error
import Tools.Maps

data FarePoliciesProduct = FarePoliciesProduct
  { farePolicies :: [FarePolicyD.FullFarePolicy],
    area :: FareProductD.Area,
    specialLocationTag :: Maybe Text
  }

makeFarePolicyByEstOrQuoteIdKey :: Text -> Text
makeFarePolicyByEstOrQuoteIdKey estOrQuoteId = "CachedQueries:FarePolicy:EstOrQuoteId-" <> estOrQuoteId

getFarePolicyByEstOrQuoteIdWithoutFallback :: (CacheFlow m r) => Text -> m (Maybe FarePolicyD.FullFarePolicy)
getFarePolicyByEstOrQuoteIdWithoutFallback estOrQuoteId = do
  Redis.get (makeFarePolicyByEstOrQuoteIdKey estOrQuoteId) >>= \case
    Nothing -> do
      logWarning $ "Fare Policy Not Found for quote id: " <> estOrQuoteId
      return Nothing
    Just a -> return $ Just $ coerce @(FarePolicyD.FullFarePolicyD 'Unsafe) @FarePolicyD.FullFarePolicy a

getFarePolicyByEstOrQuoteId :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id DMOC.MerchantOperatingCity -> DTC.TripCategory -> Variant -> Maybe FareProductD.Area -> Text -> Maybe Text -> m FarePolicyD.FullFarePolicy
getFarePolicyByEstOrQuoteId merchantOpCityId tripCategory vehVariant area estOrQuoteId txnId = do
  Redis.get (makeFarePolicyByEstOrQuoteIdKey estOrQuoteId) >>= \case
    Nothing -> do
      logWarning "Old Fare Policy Not Found, Hence using new fare policy."
      getFarePolicy merchantOpCityId tripCategory vehVariant area txnId
    Just a -> return $ coerce @(FarePolicyD.FullFarePolicyD 'Unsafe) @FarePolicyD.FullFarePolicy a

cacheFarePolicyByQuoteId :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Text -> FarePolicyD.FullFarePolicy -> m ()
cacheFarePolicyByQuoteId quoteId fp = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Redis.setExp (makeFarePolicyByEstOrQuoteIdKey quoteId) (coerce @FarePolicyD.FullFarePolicy @(FarePolicyD.FullFarePolicyD 'Unsafe) fp) expTime

-- 30 Mins, Assuming that all searchTries would be done by then. Correct logic would be searchRequestExpirationTime * searchRepeatLimit
cacheFarePolicyByEstimateId :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Text -> FarePolicyD.FullFarePolicy -> m ()
cacheFarePolicyByEstimateId estimateId fp = Redis.setExp (makeFarePolicyByEstOrQuoteIdKey estimateId) (coerce @FarePolicyD.FullFarePolicy @(FarePolicyD.FullFarePolicyD 'Unsafe) fp) 1800

clearCachedFarePolicyByEstOrQuoteId :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Text -> m ()
clearCachedFarePolicyByEstOrQuoteId = Redis.del . makeFarePolicyByEstOrQuoteIdKey

getFarePolicy :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id DMOC.MerchantOperatingCity -> DTC.TripCategory -> Variant -> Maybe FareProductD.Area -> Maybe Text -> m FarePolicyD.FullFarePolicy
getFarePolicy merchantOpCityId tripCategory vehVariant Nothing txnId = do
  fareProduct <-
    FareProduct.getBoundedFareProduct merchantOpCityId tripCategory vehVariant FareProductD.Default
      |<|>| QFareProduct.findUnboundedByMerchantVariantArea merchantOpCityId tripCategory vehVariant FareProductD.Default
      >>= fromMaybeM NoFareProduct
  farePolicy <- QFP.findById txnId fareProduct.farePolicyId >>= fromMaybeM NoFarePolicy
  return $ FarePolicyD.farePolicyToFullFarePolicy fareProduct.merchantId fareProduct.vehicleVariant fareProduct.tripCategory farePolicy
getFarePolicy merchantOpCityId tripCategory vehVariant (Just area) txnId = do
  mbFareProduct <-
    FareProduct.getBoundedFareProduct merchantOpCityId tripCategory vehVariant area
      |<|>| QFareProduct.findUnboundedByMerchantVariantArea merchantOpCityId tripCategory vehVariant area
  case mbFareProduct of
    Just fareProduct -> do
      farePolicy <- QFP.findById txnId fareProduct.farePolicyId >>= fromMaybeM NoFarePolicy
      return $ FarePolicyD.farePolicyToFullFarePolicy fareProduct.merchantId fareProduct.vehicleVariant fareProduct.tripCategory farePolicy
    Nothing -> do
      fareProduct <-
        FareProduct.getBoundedFareProduct merchantOpCityId tripCategory vehVariant FareProductD.Default
          |<|>| QFareProduct.findUnboundedByMerchantVariantArea merchantOpCityId tripCategory vehVariant FareProductD.Default
          >>= fromMaybeM NoFareProduct
      farePolicy <- QFP.findById txnId fareProduct.farePolicyId >>= fromMaybeM NoFarePolicy
      return $ FarePolicyD.farePolicyToFullFarePolicy fareProduct.merchantId fareProduct.vehicleVariant fareProduct.tripCategory farePolicy

getAllFarePoliciesProduct :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Merchant -> Id DMOC.MerchantOperatingCity -> LatLong -> Maybe LatLong -> Maybe Text -> DTC.TripCategory -> m FarePoliciesProduct
getAllFarePoliciesProduct merchantId merchantOpCityId fromlocaton mbToLocation txnId tripCategory = do
  allFareProducts <- FareProduct.getAllFareProducts merchantId merchantOpCityId fromlocaton mbToLocation tripCategory
  farePolicies <-
    mapM
      ( \fareProduct -> do
          farePolicy <- QFP.findById txnId fareProduct.farePolicyId >>= fromMaybeM NoFarePolicy
          return $ FarePolicyD.farePolicyToFullFarePolicy fareProduct.merchantId fareProduct.vehicleVariant fareProduct.tripCategory farePolicy
      )
      allFareProducts.fareProducts
  return $
    FarePoliciesProduct
      { farePolicies,
        area = allFareProducts.area,
        specialLocationTag = allFareProducts.specialLocationTag
      }

mkFarePolicyBreakups :: (Text -> breakupItemValue) -> (Text -> breakupItemValue -> breakupItem) -> Maybe Meters -> FarePolicyD.FarePolicy -> [breakupItem]
mkFarePolicyBreakups mkValue mkBreakupItem mbDistance farePolicy = do
  let distance = fromMaybe 0 mbDistance -- TODO: Fix Later
      driverExtraFeeBounds = FarePolicyD.findDriverExtraFeeBoundsByDistance distance <$> farePolicy.driverExtraFeeBounds
      nightShiftBounds = farePolicy.nightShiftBounds

      driverMinExtraFee = driverExtraFeeBounds <&> (.minFee)
      driverMinExtraFeeCaption = show Tags.DRIVER_MIN_EXTRA_FEE
      driverMinExtraFeeItem = mkBreakupItem driverMinExtraFeeCaption . (mkValue . show . (.getMoney)) <$> driverMinExtraFee

      driverMaxExtraFee = driverExtraFeeBounds <&> (.maxFee)
      driverMaxExtraFeeCaption = show Tags.DRIVER_MAX_EXTRA_FEE
      driverMaxExtraFeeItem = mkBreakupItem driverMaxExtraFeeCaption . (mkValue . show . (.getMoney)) <$> driverMaxExtraFee

      nightShiftStart = nightShiftBounds <&> (.nightShiftStart)
      nightShiftStartCaption = show Tags.NIGHT_SHIFT_START_TIME
      nightShiftStartItem = mkBreakupItem nightShiftStartCaption . (mkValue . show) <$> nightShiftStart

      nightShiftEnd = nightShiftBounds <&> (.nightShiftEnd)
      nightShiftEndCaption = show Tags.NIGHT_SHIFT_END_TIME
      nightShiftEndItem = mkBreakupItem nightShiftEndCaption . (mkValue . show) <$> nightShiftEnd

      additionalDetailsBreakups = processAdditionalDetails farePolicy.farePolicyDetails
  catMaybes
    [ driverMinExtraFeeItem,
      driverMaxExtraFeeItem,
      nightShiftStartItem,
      nightShiftEndItem
    ]
    <> additionalDetailsBreakups
  where
    processAdditionalDetails = \case
      FarePolicyD.ProgressiveDetails det -> mkAdditionalProgressiveBreakups det
      FarePolicyD.SlabsDetails det -> mkAdditionalSlabBreakups $ FarePolicyD.findFPSlabsDetailsSlabByDistance (fromMaybe 0 mbDistance) det.slabs
      FarePolicyD.RentalDetails det -> mkAdditionalRentalBreakups det

    mkAdditionalRentalBreakups det = do
      let minFareCaption = show Tags.MIN_FARE
          minFareItem = mkBreakupItem minFareCaption . mkValue $ show det.baseFare

          perHourChargeCaption = show Tags.PER_HOUR_CHARGE
          perHourChargeItem = mkBreakupItem perHourChargeCaption . mkValue $ show det.perHourCharge

          perExtraMinRateCaption = show Tags.PER_MINUTE_CHARGE
          perExtraMinRateItem = mkBreakupItem perExtraMinRateCaption . mkValue $ show det.perExtraMinRate

          perExtraKmRateCaption = show Tags.UNPLANNED_PER_KM_CHARGE
          perExtraKmRateItem = mkBreakupItem perExtraKmRateCaption . mkValue $ show det.perExtraKmRate

          includedKmPerHrCaption = show Tags.PER_HOUR_DISTANCE_KM
          includedKmPerHrItem = mkBreakupItem includedKmPerHrCaption . mkValue $ show det.includedKmPerHr

          plannedPerKmRateCaption = show Tags.PLANNED_PER_KM_CHARGE
          plannedPerKmRateItem = mkBreakupItem plannedPerKmRateCaption . mkValue $ show det.plannedPerKmRate

      [minFareItem, perHourChargeItem, perExtraMinRateItem, perExtraKmRateItem, includedKmPerHrItem, plannedPerKmRateItem]
        <> (nightShiftChargeBreakups det.nightShiftCharge)

    mkAdditionalProgressiveBreakups det = do
      let perExtraKmFareSections = NE.sortBy (comparing (.startDistance)) det.perExtraKmRateSections
          mkPerExtraKmFareItem section = do
            let perExtraKmFareCaption = show Tags.EXTRA_PER_KM_FARE
            mkBreakupItem perExtraKmFareCaption (mkValue $ show (round section.perExtraKmRate :: Money))

          perExtraKmFareItems = mkPerExtraKmFareItem <$> (toList perExtraKmFareSections)

      perExtraKmFareItems
        <> (waitingChargeBreakups det.waitingChargeInfo)
        <> (nightShiftChargeBreakups det.nightShiftCharge)

    mkAdditionalSlabBreakups det = do
      (waitingChargeBreakups det.waitingChargeInfo)
        <> (nightShiftChargeBreakups det.nightShiftCharge)

    waitingChargeBreakups Nothing = []
    waitingChargeBreakups (Just waitingChargeInfo) = do
      let (mbWaitingChargePerMin, mbWaitingOrPickupCharges) =
            waitingChargeInfo.waitingCharge & \case
              DPM.PerMinuteWaitingCharge hpm -> (Just $ show (round hpm :: Money), Nothing)
              DPM.ConstantWaitingCharge mo -> (Nothing, Just $ show mo)

          waitingOrPickupChargesCaption = show Tags.WAITING_OR_PICKUP_CHARGES
          mbWaitingOrPickupChargesItem = mkBreakupItem waitingOrPickupChargesCaption . mkValue <$> mbWaitingOrPickupCharges

          mbWaitingChargePerMinCaption = show Tags.WAITING_CHARGE_PER_MIN
          mbWaitingChargePerMinItem = mkBreakupItem mbWaitingChargePerMinCaption . mkValue <$> mbWaitingChargePerMin

      catMaybes [mbWaitingOrPickupChargesItem, mbWaitingChargePerMinItem]

    nightShiftChargeBreakups nightShiftChargeInfo = do
      let getNightShiftChargeValue (DPM.ProgressiveNightShiftCharge a) = show (round a :: Money) -- fix from customer side first
          getNightShiftChargeValue (DPM.ConstantNightShiftCharge a) = show a

      let oldNightShiftCharge = getNightShiftChargeValue <$> nightShiftChargeInfo
          oldNightShiftChargeCaption = show Tags.NIGHT_SHIFT_CHARGE
          oldNightShiftChargeItem = mkBreakupItem oldNightShiftChargeCaption . mkValue <$> oldNightShiftCharge

      catMaybes [oldNightShiftChargeItem]
