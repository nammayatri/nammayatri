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
import Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import qualified Domain.Types.ServiceTierType as DVST
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Types.SpecialLocation as SL
import qualified SharedLogic.FareProduct as FareProduct
import qualified Storage.CachedQueries.FarePolicy as QFP
import qualified Storage.CachedQueries.FareProduct as QFareProduct
import Tools.Error
import Tools.Maps

data FarePoliciesProduct = FarePoliciesProduct
  { farePolicies :: [FarePolicyD.FullFarePolicy],
    area :: SL.Area,
    specialLocationTag :: Maybe Text
  }

makeFarePolicyByEstOrQuoteIdKey :: Text -> Text
makeFarePolicyByEstOrQuoteIdKey estOrQuoteId = "CachedQueries:FarePolicy:EstOrQuoteId-" <> estOrQuoteId

getFarePolicyByEstOrQuoteIdWithoutFallback :: (CacheFlow m r) => Text -> m (Maybe FarePolicyD.FullFarePolicy)
getFarePolicyByEstOrQuoteIdWithoutFallback estOrQuoteId = do
  Redis.safeGet (makeFarePolicyByEstOrQuoteIdKey estOrQuoteId) >>= \case
    Nothing -> do
      logWarning $ "Fare Policy Not Found for quote id: " <> estOrQuoteId
      return Nothing
    Just a -> return $ Just $ coerce @(FarePolicyD.FullFarePolicyD 'Unsafe) @FarePolicyD.FullFarePolicy a

getFarePolicyByEstOrQuoteId :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id DMOC.MerchantOperatingCity -> DTC.TripCategory -> DVST.ServiceTierType -> Maybe SL.Area -> Text -> Maybe Text -> Maybe Text -> m FarePolicyD.FullFarePolicy
getFarePolicyByEstOrQuoteId merchantOpCityId tripCategory vehicleServiceTier area estOrQuoteId txnId idName = do
  Redis.safeGet (makeFarePolicyByEstOrQuoteIdKey estOrQuoteId) >>= \case
    Nothing -> do
      logWarning "Old Fare Policy Not Found, Hence using new fare policy."
      getFarePolicy merchantOpCityId tripCategory vehicleServiceTier area txnId idName
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

getFarePolicy :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id DMOC.MerchantOperatingCity -> DTC.TripCategory -> DVST.ServiceTierType -> Maybe SL.Area -> Maybe Text -> Maybe Text -> m FarePolicyD.FullFarePolicy
getFarePolicy merchantOpCityId tripCategory serviceTier Nothing txnId idName = do
  fareProduct <-
    FareProduct.getBoundedFareProduct merchantOpCityId tripCategory serviceTier SL.Default
      |<|>| QFareProduct.findUnboundedByMerchantVariantArea merchantOpCityId tripCategory serviceTier SL.Default
      >>= fromMaybeM NoFareProduct
  farePolicy <- QFP.findById txnId idName fareProduct.farePolicyId >>= fromMaybeM NoFarePolicy
  return $ FarePolicyD.farePolicyToFullFarePolicy fareProduct.merchantId fareProduct.vehicleServiceTier fareProduct.tripCategory farePolicy
getFarePolicy merchantOpCityId tripCategory serviceTier (Just area) txnId idName = do
  mbFareProduct <-
    FareProduct.getBoundedFareProduct merchantOpCityId tripCategory serviceTier area
      |<|>| QFareProduct.findUnboundedByMerchantVariantArea merchantOpCityId tripCategory serviceTier area
  case mbFareProduct of
    Just fareProduct -> do
      farePolicy <- QFP.findById txnId idName fareProduct.farePolicyId >>= fromMaybeM NoFarePolicy
      return $ FarePolicyD.farePolicyToFullFarePolicy fareProduct.merchantId fareProduct.vehicleServiceTier fareProduct.tripCategory farePolicy
    Nothing -> do
      fareProduct <-
        FareProduct.getBoundedFareProduct merchantOpCityId tripCategory serviceTier SL.Default
          |<|>| QFareProduct.findUnboundedByMerchantVariantArea merchantOpCityId tripCategory serviceTier SL.Default
          >>= fromMaybeM NoFareProduct
      farePolicy <- QFP.findById txnId idName fareProduct.farePolicyId >>= fromMaybeM NoFarePolicy
      return $ FarePolicyD.farePolicyToFullFarePolicy fareProduct.merchantId fareProduct.vehicleServiceTier fareProduct.tripCategory farePolicy

getAllFarePoliciesProduct :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Merchant -> Id DMOC.MerchantOperatingCity -> LatLong -> Maybe LatLong -> Maybe Text -> Maybe Text -> DTC.TripCategory -> m FarePoliciesProduct
getAllFarePoliciesProduct merchantId merchantOpCityId fromlocaton mbToLocation txnId idName tripCategory = do
  allFareProducts <- FareProduct.getAllFareProducts merchantId merchantOpCityId fromlocaton mbToLocation tripCategory
  farePolicies <-
    mapM
      ( \fareProduct -> do
          farePolicy <- QFP.findById txnId idName fareProduct.farePolicyId >>= fromMaybeM NoFarePolicy
          return $ FarePolicyD.farePolicyToFullFarePolicy fareProduct.merchantId fareProduct.vehicleServiceTier fareProduct.tripCategory farePolicy
      )
      allFareProducts.fareProducts
  return $
    FarePoliciesProduct
      { farePolicies,
        area = allFareProducts.area,
        specialLocationTag = allFareProducts.specialLocationTag
      }

mkFarePolicyBreakups :: (Text -> breakupItemValue) -> (Text -> breakupItemValue -> breakupItem) -> Maybe Meters -> Maybe HighPrecMoney -> FarePolicyD.FarePolicy -> [breakupItem]
mkFarePolicyBreakups mkValue mkBreakupItem mbDistance mbTollCharges farePolicy = do
  let distance = fromMaybe 0 mbDistance -- TODO: Fix Later
      driverExtraFeeBounds = FarePolicyD.findDriverExtraFeeBoundsByDistance distance <$> farePolicy.driverExtraFeeBounds
      nightShiftBounds = farePolicy.nightShiftBounds

      tollChargesCaption = show Tags.TOLL_CHARGES
      tollChargesItem = mkBreakupItem tollChargesCaption . (mkValue . show) <$> mbTollCharges

      congestionChargeMultiplierCaption = show Tags.CONGESTION_CHARGE_MULTIPLIER
      congestionChargeMultiplierItem = mkBreakupItem congestionChargeMultiplierCaption . (mkValue . show) <$> farePolicy.congestionChargeMultiplier

      parkingChargeCaption = show Tags.PARKING_CHARGE
      parkingChargeItem = mkBreakupItem parkingChargeCaption . (mkValue . show) <$> farePolicy.parkingCharge

      serviceChargeCaption = show Tags.SERVICE_CHARGE
      serviceChargeItem = mkBreakupItem serviceChargeCaption . (mkValue . show . (.getMoney)) <$> farePolicy.serviceCharge

      governmentChargeCaption = show Tags.GOVERNMENT_CHARGE
      governmentChargeItem = mkBreakupItem governmentChargeCaption . (mkValue . show) <$> farePolicy.govtCharges

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
    [ tollChargesItem,
      serviceChargeItem,
      parkingChargeItem,
      governmentChargeItem,
      driverMinExtraFeeItem,
      driverMaxExtraFeeItem,
      nightShiftStartItem,
      nightShiftEndItem,
      congestionChargeMultiplierItem
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

          perExtraKmStepFareItems = mkPerExtraKmStepFareItem [] (toList perExtraKmFareSections) det.baseDistance.getMeters

          baseDistanceCaption = show Tags.BASE_DISTANCE
          baseDistanceBreakup = mkBreakupItem baseDistanceCaption (mkValue $ show det.baseDistance)

          baseFareCaption = show Tags.BASE_FARE
          baseFareBreakup = mkBreakupItem baseFareCaption (mkValue $ show det.baseFare)

          pickupChargeCaption = show Tags.DEAD_KILOMETER_FARE
          pickupChargeBreakup = mkBreakupItem pickupChargeCaption (mkValue $ show det.deadKmFare)

      [baseDistanceBreakup, baseFareBreakup, pickupChargeBreakup]
        <> perExtraKmFareItems
        <> perExtraKmStepFareItems
        <> (waitingChargeBreakups det.waitingChargeInfo)
        <> (nightShiftChargeBreakups det.nightShiftCharge)
      where
        mkPerExtraKmStepFareItem perExtraKmStepFareItems [] _ = perExtraKmStepFareItems
        mkPerExtraKmStepFareItem perExtraKmStepFareItems [s1] baseDistance = do
          let startDistance = s1.startDistance.getMeters + baseDistance
              perExtraKmStepFareCaption = show $ Tags.EXTRA_PER_KM_STEP_FARE startDistance Nothing
              perExtraKmStepFareItem = mkBreakupItem perExtraKmStepFareCaption (mkValue $ show (round s1.perExtraKmRate :: Money))
          perExtraKmStepFareItems <> [perExtraKmStepFareItem]
        mkPerExtraKmStepFareItem perExtraKmStepFareItems (s1 : s2 : ss) baseDistance = do
          let startDistance = s1.startDistance.getMeters + baseDistance
              endDistance = s2.startDistance.getMeters + baseDistance
              perExtraKmStepFareCaption = show $ Tags.EXTRA_PER_KM_STEP_FARE startDistance (Just endDistance)
              perExtraKmStepFareItem = mkBreakupItem perExtraKmStepFareCaption (mkValue $ show (round s1.perExtraKmRate :: Money))
          mkPerExtraKmStepFareItem (perExtraKmStepFareItems <> [perExtraKmStepFareItem]) (s2 : ss) baseDistance

    mkAdditionalSlabBreakups det = do
      let baseDistanceCaption = show Tags.BASE_DISTANCE
          baseDistanceBreakup = mkBreakupItem baseDistanceCaption (mkValue $ show det.startDistance)

          baseFareCaption = show Tags.BASE_FARE
          baseFareBreakup = mkBreakupItem baseFareCaption (mkValue $ show det.baseFare)

      [baseDistanceBreakup, baseFareBreakup]
        <> (platformFeeBreakups det.platformFeeInfo)
        <> (waitingChargeBreakups det.waitingChargeInfo)
        <> (nightShiftChargeBreakups det.nightShiftCharge)

    platformFeeBreakups Nothing = []
    platformFeeBreakups (Just platformFeeInfo) = do
      let (mbProgressivePlatformFee, mbConstantPlatformFee) =
            platformFeeInfo.platformFeeCharge & \case
              FarePolicyD.ProgressivePlatformFee ppf -> (Just $ show ppf, Nothing)
              FarePolicyD.ConstantPlatformFee cpf -> (Nothing, Just $ show cpf)

          mbProgressivePlatformFeeCaption = show Tags.PROGRESSIVE_PLATFORM_CHARGE
          mbProgressivePlatformFeeItem = mkBreakupItem mbProgressivePlatformFeeCaption . mkValue <$> mbProgressivePlatformFee

          mbConstantPlatformFeeCation = show Tags.CONSTANT_PLATFORM_CHARGE
          mbConstantPlatformFeeItem = mkBreakupItem mbConstantPlatformFeeCation . mkValue <$> mbConstantPlatformFee

          mbPlatformFeeCgstCation = show Tags.PLATFORM_FEE_CGST
          mbPlatformFeeCgstItem = mkBreakupItem mbPlatformFeeCgstCation (mkValue $ show platformFeeInfo.cgst)

          mbPlatformFeeSgstCation = show Tags.PLATFORM_FEE_SGST
          mbPlatformFeeSgstItem = mkBreakupItem mbPlatformFeeSgstCation (mkValue $ show platformFeeInfo.sgst)

      catMaybes [mbProgressivePlatformFeeItem, mbConstantPlatformFeeItem, Just mbPlatformFeeCgstItem, Just mbPlatformFeeSgstItem]

    waitingChargeBreakups Nothing = []
    waitingChargeBreakups (Just waitingChargeInfo) = do
      let (mbWaitingChargePerMin, mbConstantWaitingCharges) =
            waitingChargeInfo.waitingCharge & \case
              DPM.PerMinuteWaitingCharge hpm -> (Just $ show (round hpm :: Money), Nothing)
              DPM.ConstantWaitingCharge mo -> (Nothing, Just $ show mo)

          waitingOrPickupChargesCaption = show Tags.WAITING_OR_PICKUP_CHARGES -- TODO :: To be deprecated
          mbWaitingOrPickupChargesItem = mkBreakupItem waitingOrPickupChargesCaption . mkValue <$> mbConstantWaitingCharges

          mbWaitingChargePerMinCaption = show Tags.WAITING_CHARGE_PER_MIN
          mbWaitingChargePerMinItem = mkBreakupItem mbWaitingChargePerMinCaption . mkValue <$> mbWaitingChargePerMin

          mbConstantWaitingChargeCaption = show Tags.CONSTANT_WAITING_CHARGE
          mbConstantWaitingChargeItem = mkBreakupItem mbConstantWaitingChargeCaption . mkValue <$> mbConstantWaitingCharges

          freeWaitingTimeCation = show Tags.FREE_WAITING_TIME_IN_MINUTES
          freeWaitingTimeItem = mkBreakupItem freeWaitingTimeCation . mkValue . show $ waitingChargeInfo.freeWaitingTime.getMinutes

      catMaybes [mbWaitingOrPickupChargesItem, mbWaitingChargePerMinItem, mbConstantWaitingChargeItem, Just freeWaitingTimeItem]

    nightShiftChargeBreakups nightShiftChargeInfo = do
      let getNightShiftChargeValue (DPM.ProgressiveNightShiftCharge a) = show (round a :: Money) -- fix from customer side first
          getNightShiftChargeValue (DPM.ConstantNightShiftCharge a) = show a

      let oldNightShiftCharge = getNightShiftChargeValue <$> nightShiftChargeInfo
          oldNightShiftChargeCaption = show Tags.NIGHT_SHIFT_CHARGE
          oldNightShiftChargeItem = mkBreakupItem oldNightShiftChargeCaption . mkValue <$> oldNightShiftCharge

      catMaybes [oldNightShiftChargeItem]
