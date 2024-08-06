{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module SharedLogic.FarePolicy where

import BecknV2.OnDemand.Tags as Tags
import Data.Coerce (coerce)
import qualified Data.Geohash as Geohash
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Ord (comparing)
import Data.Text as T hiding (find)
import Data.Time hiding (getCurrentTime)
import Data.Time.Calendar.WeekDate
import Domain.Types.Common (UsageSafety (..))
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.FarePolicy as FarePolicyD
import qualified Domain.Types.FareProduct as FareProduct
import Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.ServiceTierType
import qualified Domain.Types.ServiceTierType as DVST
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Types.SpecialLocation as SL
import qualified SharedLogic.FareProduct as FareProduct
import qualified Storage.Cac.FarePolicy as QFP
import qualified Storage.CachedQueries.CancellationFarePolicy as QCCFP
import qualified Storage.CachedQueries.FareProduct as QFareProduct
import qualified Storage.CachedQueries.SurgePricing as SurgePricing
import Tools.Error
import Tools.Maps
import Utils.Common.Cac.KeyNameConstants

data FarePoliciesProduct = FarePoliciesProduct
  { farePolicies :: [FarePolicyD.FullFarePolicy],
    area :: SL.Area,
    specialLocationTag :: Maybe Text,
    specialLocationName :: Maybe Text
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

getFarePolicyByEstOrQuoteId :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Maybe LatLong -> Id DMOC.MerchantOperatingCity -> DTC.TripCategory -> DVST.ServiceTierType -> Maybe SL.Area -> Text -> Maybe Bool -> Maybe CacKey -> m FarePolicyD.FullFarePolicy
getFarePolicyByEstOrQuoteId mbFromlocaton merchantOpCityId tripCategory vehicleServiceTier area estOrQuoteId isDashboardRequest txnId = do
  Redis.safeGet (makeFarePolicyByEstOrQuoteIdKey estOrQuoteId) >>= \case
    Nothing -> do
      logWarning "Old Fare Policy Not Found, Hence using new fare policy."
      getFarePolicy mbFromlocaton merchantOpCityId (fromMaybe False isDashboardRequest) tripCategory vehicleServiceTier area txnId
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

getFarePolicy :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Maybe LatLong -> Id DMOC.MerchantOperatingCity -> Bool -> DTC.TripCategory -> DVST.ServiceTierType -> Maybe SL.Area -> Maybe CacKey -> m FarePolicyD.FullFarePolicy
getFarePolicy mbFromlocaton merchantOpCityId isDashboard tripCategory serviceTier Nothing txnId = do
  let searchSources = FareProduct.getSearchSources isDashboard
  fareProduct <-
    FareProduct.getBoundedFareProduct merchantOpCityId searchSources tripCategory serviceTier SL.Default
      |<|>| QFareProduct.findUnboundedByMerchantVariantArea merchantOpCityId searchSources tripCategory serviceTier SL.Default
      >>= fromMaybeM NoFareProduct
  getFullFarePolicy mbFromlocaton txnId fareProduct
getFarePolicy mbFromlocaton merchantOpCityId isDashboard tripCategory serviceTier (Just area) txnId = do
  let searchSources = FareProduct.getSearchSources isDashboard
  mbFareProduct <-
    FareProduct.getBoundedFareProduct merchantOpCityId searchSources tripCategory serviceTier area
      |<|>| QFareProduct.findUnboundedByMerchantVariantArea merchantOpCityId searchSources tripCategory serviceTier area
  case mbFareProduct of
    Just fareProduct -> getFullFarePolicy mbFromlocaton txnId fareProduct
    Nothing -> do
      fareProduct <-
        FareProduct.getBoundedFareProduct merchantOpCityId searchSources tripCategory serviceTier SL.Default
          |<|>| QFareProduct.findUnboundedByMerchantVariantArea merchantOpCityId searchSources tripCategory serviceTier SL.Default
          >>= fromMaybeM NoFareProduct
      getFullFarePolicy mbFromlocaton txnId fareProduct

getAllFarePoliciesProduct :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Id Merchant -> Id DMOC.MerchantOperatingCity -> Bool -> LatLong -> Maybe LatLong -> Maybe CacKey -> DTC.TripCategory -> m FarePoliciesProduct
getAllFarePoliciesProduct merchantId merchantOpCityId isDashboard fromlocaton mbToLocation txnId tripCategory = do
  let searchSources = FareProduct.getSearchSources isDashboard
  allFareProducts <- FareProduct.getAllFareProducts merchantId merchantOpCityId searchSources fromlocaton mbToLocation tripCategory
  farePolicies <- mapM (getFullFarePolicy (Just fromlocaton) txnId) allFareProducts.fareProducts
  return $
    FarePoliciesProduct
      { farePolicies,
        area = allFareProducts.area,
        specialLocationTag = allFareProducts.specialLocationTag,
        specialLocationName = allFareProducts.specialLocationName
      }

getFullFarePolicy :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) => Maybe LatLong -> Maybe CacKey -> FareProduct.FareProduct -> m FarePolicyD.FullFarePolicy
getFullFarePolicy sourceLatLong txnId fareProduct = do
  now <- getCurrentTime
  let localTimeZoneSeconds = 19800 -- fix this
  congestionChargeMultiplierFromModel <-
    case fareProduct.tripCategory of
      DTC.OneWay _ -> getCongestionChargeMultiplierFromModel localTimeZoneSeconds now sourceLatLong fareProduct.vehicleServiceTier
      _ -> return Nothing -- For now, we are not supporting congestion charge through model for other trips
  farePolicy' <- QFP.findById txnId fareProduct.farePolicyId >>= fromMaybeM NoFarePolicy
  let updatedCongestionChargeMultiplier =
        case congestionChargeMultiplierFromModel of
          Just congestionChargeMultiplier -> Just $ FarePolicyD.BaseFareAndExtraDistanceFare congestionChargeMultiplier
          Nothing -> farePolicy'.congestionChargeMultiplier
  let farePolicy = updateCongestionChargeMultiplier farePolicy' updatedCongestionChargeMultiplier
  cancellationFarePolicy <- maybe (return Nothing) QCCFP.findById farePolicy.cancellationFarePolicyId
  return $ FarePolicyD.farePolicyToFullFarePolicy fareProduct.merchantId fareProduct.vehicleServiceTier fareProduct.tripCategory cancellationFarePolicy farePolicy

updateCongestionChargeMultiplier :: FarePolicyD.FarePolicy -> Maybe FarePolicyD.CongestionChargeMultiplier -> FarePolicyD.FarePolicy
updateCongestionChargeMultiplier FarePolicyD.FarePolicy {..} congestionMultiplier = FarePolicyD.FarePolicy {congestionChargeMultiplier = congestionMultiplier, ..}

mkFarePolicyBreakups :: (Text -> breakupItemValue) -> (Text -> breakupItemValue -> breakupItem) -> Maybe Meters -> Maybe HighPrecMoney -> FarePolicyD.FarePolicy -> [breakupItem]
mkFarePolicyBreakups mkValue mkBreakupItem mbDistance mbTollCharges farePolicy = do
  let distance = fromMaybe 0 mbDistance -- TODO: Fix Later
      driverExtraFeeBounds = FarePolicyD.findDriverExtraFeeBoundsByDistance distance <$> farePolicy.driverExtraFeeBounds
      nightShiftBounds = farePolicy.nightShiftBounds

      tollChargesCaption = show Tags.TOLL_CHARGES
      tollChargesItem = mkBreakupItem tollChargesCaption . (mkValue . show) <$> mbTollCharges

      congestionChargePercentageCaption = show Tags.CONGESTION_CHARGE_PERCENTAGE
      congestionChargePercentageItem =
        farePolicy.congestionChargeMultiplier <&> \case
          FarePolicyD.BaseFareAndExtraDistanceFare congestionChargeMultiplier -> mkBreakupItem congestionChargePercentageCaption (mkValue $ show ((congestionChargeMultiplier - 1) * 100))
          FarePolicyD.ExtraDistanceFare congestionChargeMultiplier -> mkBreakupItem congestionChargePercentageCaption (mkValue $ show ((congestionChargeMultiplier - 1) * 100))

      parkingChargeCaption = show Tags.PARKING_CHARGE
      parkingChargeItem = mkBreakupItem parkingChargeCaption . (mkValue . show) <$> farePolicy.parkingCharge

      serviceChargeCaption = show Tags.SERVICE_CHARGE
      serviceChargeItem = mkBreakupItem serviceChargeCaption . (mkValue . highPrecMoneyToText) <$> farePolicy.serviceCharge

      governmentChargeCaption = show Tags.GOVERNMENT_CHARGE
      governmentChargeItem = mkBreakupItem governmentChargeCaption . (mkValue . show) <$> farePolicy.govtCharges

      mbDriverExtraFeeBoundSections = farePolicy.driverExtraFeeBounds <&> \driverExtraFeeBound -> NE.sortBy (comparing (.startDistance)) driverExtraFeeBound
      driverExtraFeeBoundsMinFeeItems = maybe [] (\driverExtraFeeBoundSections -> mkDriverExtraFeeBoundsMinFeeItem [] (toList driverExtraFeeBoundSections) 0) mbDriverExtraFeeBoundSections
      driverExtraFeeBoundsMaxFeeItems = maybe [] (\driverExtraFeeBoundSections -> mkDriverExtraFeeBoundsMaxFeeItem [] (toList driverExtraFeeBoundSections) 0) mbDriverExtraFeeBoundSections

      driverMinExtraFee = driverExtraFeeBounds <&> (.minFee)
      driverMinExtraFeeCaption = show Tags.DRIVER_MIN_EXTRA_FEE
      driverMinExtraFeeItem = mkBreakupItem driverMinExtraFeeCaption . (mkValue . highPrecMoneyToText) <$> driverMinExtraFee

      driverMaxExtraFee = driverExtraFeeBounds <&> (.maxFee)
      driverMaxExtraFeeCaption = show Tags.DRIVER_MAX_EXTRA_FEE
      driverMaxExtraFeeItem = mkBreakupItem driverMaxExtraFeeCaption . (mkValue . highPrecMoneyToText) <$> driverMaxExtraFee

      nightShiftStart = nightShiftBounds <&> (.nightShiftStart)
      nightShiftStartInSecondsCaption = show Tags.NIGHT_SHIFT_START_TIME_IN_SECONDS
      nightShiftStartInSecondsItem = mkBreakupItem nightShiftStartInSecondsCaption . (mkValue . show . (.getSeconds) . secondsFromTimeOfDay) <$> nightShiftStart

      nightShiftEnd = nightShiftBounds <&> (.nightShiftEnd)
      nightShiftEndInSecondsCaption = show Tags.NIGHT_SHIFT_END_TIME_IN_SECONDS
      nightShiftEndInSecondsItem = mkBreakupItem nightShiftEndInSecondsCaption . (mkValue . show . (.getSeconds) . secondsFromTimeOfDay) <$> nightShiftEnd

      nightShiftStartCaption = show Tags.NIGHT_SHIFT_START_TIME
      nightShiftStartItem = mkBreakupItem nightShiftStartCaption . (mkValue . show) <$> nightShiftStart

      nightShiftEndCaption = show Tags.NIGHT_SHIFT_END_TIME
      nightShiftEndItem = mkBreakupItem nightShiftEndCaption . (mkValue . show) <$> nightShiftEnd

      insuranceChargeCaption = mkInsuranceChargeCaption farePolicy.distanceUnit
      insuranceChargeItem = mkBreakupItem insuranceChargeCaption . (mkValue . highPrecMoneyToText) <$> farePolicy.perDistanceUnitInsuranceCharge

      cardChargePercentageCaption = show Tags.CARD_CHARGE_PERCENTAGE
      cardChargePercentageItem = mkBreakupItem cardChargePercentageCaption . (mkValue . show . toPercentage) <$> (farePolicy.cardCharge >>= (.perDistanceUnitMultiplier))

      fixedCardChargeCaption = show Tags.FIXED_CARD_CHARGE
      fixedCardChargeItem = mkBreakupItem fixedCardChargeCaption . (mkValue . highPrecMoneyToText) <$> (farePolicy.cardCharge >>= (.fixed))

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
      nightShiftStartInSecondsItem,
      nightShiftEndInSecondsItem,
      congestionChargePercentageItem,
      insuranceChargeItem,
      cardChargePercentageItem,
      fixedCardChargeItem
    ]
    <> additionalDetailsBreakups
    <> driverExtraFeeBoundsMinFeeItems
    <> driverExtraFeeBoundsMaxFeeItems
  where
    mkInsuranceChargeCaption = \case
      Meter -> show Tags.INSURANCE_CHARGE_PER_METER
      Mile -> show Tags.INSURANCE_CHARGE_PER_MILE
      Kilometer -> show Tags.INSURANCE_CHARGE_PER_KM
      Yard -> show Tags.INSURANCE_CHARGE_PER_YARD

    toPercentage x = (x - 1) * 100

    processAdditionalDetails = \case
      FarePolicyD.ProgressiveDetails det -> mkAdditionalProgressiveBreakups det
      FarePolicyD.SlabsDetails det -> mkAdditionalSlabBreakups $ FarePolicyD.findFPSlabsDetailsSlabByDistance (fromMaybe 0 mbDistance) det.slabs
      FarePolicyD.RentalDetails det -> mkAdditionalRentalBreakups det
      FarePolicyD.InterCityDetails det -> mkAdditionalInterCityBreakups det
      FarePolicyD.AmbulanceDetails det -> mkAdditionalAmbulanceBreakups det
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

          pickupChargeCaption = show Tags.DEAD_KILOMETER_FARE
          pickupChargeBreakup = mkBreakupItem pickupChargeCaption (mkValue $ show det.deadKmFare)

      [minFareItem, perHourChargeItem, perExtraMinRateItem, perExtraKmRateItem, includedKmPerHrItem, plannedPerKmRateItem, pickupChargeBreakup]
        <> (waitingChargeBreakups det.waitingChargeInfo)
        <> (oldNightShiftChargeBreakups det.nightShiftCharge)
        <> (newNightShiftChargeBreakups det.nightShiftCharge)

    mkAdditionalInterCityBreakups det = do
      let minFareCaption = show Tags.MIN_FARE
          minFareItem = mkBreakupItem minFareCaption . mkValue $ show det.baseFare

          perHourChargeCaption = show Tags.PER_HOUR_CHARGE
          perHourChargeItem = mkBreakupItem perHourChargeCaption . mkValue $ show det.perHourCharge

          perExtraMinRateCaption = show Tags.PER_MINUTE_CHARGE
          perExtraMinRateItem = mkBreakupItem perExtraMinRateCaption . mkValue $ show det.perExtraMinRate

          perExtraKmRateCaption = show Tags.UNPLANNED_PER_KM_CHARGE
          perExtraKmRateItem = mkBreakupItem perExtraKmRateCaption . mkValue $ show det.perExtraKmRate

          includedKmPerHrCaption = show Tags.PER_HOUR_DISTANCE_KM
          includedKmPerHrItem = mkBreakupItem includedKmPerHrCaption . mkValue $ show det.kmPerPlannedExtraHour

          plannedPerKmRateCaption = show Tags.PLANNED_PER_KM_CHARGE
          plannedPerKmRateItem = mkBreakupItem plannedPerKmRateCaption . mkValue $ show det.perKmRateOneWay

          plannedPerKmRateRoundCaption = show Tags.PLANNED_PER_KM_CHARGE_ROUND_TRIP
          plannedPerKmRateRoundItem = mkBreakupItem plannedPerKmRateRoundCaption . mkValue $ show det.perKmRateRoundTrip

          perDayMaxHourAllowanceCaption = show Tags.PER_DAY_MAX_HOUR_ALLOWANCE
          perDayMaxHourAllowanceItem = mkBreakupItem perDayMaxHourAllowanceCaption . mkValue $ show det.perDayMaxHourAllowance

          pickupChargeCaption = show Tags.DEAD_KILOMETER_FARE
          pickupChargeBreakup = mkBreakupItem pickupChargeCaption (mkValue $ show det.deadKmFare)

      [minFareItem, pickupChargeBreakup, perHourChargeItem, perExtraMinRateItem, perExtraKmRateItem, includedKmPerHrItem, plannedPerKmRateItem, plannedPerKmRateRoundItem, perDayMaxHourAllowanceItem]
        <> (oldNightShiftChargeBreakups det.nightShiftCharge)
        <> (newNightShiftChargeBreakups det.nightShiftCharge)

    mkAdditionalProgressiveBreakups det = do
      let perExtraKmFareSections = NE.sortBy (comparing (.startDistance)) det.perExtraKmRateSections

          mkPerExtraKmFareItem section = do
            let perExtraKmFareCaption = show Tags.EXTRA_PER_KM_FARE
            mkBreakupItem perExtraKmFareCaption (mkValue $ highPrecMoneyToText section.perExtraKmRate)
          perExtraKmFareItems = mkPerExtraKmFareItem <$> (toList perExtraKmFareSections)

          perExtraKmStepFareItems = mkPerExtraKmStepFareItem [] (toList perExtraKmFareSections) det.baseDistance.getMeters

          perMinRateSections = List.sortOn (.rideDuration) $ maybe [] toList det.perMinRateSections
          perMinStepFareItems = mkPerMinStepFareItem [] perMinRateSections

          baseDistanceCaption = show Tags.BASE_DISTANCE
          baseDistanceBreakup = mkBreakupItem baseDistanceCaption (mkValue $ show det.baseDistance)

          baseFareCaption = show Tags.BASE_FARE
          baseFareBreakup = mkBreakupItem baseFareCaption (mkValue $ show det.baseFare)

          pickupChargeCaption = show Tags.DEAD_KILOMETER_FARE
          pickupChargeBreakup = mkBreakupItem pickupChargeCaption (mkValue $ show det.deadKmFare)

      [baseDistanceBreakup, baseFareBreakup, pickupChargeBreakup]
        <> perExtraKmFareItems
        <> perExtraKmStepFareItems
        <> perMinStepFareItems
        <> (waitingChargeBreakups det.waitingChargeInfo)
        <> (oldNightShiftChargeBreakups det.nightShiftCharge)
        <> (newNightShiftChargeBreakups det.nightShiftCharge)
      where
        mkPerExtraKmStepFareItem perExtraKmStepFareItems [] _ = perExtraKmStepFareItems
        mkPerExtraKmStepFareItem perExtraKmStepFareItems [s1] baseDistance = do
          let startDistance = s1.startDistance.getMeters + baseDistance
              perExtraKmStepFareCaption = show $ Tags.EXTRA_PER_KM_STEP_FARE startDistance Nothing
              perExtraKmStepFareItem = mkBreakupItem perExtraKmStepFareCaption (mkValue $ highPrecMoneyToText s1.perExtraKmRate)
          perExtraKmStepFareItems <> [perExtraKmStepFareItem]
        mkPerExtraKmStepFareItem perExtraKmStepFareItems (s1 : s2 : ss) baseDistance = do
          let startDistance = s1.startDistance.getMeters + baseDistance
              endDistance = s2.startDistance.getMeters + baseDistance
              perExtraKmStepFareCaption = show $ Tags.EXTRA_PER_KM_STEP_FARE startDistance (Just endDistance)
              perExtraKmStepFareItem = mkBreakupItem perExtraKmStepFareCaption (mkValue $ highPrecMoneyToText s1.perExtraKmRate)
          mkPerExtraKmStepFareItem (perExtraKmStepFareItems <> [perExtraKmStepFareItem]) (s2 : ss) baseDistance

        mkPerMinStepFareItem perMinStepFareItems [] = perMinStepFareItems
        mkPerMinStepFareItem perMinStepFareItems [fp1] = do
          let sectionStartDuration = fp1.rideDuration.getMinutes
              perMinStepFareCaption = show $ Tags.PER_MINUTE_STEP_FARE sectionStartDuration Nothing
              perMinStepFareItem = mkBreakupItem perMinStepFareCaption (mkValue $ highPrecMoneyToText fp1.perMinRate.amount)
          perMinStepFareItems <> [perMinStepFareItem]
        mkPerMinStepFareItem perMinStepFareItems (fp1 : fp2 : fps) = do
          let sectionStartDuration = fp1.rideDuration.getMinutes
              sectionEndDuration = fp2.rideDuration.getMinutes
              perMinStepFareCaption = show $ Tags.PER_MINUTE_STEP_FARE sectionStartDuration (Just sectionEndDuration)
              perMinStepFareItem = mkBreakupItem perMinStepFareCaption (mkValue $ highPrecMoneyToText fp1.perMinRate.amount)
          mkPerMinStepFareItem (perMinStepFareItems <> [perMinStepFareItem]) (fp2 : fps)

    mkAdditionalSlabBreakups det = do
      let baseDistanceCaption = show Tags.BASE_DISTANCE
          baseDistanceBreakup = mkBreakupItem baseDistanceCaption (mkValue $ show det.startDistance)

          baseFareCaption = show Tags.BASE_FARE
          baseFareBreakup = mkBreakupItem baseFareCaption (mkValue $ show det.baseFare)

      [baseDistanceBreakup, baseFareBreakup]
        <> (platformFeeBreakups det.platformFeeInfo)
        <> (waitingChargeBreakups det.waitingChargeInfo)
        <> (oldNightShiftChargeBreakups det.nightShiftCharge)
        <> (newNightShiftChargeBreakups det.nightShiftCharge)

    mkAdditionalAmbulanceBreakups det = do
      let vehicleAgeSections = NE.sortBy (comparing (.vehicleAge.getMonths)) det.slabs
          perKmStepFareItems = mkPerKmStepFareItem [] (toList vehicleAgeSections)
          baseStepFareItems = mkBaseStepFareItem [] (toList vehicleAgeSections)
          nightShiftFareItems = mkNightShiftStepFareItem [] (toList vehicleAgeSections)
          platformFeeItems = mkPlatformFeeStepFareItem [] (toList vehicleAgeSections)
          waitingChargeItems = mkWaitingChargeStepFareItem [] (toList vehicleAgeSections)

      perKmStepFareItems
        <> baseStepFareItems
        <> nightShiftFareItems
        <> platformFeeItems
        <> waitingChargeItems
      where
        mkPerKmStepFareItem perKmStepFareItems [] = perKmStepFareItems
        mkPerKmStepFareItem perKmStepFareItems [s1] = do
          let perKmStepFareItem = mkBreakupItem (show $ Tags.PER_KM_STEP_FARE s1.vehicleAge.getMonths Nothing) (mkValue $ highPrecMoneyToText s1.perKmRate)
          perKmStepFareItems <> [perKmStepFareItem]
        mkPerKmStepFareItem perKmStepFareItems (s1 : s2 : ss) = do
          let perKmStepFareItem = mkBreakupItem (show $ Tags.PER_KM_STEP_FARE s1.vehicleAge.getMonths (Just s2.vehicleAge.getMonths)) (mkValue $ highPrecMoneyToText s1.perKmRate)
          mkPerKmStepFareItem (perKmStepFareItems <> [perKmStepFareItem]) (s2 : ss)

        mkBaseStepFareItem baseStepFareItems [] = baseStepFareItems
        mkBaseStepFareItem baseStepFareItems [s1] = do
          let baseStepFareItem = mkBreakupItem (show $ Tags.BASE_STEP_FARE s1.vehicleAge.getMonths Nothing) (mkValue $ highPrecMoneyToText s1.baseFare)
          baseStepFareItems <> [baseStepFareItem]
        mkBaseStepFareItem baseStepFareItems (s1 : s2 : ss) = do
          let baseStepFareCaption = show $ Tags.BASE_STEP_FARE s1.vehicleAge.getMonths (Just s2.vehicleAge.getMonths)
              baseStepFareItem = mkBreakupItem baseStepFareCaption (mkValue $ highPrecMoneyToText s1.baseFare)
          mkBaseStepFareItem (baseStepFareItems <> [baseStepFareItem]) (s2 : ss)

        mkNightShiftStepFareItem nightShiftStepFareItems [] = nightShiftStepFareItems
        mkNightShiftStepFareItem nightShiftStepFareItems [s1] = do
          case s1.nightShiftCharge of
            Nothing -> nightShiftStepFareItems
            Just nightShiftCharge -> do
              let (mbNightShiftChargePercentage, mbConstantNightShiftCharge) = getNewNightShiftCharge nightShiftCharge
                  nightShiftPercentageStepFareItem = mkBreakupItem (show $ Tags.NIGHT_SHIFT_STEP_PERCENTAGE s1.vehicleAge.getMonths Nothing) . mkValue <$> mbNightShiftChargePercentage
                  nightShiftConstantStepFareItem = mkBreakupItem (show $ Tags.CONSTANT_NIGHT_SHIFT_STEP_CHARGE s1.vehicleAge.getMonths Nothing) . mkValue <$> mbConstantNightShiftCharge

              nightShiftStepFareItems <> catMaybes [nightShiftPercentageStepFareItem, nightShiftConstantStepFareItem]
        mkNightShiftStepFareItem nightShiftStepFareItems (s1 : s2 : ss) = do
          case s1.nightShiftCharge of
            Nothing -> nightShiftStepFareItems
            Just nightShiftCharge -> do
              let (mbNightShiftChargePercentage, mbConstantNightShiftCharge) = getNewNightShiftCharge nightShiftCharge
                  nightShiftPercentageStepFareItem = mkBreakupItem (show $ Tags.NIGHT_SHIFT_STEP_PERCENTAGE s1.vehicleAge.getMonths (Just s2.vehicleAge.getMonths)) . mkValue <$> mbNightShiftChargePercentage
                  nightShiftConstantStepFareItem = mkBreakupItem (show $ Tags.CONSTANT_NIGHT_SHIFT_STEP_CHARGE s1.vehicleAge.getMonths (Just s2.vehicleAge.getMonths)) . mkValue <$> mbConstantNightShiftCharge
              mkNightShiftStepFareItem (nightShiftStepFareItems <> catMaybes [nightShiftPercentageStepFareItem, nightShiftConstantStepFareItem]) (s2 : ss)

        mkPlatformFeeStepFareItem platformFeeStepFareItems [] = platformFeeStepFareItems
        mkPlatformFeeStepFareItem platformFeeStepFareItems [s1] = do
          case s1.platformFeeInfo of
            Nothing -> platformFeeStepFareItems
            Just platformFeeInfo -> do
              let (mbProgressivePlatformFee, mbConstantPlatformFee) = getPlatformFee platformFeeInfo

                  mbProgressivePlatformFeeStepFareItem = mkBreakupItem (show $ Tags.PLATFORM_FEE_STEP_FARE s1.vehicleAge.getMonths Nothing) . mkValue <$> mbProgressivePlatformFee
                  mbConstantPlatformFeeItem = mkBreakupItem (show $ Tags.CONSTANT_PLATFORM_FEE_STEP_FARE s1.vehicleAge.getMonths Nothing) . mkValue <$> mbConstantPlatformFee
                  mbPlatformFeeCgstItem = mkBreakupItem (show $ Tags.PLATFORM_FEE_CGST_STEP_FARE s1.vehicleAge.getMonths Nothing) (mkValue $ show platformFeeInfo.cgst)
                  mbPlatformFeeSgstItem = mkBreakupItem (show $ Tags.PLATFORM_FEE_SGST_STEP_FARE s1.vehicleAge.getMonths Nothing) (mkValue $ show platformFeeInfo.sgst)

              platformFeeStepFareItems <> catMaybes [mbProgressivePlatformFeeStepFareItem, mbConstantPlatformFeeItem, Just mbPlatformFeeCgstItem, Just mbPlatformFeeSgstItem]
        mkPlatformFeeStepFareItem platformFeeStepFareItems (s1 : s2 : ss) = do
          case s1.platformFeeInfo of
            Nothing -> platformFeeStepFareItems
            Just platformFeeInfo -> do
              let (mbProgressivePlatformFee, mbConstantPlatformFee) = getPlatformFee platformFeeInfo

                  mbProgressivePlatformFeeStepFareItem = mkBreakupItem (show $ Tags.PLATFORM_FEE_STEP_FARE s1.vehicleAge.getMonths (Just s2.vehicleAge.getMonths)) . mkValue <$> mbProgressivePlatformFee
                  mbConstantPlatformFeeItem = mkBreakupItem (show $ Tags.CONSTANT_PLATFORM_FEE_STEP_FARE s1.vehicleAge.getMonths (Just s2.vehicleAge.getMonths)) . mkValue <$> mbConstantPlatformFee
                  mbPlatformFeeCgstItem = mkBreakupItem (show $ Tags.PLATFORM_FEE_CGST_STEP_FARE s1.vehicleAge.getMonths (Just s2.vehicleAge.getMonths)) (mkValue $ show platformFeeInfo.cgst)
                  mbPlatformFeeSgstItem = mkBreakupItem (show $ Tags.PLATFORM_FEE_SGST_STEP_FARE s1.vehicleAge.getMonths (Just s2.vehicleAge.getMonths)) (mkValue $ show platformFeeInfo.sgst)

              mkPlatformFeeStepFareItem (platformFeeStepFareItems <> catMaybes [mbProgressivePlatformFeeStepFareItem, mbConstantPlatformFeeItem, Just mbPlatformFeeCgstItem, Just mbPlatformFeeSgstItem]) (s2 : ss)

        mkWaitingChargeStepFareItem waitingChargeStepFareItems [] = waitingChargeStepFareItems
        mkWaitingChargeStepFareItem waitingChargeStepFareItems [s1] = do
          case s1.waitingChargeInfo of
            Nothing -> waitingChargeStepFareItems
            Just waitingChargeInfo -> do
              let (_, mbWaitingChargePerMinFloat, mbConstantWaitingCharges) = getWaitingCharge waitingChargeInfo

                  mbWaitingChargePerMinFloatStepFareItem = mkBreakupItem (show $ Tags.WAITING_CHARGE_PER_MIN_STEP_FARE s1.vehicleAge.getMonths Nothing) . mkValue <$> mbWaitingChargePerMinFloat
                  mbConstantWaitingChargeStepFareItem = mkBreakupItem (show $ Tags.CONSTANT_WAITING_CHARGE_STEP_FARE s1.vehicleAge.getMonths Nothing) . mkValue <$> mbConstantWaitingCharges
                  freeWaitingTimeStepFareItem = mkBreakupItem (show $ Tags.FREE_WAITING_TIME_STEP_MINUTES s1.vehicleAge.getMonths Nothing) . mkValue . show $ waitingChargeInfo.freeWaitingTime.getMinutes

              waitingChargeStepFareItems <> catMaybes [mbWaitingChargePerMinFloatStepFareItem, mbConstantWaitingChargeStepFareItem, Just freeWaitingTimeStepFareItem]
        mkWaitingChargeStepFareItem waitingChargeStepFareItems (s1 : s2 : ss) = do
          case s1.waitingChargeInfo of
            Nothing -> waitingChargeStepFareItems
            Just waitingChargeInfo -> do
              let (_, mbWaitingChargePerMinFloat, mbConstantWaitingCharges) = getWaitingCharge waitingChargeInfo

                  mbWaitingChargePerMinFloatStepFareItem = mkBreakupItem (show $ Tags.WAITING_CHARGE_PER_MIN_STEP_FARE s1.vehicleAge.getMonths (Just s2.vehicleAge.getMonths)) . mkValue <$> mbWaitingChargePerMinFloat
                  mbConstantWaitingChargeStepFareItem = mkBreakupItem (show $ Tags.CONSTANT_WAITING_CHARGE_STEP_FARE s1.vehicleAge.getMonths (Just s2.vehicleAge.getMonths)) . mkValue <$> mbConstantWaitingCharges
                  freeWaitingTimeStepFareItem = mkBreakupItem (show $ Tags.FREE_WAITING_TIME_STEP_MINUTES s1.vehicleAge.getMonths (Just s2.vehicleAge.getMonths)) . mkValue . show $ waitingChargeInfo.freeWaitingTime.getMinutes

              mkWaitingChargeStepFareItem (waitingChargeStepFareItems <> catMaybes [mbWaitingChargePerMinFloatStepFareItem, mbConstantWaitingChargeStepFareItem, Just freeWaitingTimeStepFareItem]) (s2 : ss)

    platformFeeBreakups Nothing = []
    platformFeeBreakups (Just platformFeeInfo) = do
      let (mbProgressivePlatformFee, mbConstantPlatformFee) = getPlatformFee platformFeeInfo

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
      let (mbWaitingChargePerMin, mbWaitingChargePerMinFloat, mbConstantWaitingCharges) = getWaitingCharge waitingChargeInfo

          waitingOrPickupChargesCaption = show Tags.WAITING_OR_PICKUP_CHARGES -- TODO :: To be deprecated
          mbWaitingOrPickupChargesItem = mkBreakupItem waitingOrPickupChargesCaption . mkValue <$> mbConstantWaitingCharges

          mbWaitingChargePerMinCaption = show Tags.WAITING_CHARGE_PER_MIN
          mbWaitingChargePerMinItem = mkBreakupItem mbWaitingChargePerMinCaption . mkValue <$> mbWaitingChargePerMin

          mbWaitingChargePerMinFloatCaption = show Tags.WAITING_CHARGE_RATE_PER_MIN
          mbWaitingChargePerMinFloatItem = mkBreakupItem mbWaitingChargePerMinFloatCaption . mkValue <$> mbWaitingChargePerMinFloat

          mbConstantWaitingChargeCaption = show Tags.CONSTANT_WAITING_CHARGE
          mbConstantWaitingChargeItem = mkBreakupItem mbConstantWaitingChargeCaption . mkValue <$> mbConstantWaitingCharges

          freeWaitingTimeCation = show Tags.FREE_WAITING_TIME_IN_MINUTES
          freeWaitingTimeItem = mkBreakupItem freeWaitingTimeCation . mkValue . show $ waitingChargeInfo.freeWaitingTime.getMinutes

      catMaybes [mbWaitingOrPickupChargesItem, mbWaitingChargePerMinItem, mbWaitingChargePerMinFloatItem, mbConstantWaitingChargeItem, Just freeWaitingTimeItem]

    oldNightShiftChargeBreakups nightShiftChargeInfo = do
      let getNightShiftChargeValue (FarePolicyD.ProgressiveNightShiftCharge a) = highPrecMoneyToText $ toHighPrecMoney a -- fix from customer side first
          getNightShiftChargeValue (FarePolicyD.ConstantNightShiftCharge a) = show a

      let oldNightShiftCharge = getNightShiftChargeValue <$> nightShiftChargeInfo
          oldNightShiftChargeCaption = show Tags.NIGHT_SHIFT_CHARGE
          oldNightShiftChargeItem = mkBreakupItem oldNightShiftChargeCaption . mkValue <$> oldNightShiftCharge

      catMaybes [oldNightShiftChargeItem]

    newNightShiftChargeBreakups Nothing = []
    newNightShiftChargeBreakups (Just nightShiftChargeInfo) = do
      let (mbNightShiftChargePercentage, mbConstantNightShiftCharge) = getNewNightShiftCharge nightShiftChargeInfo

          mbNightShiftChargePercentageCaption = show Tags.NIGHT_SHIFT_CHARGE_PERCENTAGE
          mbNightShiftChargePercentageItem = mkBreakupItem mbNightShiftChargePercentageCaption . mkValue <$> mbNightShiftChargePercentage

          mbConstantNightShiftChargeCaption = show Tags.CONSTANT_NIGHT_SHIFT_CHARGE
          mbConstantNightShiftChargeItem = mkBreakupItem mbConstantNightShiftChargeCaption . mkValue <$> mbConstantNightShiftCharge

      catMaybes [mbNightShiftChargePercentageItem, mbConstantNightShiftChargeItem]

    getNewNightShiftCharge nightShiftCharge =
      nightShiftCharge & \case
        FarePolicyD.ProgressiveNightShiftCharge a -> (Just $ show ((a - 1) * 100), Nothing)
        FarePolicyD.ConstantNightShiftCharge a -> (Nothing, Just $ show a)

    getWaitingCharge waitingChargeInfo =
      waitingChargeInfo.waitingCharge & \case
        FarePolicyD.PerMinuteWaitingCharge hpm -> (Just $ highPrecMoneyToText hpm, Just $ show hpm, Nothing)
        FarePolicyD.ConstantWaitingCharge mo -> (Nothing, Nothing, Just $ show mo)

    getPlatformFee platformFeeInfo =
      platformFeeInfo.platformFeeCharge & \case
        FarePolicyD.ProgressivePlatformFee ppf -> (Just $ show ppf, Nothing)
        FarePolicyD.ConstantPlatformFee cpf -> (Nothing, Just $ show cpf)

    mkDriverExtraFeeBoundsMinFeeItem driverExtraFeeBoundsMinFeeItems [] _ = driverExtraFeeBoundsMinFeeItems
    mkDriverExtraFeeBoundsMinFeeItem driverExtraFeeBoundsMinFeeItems [s1] startDistance = do
      let driverExtraFeeBoundMinFeeCaption = show $ Tags.DRIVER_EXTRA_FEE_BOUNDS_STEP_MIN_FEE startDistance Nothing
          driverExtraFeeBoundMinFeeItem = mkBreakupItem driverExtraFeeBoundMinFeeCaption (mkValue $ highPrecMoneyToText s1.minFee)
      driverExtraFeeBoundsMinFeeItems <> [driverExtraFeeBoundMinFeeItem]
    mkDriverExtraFeeBoundsMinFeeItem driverExtraFeeBoundsMinFeeItems (s1 : s2 : ss) startDistance = do
      let driverExtraFeeBoundMinFeeCaption = show $ Tags.DRIVER_EXTRA_FEE_BOUNDS_STEP_MIN_FEE startDistance (Just s2.startDistance.getMeters)
          driverExtraFeeBoundMinFeeItem = mkBreakupItem driverExtraFeeBoundMinFeeCaption (mkValue $ highPrecMoneyToText s1.minFee)
      mkDriverExtraFeeBoundsMinFeeItem (driverExtraFeeBoundsMinFeeItems <> [driverExtraFeeBoundMinFeeItem]) (s2 : ss) s2.startDistance.getMeters

    mkDriverExtraFeeBoundsMaxFeeItem driverExtraFeeBoundsMaxFeeItems [] _ = driverExtraFeeBoundsMaxFeeItems
    mkDriverExtraFeeBoundsMaxFeeItem driverExtraFeeBoundsMaxFeeItems [s1] startDistance = do
      let driverExtraFeeBoundMaxFeeCaption = show $ Tags.DRIVER_EXTRA_FEE_BOUNDS_STEP_MAX_FEE startDistance Nothing
          driverExtraFeeBoundMaxFeeItem = mkBreakupItem driverExtraFeeBoundMaxFeeCaption (mkValue $ highPrecMoneyToText s1.maxFee)
      driverExtraFeeBoundsMaxFeeItems <> [driverExtraFeeBoundMaxFeeItem]
    mkDriverExtraFeeBoundsMaxFeeItem driverExtraFeeBoundsMaxFeeItems (s1 : s2 : ss) startDistance = do
      let driverExtraFeeBoundMaxFeeCaption = show $ Tags.DRIVER_EXTRA_FEE_BOUNDS_STEP_MAX_FEE startDistance (Just s2.startDistance.getMeters)
          driverExtraFeeBoundMaxFeeItem = mkBreakupItem driverExtraFeeBoundMaxFeeCaption (mkValue $ highPrecMoneyToText s1.maxFee)
      mkDriverExtraFeeBoundsMaxFeeItem (driverExtraFeeBoundsMaxFeeItems <> [driverExtraFeeBoundMaxFeeItem]) (s2 : ss) s2.startDistance.getMeters

utcToIst :: Minutes -> UTCTime -> LocalTime
utcToIst timeZoneDiff utcTime = utcToLocalTime (minutesToTimeZone timeZoneDiff.getMinutes) utcTime -- IST is UTC + 5:30

localTimeToDayOfWeekAndHour :: LocalTime -> (DayOfWeek, Int)
localTimeToDayOfWeekAndHour localTime =
  let (year, month, day) = toGregorian $ localDay localTime
      (_, _, dayOfWeekNum) = toWeekDate $ fromGregorian year month day
      dayWeek = toEnum dayOfWeekNum :: DayOfWeek
      hourOfDay = todHour $ localTimeOfDay localTime
   in (dayWeek, hourOfDay)

getCongestionChargeMultiplierFromModel ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  Seconds ->
  UTCTime ->
  Maybe LatLong ->
  ServiceTierType ->
  m (Maybe Centesimal)
getCongestionChargeMultiplierFromModel _ _ Nothing _ = pure Nothing
getCongestionChargeMultiplierFromModel timeDiffFromUtc now (Just sourceLatLong) serviceTier = do
  let localTime = utcToIst (secondsToMinutes timeDiffFromUtc) now
  let (dayWeek, hourOfDay) = localTimeToDayOfWeekAndHour localTime
  let mbSourceHash = Geohash.encode 5 (sourceLatLong.lat, sourceLatLong.lon)
  case mbSourceHash of
    Nothing -> pure Nothing
    Just sourceHash -> do
      surgePrice <- SurgePricing.findByHexDayHourAndVehicleServiceTier (T.pack sourceHash) (T.pack . show $ dayWeek) hourOfDay serviceTier
      return $ surgePrice <&> (.surgeMultiplier)
