{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Estimate
  ( module DEst,
    buildEstimate,
    buildEstimateFromSlabFarePolicy,
  )
where

import Domain.Types.Estimate as DEst
import Domain.Types.FarePolicy
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SearchRequest as DSR
import Domain.Types.SlabFarePolicy
import Kernel.Prelude
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.FareCalculator
import Storage.CachedQueries.CacheConfig

buildEstimate ::
  (HasCacheConfig r, EsqDBFlow m r, HedisFlow m r) =>
  DM.Merchant ->
  Id DSR.SearchRequest ->
  UTCTime ->
  Meters ->
  FarePolicy ->
  m DEst.Estimate
buildEstimate org searchReqId startTime dist farePolicy = do
  fareParams <- calculateFare org.id (Left farePolicy) dist startTime Nothing
  let baseFare = fareSum fareParams
      estimateBreakups = mkBreakupListItems farePolicy
  logDebug $ "baseFare: " <> show baseFare
  uuid <- generateGUID
  now <- getCurrentTime
  pure
    DEst.Estimate
      { id = Id uuid,
        requestId = searchReqId,
        vehicleVariant = farePolicy.vehicleVariant,
        minFare = baseFare,
        maxFare = baseFare + farePolicy.driverExtraFee.maxFee,
        estimateBreakupList = estimateBreakups,
        nightShiftRate =
          DEst.NightShiftRate
            { nightShiftMultiplier = fareParams.nightShiftRate,
              nightShiftStart = farePolicy.nightShiftStart,
              nightShiftEnd = farePolicy.nightShiftEnd
            },
        waitingCharges =
          DEst.WaitingCharges
            { waitingTimeEstimatedThreshold = farePolicy.waitingTimeEstimatedThreshold,
              waitingChargePerMin = farePolicy.waitingChargePerMin,
              waitingOrPickupCharges = Nothing
            },
        createdAt = now
      }

mkBreakupListItems ::
  FarePolicy ->
  [EstimateBreakup]
mkBreakupListItems farePolicy = do
  let baseDistanceFare = roundToIntegral farePolicy.baseDistanceFare
      baseDistanceFareCaption = "BASE_DISTANCE_FARE"
      baseDistanceFareItem = mkBreakupItem baseDistanceFareCaption (mkPrice baseDistanceFare)

      perExtraKmFare = roundToIntegral farePolicy.perExtraKmFare
      perExtraKmFareCaption = "EXTRA_PER_KM_FARE"
      perExtraKmFareItem = mkBreakupItem perExtraKmFareCaption (mkPrice perExtraKmFare)

      deadKmFare = farePolicy.deadKmFare
      deadKmFareCaption = "DEAD_KILOMETER_FARE"
      deadKmFareItem = mkBreakupItem deadKmFareCaption (mkPrice deadKmFare)

      driverMinExtraFee = farePolicy.driverExtraFee.minFee
      driverMinExtraFeeCaption = "DRIVER_MIN_EXTRA_FEE"
      driverMinExtraFeeItem = mkBreakupItem driverMinExtraFeeCaption (mkPrice driverMinExtraFee)

      driverMaxExtraFee = farePolicy.driverExtraFee.maxFee
      driverMaxExtraFeeCaption = "DRIVER_MAX_EXTRA_FEE"
      driverMaxExtraFeeItem = mkBreakupItem driverMaxExtraFeeCaption (mkPrice driverMaxExtraFee)

  [baseDistanceFareItem, perExtraKmFareItem, deadKmFareItem, driverMinExtraFeeItem, driverMaxExtraFeeItem]
  where
    currency = "INR"
    mkPrice = DEst.EstimateBreakupPrice currency
    mkBreakupItem = DEst.EstimateBreakup

buildEstimateFromSlabFarePolicy ::
  (HasCacheConfig r, EsqDBFlow m r, HedisFlow m r) =>
  DM.Merchant ->
  Id DSR.SearchRequest ->
  UTCTime ->
  Meters ->
  SlabFarePolicy ->
  m Estimate
buildEstimateFromSlabFarePolicy org searchReqId startTime dist slabFarePolicy = do
  fareParams <- calculateFare org.id (Right slabFarePolicy) dist startTime Nothing
  let baseFare = fareSum fareParams
      estimateBreakups = mkBreakupSlabListItems slabFarePolicy fareParams.baseFare fareParams.waitingOrPickupCharges
  logDebug $ "baseFare: " <> show baseFare
  uuid <- generateGUID
  now <- getCurrentTime
  pure
    Estimate
      { id = Id uuid,
        requestId = searchReqId,
        vehicleVariant = slabFarePolicy.vehicleVariant,
        minFare = baseFare,
        maxFare = baseFare,
        estimateBreakupList = estimateBreakups,
        nightShiftRate =
          NightShiftRate
            { nightShiftMultiplier = fareParams.nightShiftRate,
              nightShiftStart = slabFarePolicy.nightShiftStart,
              nightShiftEnd = slabFarePolicy.nightShiftEnd
            },
        waitingCharges =
          WaitingCharges
            { waitingTimeEstimatedThreshold = Nothing,
              waitingChargePerMin = Nothing,
              waitingOrPickupCharges = fareParams.waitingOrPickupCharges
            },
        createdAt = now
      }

mkBreakupSlabListItems ::
  SlabFarePolicy ->
  Money ->
  Maybe Money ->
  [EstimateBreakup]
mkBreakupSlabListItems slabFarePolicy baseFare waitingOrPickupCharge = do
  let baseDistanceFare = baseFare
      baseDistanceFareCaption = "BASE_DISTANCE_FARE"
      baseDistanceFareItem = mkBreakupItem baseDistanceFareCaption (mkPrice baseDistanceFare)

      serviceCharge = slabFarePolicy.serviceCharge
      serviceChargeCaption = "SERVICE_CHARGE"
      serviceChargeItem = mkBreakupItem serviceChargeCaption (mkPrice serviceCharge)

      waitingOrPickupCharges = fromMaybe 0 waitingOrPickupCharge
      waitingOrPickupChargesCaption = "WAITING_OR_PICKUP_CHARGES"
      waitingOrPickupChargesItem = mkBreakupItem waitingOrPickupChargesCaption (mkPrice waitingOrPickupCharges)

      mbFixedGovtRate = slabFarePolicy.govtChargesPerc
      mbFixedGovtRateCaption = "FIXED_GOVERNMENT_RATE"
      mbFixedGovtRateItem = (\fixedGovtRate -> mkBreakupItem mbFixedGovtRateCaption (mkPrice $ fromIntegral ((fixedGovtRate * fromIntegral baseFare) `div` 100))) <$> mbFixedGovtRate

  [baseDistanceFareItem, serviceChargeItem, waitingOrPickupChargesItem] <> catMaybes [mbFixedGovtRateItem]
  where
    currency = "INR"
    mkPrice = DEst.EstimateBreakupPrice currency
    mkBreakupItem = DEst.EstimateBreakup
