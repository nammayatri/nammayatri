{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Estimate
  ( EstimateItem (..),
    WaitingCharges (..),
    NightShiftRate (..),
    BreakupItem (..),
    buildEstimate,
    buildEstimateFromSlabFarePolicy,
  )
where

import qualified Data.List.NonEmpty as NE
import Domain.Types.FarePolicy
import qualified Domain.Types.Merchant as DM
import Domain.Types.SlabFarePolicy
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Types.Common
import Kernel.Utils.Common
import SharedLogic.DriverPool.Types (DriverPoolResult)
import SharedLogic.FareCalculator
import Storage.CachedQueries.CacheConfig
import Tools.Maps (LatLong (..))

data EstimateItem = EstimateItem
  { vehicleVariant :: Variant.Variant,
    distanceToPickup :: Meters,
    minFare :: Money,
    maxFare :: Money,
    estimateBreakupList :: [BreakupItem],
    nightShiftRate :: Maybe NightShiftRate,
    waitingCharges :: WaitingCharges,
    driversLatLong :: [LatLong]
  }

data WaitingCharges = WaitingCharges
  { waitingTimeEstimatedThreshold :: Maybe Seconds,
    waitingChargePerMin :: Maybe Money,
    waitingOrPickupCharges :: Maybe Money
  }

data NightShiftRate = NightShiftRate
  { nightShiftMultiplier :: Maybe Centesimal,
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay
  }

data BreakupItem = BreakupItem
  { title :: Text,
    price :: BreakupPrice
  }

data BreakupPrice = BreakupPrice
  { currency :: Text,
    value :: Money
  }

buildEstimate ::
  (HasCacheConfig r, EsqDBFlow m r, HedisFlow m r) =>
  DM.Merchant ->
  UTCTime ->
  Meters ->
  (FarePolicy, NonEmpty DriverPoolResult) ->
  m EstimateItem
buildEstimate org startTime dist (farePolicy, filteredPoolByVehVariant) = do
  fareParams <- calculateFare org.id (Left farePolicy) dist startTime Nothing
  let baseFare = fareSum fareParams
      currency = "INR"
      estimateBreakups = mkBreakupListItems (BreakupPrice currency) BreakupItem farePolicy
  let latlongList = fmap (\x -> LatLong x.lat x.lon) filteredPoolByVehVariant
  logDebug $ "baseFare: " <> show baseFare
  let driverMetadata = NE.head filteredPoolByVehVariant
  logDebug $ "distanceToPickup: " <> show driverMetadata.distanceToPickup
  pure
    EstimateItem
      { vehicleVariant = farePolicy.vehicleVariant,
        distanceToPickup = driverMetadata.distanceToPickup,
        minFare = baseFare,
        maxFare = baseFare + farePolicy.driverExtraFee.maxFee,
        estimateBreakupList = estimateBreakups,
        driversLatLong = toList latlongList,
        nightShiftRate =
          Just $
            NightShiftRate
              { nightShiftMultiplier = fareParams.nightShiftRate,
                nightShiftStart = farePolicy.nightShiftStart,
                nightShiftEnd = farePolicy.nightShiftEnd
              },
        waitingCharges =
          WaitingCharges
            { waitingTimeEstimatedThreshold = farePolicy.waitingTimeEstimatedThreshold,
              waitingChargePerMin = farePolicy.waitingChargePerMin,
              waitingOrPickupCharges = Nothing
            }
      }

mkBreakupListItems ::
  (Money -> breakupItemPrice) ->
  (Text -> breakupItemPrice -> breakupItem) ->
  FarePolicy ->
  [breakupItem]
mkBreakupListItems mkPrice mkBreakupItem farePolicy = do
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

buildEstimateFromSlabFarePolicy ::
  (HasCacheConfig r, EsqDBFlow m r, HedisFlow m r) =>
  DM.Merchant ->
  UTCTime ->
  Meters ->
  (SlabFarePolicy, NonEmpty DriverPoolResult) ->
  m EstimateItem
buildEstimateFromSlabFarePolicy org startTime dist (slabFarePolicy, filteredPoolByVehVariant) = do
  fareParams <- calculateFare org.id (Right slabFarePolicy) dist startTime Nothing
  let baseFare = fareSum fareParams
      currency = "INR"
      estimateBreakups = mkBreakupSlabListItems (BreakupPrice currency) BreakupItem slabFarePolicy fareParams.baseFare fareParams.waitingOrPickupCharges
  let latlongList = fmap (\x -> LatLong x.lat x.lon) filteredPoolByVehVariant
  logDebug $ "baseFare: " <> show baseFare
  let driverMetadata = NE.head filteredPoolByVehVariant
  logDebug $ "distanceToPickup: " <> show driverMetadata.distanceToPickup
  pure
    EstimateItem
      { vehicleVariant = slabFarePolicy.vehicleVariant,
        distanceToPickup = driverMetadata.distanceToPickup,
        minFare = baseFare,
        maxFare = baseFare,
        estimateBreakupList = estimateBreakups,
        driversLatLong = toList latlongList,
        nightShiftRate =
          Just $
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
            }
      }

mkBreakupSlabListItems ::
  (Money -> breakupItemPrice) ->
  (Text -> breakupItemPrice -> breakupItem) ->
  SlabFarePolicy ->
  Money ->
  Maybe Money ->
  [breakupItem]
mkBreakupSlabListItems mkPrice mkBreakupItem slabFarePolicy baseFare waitingOrPickupCharge = do
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
