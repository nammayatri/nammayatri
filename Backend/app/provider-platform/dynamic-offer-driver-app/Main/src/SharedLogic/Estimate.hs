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
  )
where

import Domain.Types.Estimate as DEst
import Domain.Types.FarePolicy
import Kernel.Prelude
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.FareCalculator
import Storage.CachedQueries.CacheConfig

buildEstimate ::
  (HasCacheConfig r, EsqDBFlow m r, HedisFlow m r) =>
  Text ->
  UTCTime ->
  Meters ->
  FarePolicy ->
  m DEst.Estimate
buildEstimate transactionId startTime dist farePolicy = do
  fareParams <-
    calculateFareParameters
      CalculateFareParametersParams
        { farePolicy = farePolicy,
          distance = dist,
          rideTime = startTime,
          waitingTime = Nothing,
          driverSelectedFare = Nothing,
          customerExtraFee = Nothing
        }
  let baseFare = fareSum fareParams
      estimateBreakups = mkBreakupList  DEst.EstimateBreakup (DEst.EstimateBreakupPrice "INR") fareParams
  logDebug $ "baseFare: " <> show baseFare
  uuid <- generateGUID
  now <- getCurrentTime
  pure
    DEst.Estimate
      { id = Id uuid,
        transactionId = transactionId,
        vehicleVariant = farePolicy.vehicleVariant,
        minFare = baseFare,
        maxFare = baseFare + fromMaybe 0 (farePolicy.driverExtraFeeBounds <&> (.maxFee)),
        estimateBreakupList = estimateBreakups,
        nightShiftInfo =
          ((,) <$> fareParams.nightShiftCharge <*> farePolicy.nightShiftBounds)
            <&> \(nightShiftCharge, nightShiftBounds) ->
              NightShiftInfo
                { nightShiftCharge = nightShiftCharge,
                  oldNightShiftCharge = undefined,
                  nightShiftStart = nightShiftBounds.nightShiftStart,
                  nightShiftEnd = nightShiftBounds.nightShiftStart
                },
        waitingCharges = makeWaitingCharges,
        createdAt = now
      }
  where
    makeWaitingCharges = do
      let waitingCharge = case farePolicy.farePolicyDetails of
            SlabsDetails det -> (findFPSlabsDetailsSlabByDistance dist det.slabs).waitingCharge
            ProgressiveDetails det -> det.waitingCharge
      let (mbWaitingChargePerMin, mbWaitingOrPickupCharges) = case waitingCharge of
            Just (PerMinuteWaitingCharge charge) -> (Just $ roundToIntegral charge, Nothing)
            Just (ConstantWaitingCharge charge) -> (Nothing, Just charge)
            Nothing -> (Nothing, Nothing)
      WaitingCharges
        { waitingTimeEstimatedThreshold = farePolicy.waitingTimeEstimatedThreshold,
          waitingChargePerMin = mbWaitingChargePerMin,
          waitingOrPickupCharges = mbWaitingOrPickupCharges
        }