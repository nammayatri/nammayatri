{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module Lib.IncentiveJourney.Condition
  ( evaluateCondition,
    deltaForCondition,
    rideMatchesLocationFilter,
    rideMatchesVehicleFilter,
  )
where

import Kernel.Prelude hiding (EQ, GT, LT)
import Lib.IncentiveJourney.Types

evaluateCondition :: ConditionOperator -> Int -> Int -> Bool
evaluateCondition conditionOperator lhs rhs =
  case conditionOperator of
    GTE -> lhs >= rhs
    GT -> lhs > rhs
    EQ -> lhs == rhs
    LTE -> lhs <= rhs
    LT -> lhs < rhs

deltaForCondition :: ConditionType -> RideDeltas -> Int
deltaForCondition conditionType deltas =
  case conditionType of
    RideCompleted -> deltas.ridesDelta
    Earnings -> deltas.earningsDelta
    Distance -> deltas.distanceMetersDelta
    RideDuration -> deltas.rideTimeSecondsDelta
    BookingTicket -> 0

-- | Nothing on the milestone means unrestricted. Compare against ride's shown category/tier.
rideMatchesVehicleFilter :: EvalMilestone -> Maybe Text -> Maybe Text -> Bool
rideMatchesVehicleFilter milestone mbRideVehicleCategory mbRideServiceTierType =
  (isNothing milestone.vehicleCategory || milestone.vehicleCategory == mbRideVehicleCategory)
    && (isNothing milestone.serviceTierType || milestone.serviceTierType == mbRideServiceTierType)

-- | Area is an independent dimension (like vehicle). Default / Nothing = any location.
rideMatchesLocationFilter :: EvalMilestone -> Maybe Text -> Maybe Text -> Bool
rideMatchesLocationFilter milestone mbPickupSpecialLocationId mbDropSpecialLocationId =
  case milestone.areaType of
    Nothing -> True
    Just Default -> True
    Just Pickup -> matchesRequired milestone.specialLocationIds mbPickupSpecialLocationId
    Just Drop -> matchesRequired milestone.specialLocationIds mbDropSpecialLocationId
    Just PickupDrop ->
      matchesRequired milestone.specialLocationIds mbPickupSpecialLocationId
        && matchesRequired milestone.specialLocationIds mbDropSpecialLocationId
  where
    matchesRequired Nothing _ = False
    matchesRequired (Just allowedIds) mbActualId = maybe False (`elem` allowedIds) mbActualId
