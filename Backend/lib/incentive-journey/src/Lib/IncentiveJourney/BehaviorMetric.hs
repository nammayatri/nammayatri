{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module Lib.IncentiveJourney.BehaviorMetric
  ( MetricContext (..),
    BehaviorMetricTick (..),
    emptyMetricContext,
    mergeMetricContext,
    applyRideDeltasToContext,
    metricValueForCondition,
    evaluateBehaviorCondition,
    behaviorTickDebounceKey,
    varRidesCompleted,
    varEarnings,
    varDistanceMeters,
    varRideTimeSeconds,
    varCancellationRate,
    varAcceptanceRate,
    varOnlineMinutesTotal,
  )
where

import Data.Aeson (Value)
import qualified Data.HashMap.Strict as HM
import Kernel.Prelude
import qualified Lib.IncentiveJourney.Condition as Condition
import qualified Lib.IncentiveJourney.JsonLogic as JsonLogic
import Lib.IncentiveJourney.Types

-- | Absolute counters / rates for JsonLogic (and BehaviourMetric simple columns).
-- Ride + behaviour vars share this bag — same level.
data MetricContext = MetricContext
  { vars :: HM.HashMap Text Int
  }
  deriving (Eq, Show, Generic)

-- | Kafka tick payload. App produces after Redis bump; consumer loads full bag + evals.
data BehaviorMetricTick = BehaviorMetricTick
  { personId :: Text,
    merchantId :: Text,
    merchantOperatingCityId :: Text,
    actionType :: Text,
    occurredAt :: UTCTime
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

emptyMetricContext :: MetricContext
emptyMetricContext = MetricContext {vars = HM.empty}

varRidesCompleted :: Text
varRidesCompleted = "ridesCompleted"

varEarnings :: Text
varEarnings = "earnings"

varDistanceMeters :: Text
varDistanceMeters = "distanceMeters"

varRideTimeSeconds :: Text
varRideTimeSeconds = "rideTimeSeconds"

varCancellationRate :: Text
varCancellationRate = "cancellationRate"

varAcceptanceRate :: Text
varAcceptanceRate = "acceptanceRate"

varOnlineMinutesTotal :: Text
varOnlineMinutesTotal = "onlineMinutes.total"

mergeMetricContext :: MetricContext -> MetricContext -> MetricContext
mergeMetricContext base overlay =
  MetricContext {vars = HM.union overlay.vars base.vars}

-- | Add this ride's deltas onto period ride counters already in the bag.
applyRideDeltasToContext :: RideDeltas -> MetricContext -> MetricContext
applyRideDeltasToContext deltas ctx =
  MetricContext
    { vars =
        HM.insertWith (+) varRidesCompleted deltas.ridesDelta
          . HM.insertWith (+) varEarnings deltas.earningsDelta
          . HM.insertWith (+) varDistanceMeters deltas.distanceMetersDelta
          . HM.insertWith (+) varRideTimeSeconds deltas.rideTimeSecondsDelta
          $ ctx.vars
    }

metricValueForCondition :: ConditionType -> MetricContext -> Int
metricValueForCondition conditionType ctx =
  case conditionType of
    CancellationRate -> HM.lookupDefault 0 varCancellationRate ctx.vars
    AcceptanceRate -> HM.lookupDefault 0 varAcceptanceRate ctx.vars
    OnlineMinutes -> HM.lookupDefault 0 varOnlineMinutesTotal ctx.vars
    RideCompleted -> HM.lookupDefault 0 varRidesCompleted ctx.vars
    Earnings -> HM.lookupDefault 0 varEarnings ctx.vars
    Distance -> HM.lookupDefault 0 varDistanceMeters ctx.vars
    RideDuration -> HM.lookupDefault 0 varRideTimeSeconds ctx.vars
    BookingTicket -> HM.lookupDefault 0 "bookingTicket" ctx.vars

-- | Award/progress gate: when conditionLogic is set it is the source of truth; else simple columns vs bag.
-- (NoReward + conditionLogic False → Disqualified in Evaluate.)
evaluateBehaviorCondition ::
  Maybe Value ->
  ConditionType ->
  ConditionOperator ->
  Int ->
  MetricContext ->
  Either Text Bool
evaluateBehaviorCondition mbLogic conditionType conditionOperator conditionValue ctx =
  case mbLogic of
    Just logic -> JsonLogic.evaluateJsonLogic ctx.vars logic
    Nothing ->
      Right $
        Condition.evaluateCondition
          conditionOperator
          (metricValueForCondition conditionType ctx)
          conditionValue

behaviorTickDebounceKey :: Text -> Text
behaviorTickDebounceKey personId = "ij:bm:tick:" <> personId
