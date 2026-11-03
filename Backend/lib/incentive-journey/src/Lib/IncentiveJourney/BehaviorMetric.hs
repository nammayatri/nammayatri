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
    progressiveMergeMetricContext,
    metricContextFromCurrentMetrics,
    applyRideDeltasToContext,
    metricValueForCondition,
    evaluateBehaviorCondition,
    enrichWithDerivedBehaviorRates,
    behaviorTickDebounceKey,
    rrReceivedPendingSyncKey,
    rrReceivedStatsSyncThreshold,
    varRidesCompleted,
    varEarnings,
    varDistanceMeters,
    varRideTimeSeconds,
    varCancellationRate,
    varAcceptanceRate,
    varOnlineMinutesTotal,
    varRrReceived,
    varRrAccepted,
    varDxCancel,
    varDafCancel,
  )
where

import Data.Aeson (Value, object, withObject, (.:), (.:?), (.=))
import qualified Data.HashMap.Strict as HM
import Data.List (nub)
import Kernel.Prelude
import qualified Lib.IncentiveJourney.Condition as Condition
import qualified Lib.IncentiveJourney.Evaluation as Evaluation
import qualified Lib.IncentiveJourney.JsonLogic as JsonLogic
import Lib.IncentiveJourney.Types

-- | Absolute counters / rates for JsonLogic (and BehaviourMetric simple columns).
-- Ride + behaviour vars share this bag — same level.
data MetricContext = MetricContext
  { vars :: HM.HashMap Text Int
  }
  deriving (Eq, Show, Generic)

-- | Stream/Kafka tick: stats-sync only (no mid-period reward).
-- cjmIds / metricName / delta optional for backward compat with old publishers.
data BehaviorMetricTick = BehaviorMetricTick
  { personId :: Text,
    merchantId :: Text,
    merchantOperatingCityId :: Text,
    actionType :: Text,
    occurredAt :: UTCTime,
    cjmIds :: Maybe [Text],
    metricName :: Maybe Text,
    delta :: Maybe Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON BehaviorMetricTick where
  toJSON BehaviorMetricTick {..} =
    object
      [ "personId" .= personId,
        "merchantId" .= merchantId,
        "merchantOperatingCityId" .= merchantOperatingCityId,
        "actionType" .= actionType,
        "occurredAt" .= occurredAt,
        "cjmIds" .= cjmIds,
        "metricName" .= metricName,
        "delta" .= delta
      ]

instance FromJSON BehaviorMetricTick where
  parseJSON = withObject "BehaviorMetricTick" $ \o ->
    BehaviorMetricTick
      <$> o .: "personId"
      <*> o .: "merchantId"
      <*> o .: "merchantOperatingCityId"
      <*> o .: "actionType"
      <*> o .: "occurredAt"
      <*> o .:? "cjmIds"
      <*> o .:? "metricName"
      <*> o .:? "delta"

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

varRrReceived :: Text
varRrReceived = "rrReceived"

varRrAccepted :: Text
varRrAccepted = "rrAccepted"

varDxCancel :: Text
varDxCancel = "dxCancel"

varDafCancel :: Text
varDafCancel = "dafCancel"

mergeMetricContext :: MetricContext -> MetricContext -> MetricContext
mergeMetricContext base overlay =
  MetricContext {vars = HM.union overlay.vars base.vars}

-- | Prefer the higher counter for each var (stats vs Redis). Rates/keys only in Redis keep Redis.
-- Prevents wiped Redis zeros from under-awarding when stats.currentMetrics already progressed.
-- Rate keys are excluded from max-merge and re-derived from period counts afterward.
progressiveMergeMetricContext :: MetricContext -> MetricContext -> MetricContext
progressiveMergeMetricContext statsCtx redisCtx =
  let keys =
        filter (not . isDerivedRateKey) $
          HM.keys statsCtx.vars <> HM.keys redisCtx.vars
      pick k = max (HM.lookupDefault 0 k statsCtx.vars) (HM.lookupDefault 0 k redisCtx.vars)
   in enrichWithDerivedBehaviorRates $
        MetricContext {vars = HM.fromList [(k, pick k) | k <- nub keys]}

isDerivedRateKey :: Text -> Bool
isDerivedRateKey k = k == varCancellationRate || k == varAcceptanceRate

-- | acceptanceRate = rrAccepted*100/rrReceived; cancellationRate = (dx+daf)*100/rrAccepted.
-- Period / timebound scoped via the counts already in the bag (not city BT).
enrichWithDerivedBehaviorRates :: MetricContext -> MetricContext
enrichWithDerivedBehaviorRates ctx =
  let rrRecv = HM.lookupDefault 0 varRrReceived ctx.vars
      rrAcc = HM.lookupDefault 0 varRrAccepted ctx.vars
      cancels = HM.lookupDefault 0 varDxCancel ctx.vars + HM.lookupDefault 0 varDafCancel ctx.vars
      acceptRate = if rrRecv <= 0 then 0 else (rrAcc * 100) `div` rrRecv
      cancelRate =
        if rrAcc <= 0
          then if cancels <= 0 then 0 else 100
          else (cancels * 100) `div` rrAcc
   in MetricContext
        { vars =
            HM.insert varCancellationRate cancelRate $
              HM.insert varAcceptanceRate acceptRate ctx.vars
        }

-- | Lift stats.currentMetrics (Inside/Outside + bag keys) into a MetricContext for settle/eval.
metricContextFromCurrentMetrics :: Maybe Value -> MetricContext
metricContextFromCurrentMetrics mbMetrics =
  let online = Evaluation.lookupMetricInt mbMetrics mempty "onlineMinutes"
      rrRecv = Evaluation.lookupMetricInt mbMetrics mempty "rrReceived"
      rrAcc = Evaluation.lookupMetricInt mbMetrics mempty "rrAccepted"
      dx = Evaluation.lookupMetricInt mbMetrics mempty "dxCancel"
      daf = Evaluation.lookupMetricInt mbMetrics mempty "dafCancel"
      rides = Evaluation.lookupMetricInt mbMetrics mempty varRidesCompleted
      earningsInside = maybe 0 round (Evaluation.lookupNumber mbMetrics Evaluation.earningInsideKey)
      earningsOutside = maybe 0 round (Evaluation.lookupNumber mbMetrics Evaluation.earningOutsideKey)
      earnings = Evaluation.lookupMetricInt mbMetrics mempty varEarnings
      earningsTotal = max earnings (earningsInside + earningsOutside)
      distance = Evaluation.lookupMetricInt mbMetrics mempty varDistanceMeters
      rideTime = Evaluation.lookupMetricInt mbMetrics mempty varRideTimeSeconds
   in enrichWithDerivedBehaviorRates $
        MetricContext
          { vars =
              HM.fromList
                [ (varOnlineMinutesTotal, online),
                  (varRrReceived, rrRecv),
                  (varRrAccepted, rrAcc),
                  (varDxCancel, dx),
                  (varDafCancel, daf),
                  (varRidesCompleted, rides),
                  (varEarnings, earningsTotal),
                  (varDistanceMeters, distance),
                  (varRideTimeSeconds, rideTime)
                ]
          }

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
    JSONLogic -> 0

-- | Award/progress gate: evaluation.logic (or JSONLogic type) is source of truth; else simple columns.
-- False logic → no-op in Evaluate (no disqualify).
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
    Nothing
      | conditionType == JSONLogic ->
        Left "JSONLogic conditionType requires evaluation.logic"
      | otherwise ->
        Right $
          Condition.evaluateCondition
            conditionOperator
            (metricValueForCondition conditionType ctx)
            conditionValue

behaviorTickDebounceKey :: Text -> Text
behaviorTickDebounceKey personId = "ij:bm:tick:" <> personId

-- | Pending rrReceived delta not yet flushed to stats via Kafka (per driver).
rrReceivedPendingSyncKey :: Text -> Text
rrReceivedPendingSyncKey personId = "ij:bm:pending:rrReceived:" <> personId

-- | Flush Kafka/stats sync after this many Redis rrReceived bumps (hot path).
-- Redis INCR still runs every request; only the stats consumer is batched.
rrReceivedStatsSyncThreshold :: Int
rrReceivedStatsSyncThreshold = 10
