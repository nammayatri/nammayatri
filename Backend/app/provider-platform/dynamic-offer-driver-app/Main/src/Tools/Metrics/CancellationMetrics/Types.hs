{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Metrics.CancellationMetrics.Types
  ( HasCancellationMetrics,
    CancellationMetricsContainer (..),
    registerCancellationMetricsContainer,
  )
where

import EulerHS.Prelude
import Kernel.Tools.Metrics.CoreMetrics (DeploymentVersion)
import Kernel.Utils.Common
import Prometheus as P

type HasCancellationMetrics m r =
  HasFlowEnv m r ["cancellationMetrics" ::: CancellationMetricsContainer, "version" ::: DeploymentVersion]

-- | Cancellation counter: {city, cancelled_by, reason, vehicle_type}
-- cancelled_by: "rider" | "driver" | "system"
-- reason: "found_cheaper" | "long_wait" | "wrong_address" | "short_distance" | "wrong_direction" | "other"
type CancellationCounterMetric = P.Vector P.Label4 P.Counter

-- | Post-acceptance cancellation: {city, zone, cancelled_by, minutes_after_bucket}
-- minutes_after_bucket: "0-2" | "2-5" | "5-10" | "10+"
type PostAcceptanceCancellationMetric = P.Vector P.Label4 P.Counter

-- | Passive cancellation detection: {city, zone}
type PassiveCancellationMetric = P.Vector P.Label2 P.Counter

-- | Reallocation success: {city, zone, result}
-- result: "matched" | "timeout" | "rider_cancelled"
type ReallocationSuccessMetric = P.Vector P.Label3 P.Counter

-- | Reallocation duration: {city, zone}
type ReallocationDurationMetric = P.Vector P.Label2 P.Histogram

-- | Cancellation penalty applied: {city, vehicle_type, penalty_type}
type CancellationPenaltyMetric = P.Vector P.Label3 P.Counter

data CancellationMetricsContainer = CancellationMetricsContainer
  { cancellationCounter :: CancellationCounterMetric,
    postAcceptanceCancellationCounter :: PostAcceptanceCancellationMetric,
    passiveCancellationCounter :: PassiveCancellationMetric,
    reallocationSuccessCounter :: ReallocationSuccessMetric,
    reallocationDurationHist :: ReallocationDurationMetric,
    cancellationPenaltyCounter :: CancellationPenaltyMetric
  }

registerCancellationMetricsContainer :: IO CancellationMetricsContainer
registerCancellationMetricsContainer = do
  cancellationCounter <- registerCancellationCounter
  postAcceptanceCancellationCounter <- registerPostAcceptanceCancellationCounter
  passiveCancellationCounter <- registerPassiveCancellationCounter
  reallocationSuccessCounter <- registerReallocationSuccessCounter
  reallocationDurationHist <- registerReallocationDurationHist
  cancellationPenaltyCounter <- registerCancellationPenaltyCounter
  return $ CancellationMetricsContainer {..}

registerCancellationCounter :: IO CancellationCounterMetric
registerCancellationCounter =
  P.register $
    P.vector ("city", "cancelled_by", "reason", "vehicle_type") $
      P.counter $
        P.Info
          "ride_cancellation_total"
          "Total ride cancellations by city, who cancelled, reason, and vehicle type"

registerPostAcceptanceCancellationCounter :: IO PostAcceptanceCancellationMetric
registerPostAcceptanceCancellationCounter =
  P.register $
    P.vector ("city", "zone", "cancelled_by", "minutes_after_bucket") $
      P.counter $
        P.Info
          "ride_cancellation_after_acceptance_total"
          "Cancellations after driver acceptance, bucketed by time elapsed"

registerPassiveCancellationCounter :: IO PassiveCancellationMetric
registerPassiveCancellationCounter =
  P.register $
    P.vector ("city", "zone") $
      P.counter $
        P.Info
          "ride_passive_cancellation_total"
          "Detected passive cancellations where driver forced rider to cancel"

registerReallocationSuccessCounter :: IO ReallocationSuccessMetric
registerReallocationSuccessCounter =
  P.register $
    P.vector ("city", "zone", "result") $
      P.counter $
        P.Info
          "reallocation_success_total"
          "Reallocation outcomes after cancellation: matched, timeout, rider_cancelled"

registerReallocationDurationHist :: IO ReallocationDurationMetric
registerReallocationDurationHist =
  P.register $
    P.vector ("city", "zone") $
      P.histogram
        (P.Info "reallocation_duration_seconds" "Time taken for reallocation after cancellation")
        reallocationBuckets
  where
    reallocationBuckets = [5, 10, 15, 30, 45, 60, 90, 120, 180, 300]

registerCancellationPenaltyCounter :: IO CancellationPenaltyMetric
registerCancellationPenaltyCounter =
  P.register $
    P.vector ("city", "vehicle_type", "penalty_type") $
      P.counter $
        P.Info
          "cancellation_penalty_applied_total"
          "Cancellation penalties applied by type: fee, block, warning"
