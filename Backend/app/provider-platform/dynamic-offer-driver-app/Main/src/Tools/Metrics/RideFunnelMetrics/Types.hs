{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Metrics.RideFunnelMetrics.Types
  ( HasRideFunnelMetrics,
    RideFunnelMetricsContainer (..),
    registerRideFunnelMetricsContainer,
  )
where

import EulerHS.Prelude
import Kernel.Tools.Metrics.CoreMetrics (DeploymentVersion)
import Kernel.Utils.Common
import Prometheus as P

type HasRideFunnelMetrics m r =
  HasFlowEnv m r ["rideFunnelMetrics" ::: RideFunnelMetricsContainer, "version" ::: DeploymentVersion]

-- | Ride funnel stage counter: search -> estimate -> offer -> accept -> ride_start -> ride_complete
type RideFunnelStageCounterMetric = P.Vector P.Label4 P.Counter

-- | Ride allocation request counter: {city, zone, vehicle_type, result}
type RideAllocationRequestMetric = P.Vector P.Label4 P.Counter

-- | Ride allocation duration histogram: {city, zone, vehicle_type}
type RideAllocationDurationMetric = P.Vector P.Label3 P.Histogram

-- | Driver notification counter: {city, zone, result}
type DriverNotificationMetric = P.Vector P.Label3 P.Counter

-- | Batch driver count histogram: {city, zone, batch_number}
type BatchDriverCountMetric = P.Vector P.Label3 P.Histogram

-- | Reallocation attempt counter: {city, zone, attempt_number}
type ReallocationAttemptMetric = P.Vector P.Label3 P.Counter

-- | Driver pool size at search histogram: {city, zone, vehicle_type}
type DriverPoolSizeMetric = P.Vector P.Label3 P.Histogram

data RideFunnelMetricsContainer = RideFunnelMetricsContainer
  { rideFunnelStageCounter :: RideFunnelStageCounterMetric,
    rideAllocationRequestCounter :: RideAllocationRequestMetric,
    rideAllocationDurationHist :: RideAllocationDurationMetric,
    driverNotificationCounter :: DriverNotificationMetric,
    batchDriverCountHist :: BatchDriverCountMetric,
    reallocationAttemptCounter :: ReallocationAttemptMetric,
    driverPoolSizeHist :: DriverPoolSizeMetric
  }

registerRideFunnelMetricsContainer :: IO RideFunnelMetricsContainer
registerRideFunnelMetricsContainer = do
  rideFunnelStageCounter <- registerRideFunnelStageCounter
  rideAllocationRequestCounter <- registerRideAllocationRequestCounter
  rideAllocationDurationHist <- registerRideAllocationDurationHist
  driverNotificationCounter <- registerDriverNotificationCounter
  batchDriverCountHist <- registerBatchDriverCountHist
  reallocationAttemptCounter <- registerReallocationAttemptCounter
  driverPoolSizeHist <- registerDriverPoolSizeHist
  return $ RideFunnelMetricsContainer {..}

registerRideFunnelStageCounter :: IO RideFunnelStageCounterMetric
registerRideFunnelStageCounter =
  P.register $
    P.vector ("city", "vehicle_type", "stage", "result") $
      P.counter $
        P.Info
          "ride_funnel_stage_total"
          "Ride funnel stage transitions: search, estimate, offer, accept, ride_start, ride_complete"

registerRideAllocationRequestCounter :: IO RideAllocationRequestMetric
registerRideAllocationRequestCounter =
  P.register $
    P.vector ("city", "zone", "vehicle_type", "result") $
      P.counter $
        P.Info
          "ride_allocation_request_total"
          "Total ride allocation requests by result: matched, timeout, no_driver, cancelled"

registerRideAllocationDurationHist :: IO RideAllocationDurationMetric
registerRideAllocationDurationHist =
  P.register $
    P.vector ("city", "zone", "vehicle_type") $
      P.histogram
        (P.Info "ride_allocation_duration_seconds" "Time from search request to driver match")
        allocationDurationBuckets
  where
    allocationDurationBuckets = [5, 10, 15, 30, 45, 60, 90, 120, 180, 300]

registerDriverNotificationCounter :: IO DriverNotificationMetric
registerDriverNotificationCounter =
  P.register $
    P.vector ("city", "zone", "result") $
      P.counter $
        P.Info
          "ride_driver_notification_total"
          "Driver notifications sent and their outcomes: sent, accepted, rejected, timeout"

registerBatchDriverCountHist :: IO BatchDriverCountMetric
registerBatchDriverCountHist =
  P.register $
    P.vector ("city", "zone", "batch_number") $
      P.histogram
        (P.Info "ride_batch_driver_count" "Number of drivers found per allocation batch")
        batchDriverBuckets
  where
    batchDriverBuckets = [0, 1, 2, 3, 5, 8, 10, 15, 20]

registerReallocationAttemptCounter :: IO ReallocationAttemptMetric
registerReallocationAttemptCounter =
  P.register $
    P.vector ("city", "zone", "attempt_number") $
      P.counter $
        P.Info
          "ride_reallocation_attempt_total"
          "Reallocation attempts after cancellation, by attempt number"

registerDriverPoolSizeHist :: IO DriverPoolSizeMetric
registerDriverPoolSizeHist =
  P.register $
    P.vector ("city", "zone", "vehicle_type") $
      P.histogram
        (P.Info "ride_pool_size_at_search" "Available drivers in pool at time of search request")
        poolSizeBuckets
  where
    poolSizeBuckets = [0, 1, 2, 3, 5, 10, 15, 20, 30, 50, 100]
