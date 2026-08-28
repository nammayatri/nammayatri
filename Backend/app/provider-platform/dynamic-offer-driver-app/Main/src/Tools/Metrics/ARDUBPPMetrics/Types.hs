{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Metrics.ARDUBPPMetrics.Types
  ( HasBPPMetrics,
    BPPMetricsContainer (..),
    module CoreMetrics,
    registerBPPMetricsContainer,
  )
where

import EulerHS.Prelude
import Kernel.Tools.Metrics.CoreMetrics as CoreMetrics
import Kernel.Utils.Common
import Prometheus as P

type HasBPPMetrics m r = (HasFlowEnv m r ["bppMetrics" ::: BPPMetricsContainer, "version" ::: DeploymentVersion])

type SearchDurationMetric = (P.Vector P.Label2 P.Histogram, P.Vector P.Label2 P.Counter)

-- Label values are human-readable: merchant = merchant shortId, city = operating city name
-- (resolved via SharedLogic.MetricsLabels.getMetricsLabels at the call sites).

-- Labels: (merchant, city, version)
type SearchRequestCounterMetric = P.Vector P.Label3 P.Counter

-- Labels: (merchant, city, vehicle_service_tier, search_repeat_type, version)
type SearchTryCounterMetric = P.Vector P.Label5 P.Counter

-- Labels: (merchant, city, vehicle_service_tier, version)
type RideFunnelCounterMetric = P.Vector P.Label4 P.Counter

-- Labels: (merchant, city, vehicle_service_tier, cancellation_source, version)
type RideCancelledCounterMetric = P.Vector P.Label5 P.Counter

data BPPMetricsContainer = BPPMetricsContainer
  { searchDurationTimeout :: Seconds,
    searchDuration :: SearchDurationMetric,
    countingDeviation :: CountingDeviationMetric,
    searchRequestCounter :: SearchRequestCounterMetric,
    searchTryCounter :: SearchTryCounterMetric,
    searchRequestSentToDriverCounter :: AllocationFunnelCounterMetric,
    searchRequestExpiredCounter :: AllocationFunnelCounterMetric,
    riderAcceptanceCounter :: RideFunnelCounterMetric,
    bookingCreatedCounter :: RideFunnelCounterMetric,
    rideCreatedCounter :: RideFunnelCounterMetric,
    rideStartedCounter :: RideFunnelCounterMetric,
    rideCompletedCounter :: RideFunnelCounterMetric,
    rideCancelledCounter :: RideCancelledCounterMetric
  }

data CountingDeviationMetric = CountingDeviationMetric
  { realFareDeviation :: P.Vector P.Label2 P.Histogram,
    realDistanceDeviation :: P.Vector P.Label2 P.Histogram
  }

registerBPPMetricsContainer :: Seconds -> IO BPPMetricsContainer
registerBPPMetricsContainer searchDurationTimeout = do
  searchDuration <- registerSearchDurationMetric searchDurationTimeout
  countingDeviation <- registerCountingDeviationMetric
  searchRequestCounter <- registerSearchRequestCounter
  searchTryCounter <- registerSearchTryCounter
  searchRequestSentToDriverCounter <- registerAllocationFunnelCounter "BPP_search_request_sent_to_driver_count" "Count of search requests fanned out to drivers, batched per driver"
  searchRequestExpiredCounter <- registerAllocationFunnelCounter "BPP_search_request_expired_count" "Count of driver search requests retracted without any driver response"
  riderAcceptanceCounter <- registerRideFunnelCounter "BPP_rider_acceptance_count" "Count of rider fare acceptances (Beckn select; normal flow only, OTP/special-zone rides skip select)"
  bookingCreatedCounter <- registerRideFunnelCounter "BPP_booking_created_count" "Count of bookings confirmed on the BPP"
  rideCreatedCounter <- registerRideFunnelCounter "BPP_ride_created_count" "Count of rides created (driver assigned to booking)"
  rideStartedCounter <- registerRideFunnelCounter "BPP_ride_started_count" "Count of rides started"
  rideCompletedCounter <- registerRideFunnelCounter "BPP_ride_completed_count" "Count of rides completed"
  rideCancelledCounter <- registerRideCancelledCounter
  return $ BPPMetricsContainer {..}

registerSearchRequestCounter :: IO SearchRequestCounterMetric
registerSearchRequestCounter =
  P.register . P.vector ("merchant", "city", "version") . P.counter $
    P.Info "BPP_search_request_count" "Count of search requests received by the BPP"

registerSearchTryCounter :: IO SearchTryCounterMetric
registerSearchTryCounter =
  P.register . P.vector ("merchant", "city", "vehicle_service_tier", "search_repeat_type", "version") . P.counter $
    P.Info "BPP_search_try_count" "Count of search tries (driver allocation attempts) created"

registerRideFunnelCounter :: Text -> Text -> IO RideFunnelCounterMetric
registerRideFunnelCounter name description =
  P.register . P.vector ("merchant", "city", "vehicle_service_tier", "version") . P.counter $
    P.Info name description

registerRideCancelledCounter :: IO RideCancelledCounterMetric
registerRideCancelledCounter =
  P.register . P.vector ("merchant", "city", "vehicle_service_tier", "cancellation_source", "version") . P.counter $
    P.Info "BPP_ride_cancelled_count" "Count of bookings cancelled, labelled by cancellation source"

registerCountingDeviationMetric :: IO CountingDeviationMetric
registerCountingDeviationMetric =
  CountingDeviationMetric
    <$> (P.register . P.vector ("agency_name", "version") $ P.histogram fareDeviation $ aroundZero 10 5)
    <*> (P.register . P.vector ("agency_name", "version") $ P.histogram distanceDeviation $ aroundZero 10 6)
  where
    aroundZero factor b =
      let l = P.exponentialBuckets 1 factor b
       in reverse (map negate l) ++ l
    fareDeviation =
      P.Info
        "BPP_fare_deviation"
        "Difference between initially offered and recalculated fare of a ride"
    distanceDeviation =
      P.Info
        "BPP_distance_deviation"
        "Difference between estimated distance and real distance of a ride"

registerSearchDurationMetric :: Seconds -> IO SearchDurationMetric
registerSearchDurationMetric searchDurationTimeout = do
  searchDurationHistogram <-
    P.register $
      P.vector ("agency_name", "version") $
        P.histogram
          infoSearchDuration
          buckets
  failureCounter <-
    P.register $
      P.vector ("agency_name", "version") $
        P.counter $ P.Info "BPP_search_failure_counter" ""

  pure (searchDurationHistogram, failureCounter)
  where
    infoSearchDuration =
      P.Info
        "BPP_search_time"
        ""
    buckets =
      P.linearBuckets
        0
        0.5
        searchDurationBucketCount
    searchDurationBucketCount = (getSeconds searchDurationTimeout + 1) * 2
