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

-- "backend_version" is the deployment version; pooling_{logic,config}_version are the
-- experiment-arm versions assigned at driver-pool computation. They change on different
-- schedules and must stay separate labels.

-- Labels: (merchant, city, distance_bucket, backend_version)
type SearchRequestCounterMetric = P.Vector P.Label4 P.Counter

-- Labels: (merchant, city, vehicle_service_tier, search_repeat_type, distance_bucket, backend_version, pickup_zone, drop_zone)
-- No pooling labels here: INITIAL tries are created BEFORE the first pool computation
-- assigns pooling versions (ensurePoolingLogicVersion), so the label would encode try
-- order ("unknown" for INITIAL, populated for retries), not pooling.
-- pickup_zone/drop_zone are the special-location ids of the ride's ends, "none" when
-- regular — lets Grafana converge the funnel to a special zone (map id->name in Grafana).
type SearchTryCounterMetric = P.Vector P.Label8 P.Counter

-- Labels: (merchant, city, vehicle_service_tier, distance_bucket, pooling_logic_version, pooling_config_version, backend_version)
-- For counters emitted inside the allocation flow, where pooling versions are assigned.
type AllocationFunnelCounterMetric = P.Vector P.Label7 P.Counter

-- Labels: (merchant, city, vehicle_service_tier, distance_bucket, backend_version, pickup_zone, drop_zone)
-- pickup_zone/drop_zone: special-location id of each ride end, "none" when regular.
type RideFunnelCounterMetric = P.Vector P.Label7 P.Counter

-- Labels: (merchant, city, vehicle_service_tier, cancellation_source, distance_bucket, backend_version, pickup_zone, drop_zone)
type RideCancelledCounterMetric = P.Vector P.Label8 P.Counter

-- Labels: (merchant, city, vehicle_service_tier, distance_bucket, backend_version, stage)
-- Deliberately NO pickup_zone/drop_zone here. These are histograms, so every label
-- combination costs one series *per bucket* (41 for the linear ones). The zone pair is an
-- unbounded cross-product (~436 pickup x ~483 drop ids seen in prod) that a pod keeps
-- discovering for its whole life, which grew /metrics past vmagent's 100MB
-- -promscrape.maxScrapeSize after ~7-8h of uptime. vmagent then drops the *entire* scrape,
-- so every BPP_* series from that pod vanishes -- not just these histograms.
-- Zone breakdowns stay on the plain counters above, where they cost one series each.
type RideValueHistogram = P.Vector P.Label6 P.Histogram

-- Labels: (merchant, city, vehicle_service_tier, acceptance_flow, distance_bucket, backend_version, pickup_zone, drop_zone)
-- acceptance_flow = "normal" (Beckn select) | "special_zone" (OTP/special-zone rides that skip
-- select and fire acceptance at Init). special_zone acceptances have no matching search_try,
-- so tries-based ratios must exclude them.
type RiderAcceptanceCounterMetric = P.Vector P.Label8 P.Counter

data BPPMetricsContainer = BPPMetricsContainer
  { searchDurationTimeout :: Seconds,
    searchDuration :: SearchDurationMetric,
    countingDeviation :: CountingDeviationMetric,
    searchRequestCounter :: SearchRequestCounterMetric,
    searchTryCounter :: SearchTryCounterMetric,
    searchRequestSentToDriverCounter :: AllocationFunnelCounterMetric,
    searchRequestExpiredCounter :: AllocationFunnelCounterMetric,
    riderAcceptanceCounter :: RiderAcceptanceCounterMetric,
    bookingCreatedCounter :: RideFunnelCounterMetric,
    rideCreatedCounter :: RideFunnelCounterMetric,
    rideStartedCounter :: RideFunnelCounterMetric,
    rideCompletedCounter :: RideFunnelCounterMetric,
    rideCancelledCounter :: RideCancelledCounterMetric,
    pricePerKmHist :: RideValueHistogram,
    congestionChargeHist :: RideValueHistogram,
    rideDistanceHist :: RideValueHistogram,
    pickupDistanceHist :: RideValueHistogram
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
  riderAcceptanceCounter <- registerRiderAcceptanceCounter
  bookingCreatedCounter <- registerRideFunnelCounter "BPP_booking_created_count" "Count of bookings confirmed on the BPP"
  rideCreatedCounter <- registerRideFunnelCounter "BPP_ride_created_count" "Count of rides created (driver assigned to booking)"
  rideStartedCounter <- registerRideFunnelCounter "BPP_ride_started_count" "Count of rides started"
  rideCompletedCounter <- registerRideFunnelCounter "BPP_ride_completed_count" "Count of rides completed"
  rideCancelledCounter <- registerRideCancelledCounter
  pricePerKmHist <- registerRideValueHistogram "BPP_price_per_km" "Fare per km (INR/km) by stage: pre_ride = estimated at booking creation (all bookings); completed = realised at ride end (completed rides)" (P.linearBuckets 0 5 40)
  congestionChargeHist <- registerRideValueHistogram "BPP_congestion_charge" "Congestion charge (INR) by stage: pre_ride = estimated at booking creation; completed = realised at ride end. Observed only when a congestion charge is present" (P.linearBuckets 0 5 40)
  rideDistanceHist <- registerRideValueHistogram "BPP_ride_distance_meters" "Trip distance (meters) by stage: pre_ride = estimated at booking creation (all bookings); completed = chargeable distance at ride end (completed rides)" (P.exponentialBuckets 500 2 12)
  pickupDistanceHist <- registerRideValueHistogram "BPP_pickup_distance_meters" "Assigned driver distance to pickup (meters) by stage: pre_ride = at ride assignment (all assigned rides); completed = at ride end (completed rides)" (P.exponentialBuckets 100 2 12)
  return $ BPPMetricsContainer {..}

registerSearchRequestCounter :: IO SearchRequestCounterMetric
registerSearchRequestCounter =
  P.register . P.vector ("merchant", "city", "distance_bucket", "backend_version") . P.counter $
    P.Info "BPP_search_request_count" "Count of search requests received by the BPP"

registerSearchTryCounter :: IO SearchTryCounterMetric
registerSearchTryCounter =
  P.register . P.vector ("merchant", "city", "vehicle_service_tier", "search_repeat_type", "distance_bucket", "backend_version", "pickup_zone", "drop_zone") . P.counter $
    P.Info "BPP_search_try_count" "Count of search tries (driver allocation attempts) created"

registerAllocationFunnelCounter :: Text -> Text -> IO AllocationFunnelCounterMetric
registerAllocationFunnelCounter name description =
  P.register . P.vector ("merchant", "city", "vehicle_service_tier", "distance_bucket", "pooling_logic_version", "pooling_config_version", "backend_version") . P.counter $
    P.Info name description

registerRideFunnelCounter :: Text -> Text -> IO RideFunnelCounterMetric
registerRideFunnelCounter name description =
  P.register . P.vector ("merchant", "city", "vehicle_service_tier", "distance_bucket", "backend_version", "pickup_zone", "drop_zone") . P.counter $
    P.Info name description

registerRiderAcceptanceCounter :: IO RiderAcceptanceCounterMetric
registerRiderAcceptanceCounter =
  P.register . P.vector ("merchant", "city", "vehicle_service_tier", "acceptance_flow", "distance_bucket", "backend_version", "pickup_zone", "drop_zone") . P.counter $
    P.Info "BPP_rider_acceptance_count" "Count of rider fare acceptances, by acceptance_flow (normal = Beckn select; special_zone = OTP/special-zone rides that skip select and fire at Init)"

registerRideCancelledCounter :: IO RideCancelledCounterMetric
registerRideCancelledCounter =
  P.register . P.vector ("merchant", "city", "vehicle_service_tier", "cancellation_source", "distance_bucket", "backend_version", "pickup_zone", "drop_zone") . P.counter $
    P.Info "BPP_ride_cancelled_count" "Count of bookings cancelled, labelled by cancellation source"

registerRideValueHistogram :: Text -> Text -> [Double] -> IO RideValueHistogram
registerRideValueHistogram name description buckets =
  P.register . P.vector ("merchant", "city", "vehicle_service_tier", "distance_bucket", "backend_version", "stage") $
    P.histogram (P.Info name description) buckets

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
