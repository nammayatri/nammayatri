{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.SessionizerMetrics.Prometheus.Metrics
  ( -- * Histograms
    observeExternalAPILatency,
    observePaymentLatency,
    observeSearchE2ELatency,
    observeAllocationLatency,
    observeDbQueryLatency,

    -- * Counters
    incrementAllocationCounter,
    incrementPaymentCounter,
    incrementCircuitBreakerTransition,
    incrementSlowQueryCounter,

    -- * Gauges
    incrementActiveRides,
    decrementActiveRides,
    setActiveRides,
    incrementOnlineDrivers,
    decrementOnlineDrivers,
    setOnlineDrivers,
    setConnectionPoolUtilization,
  )
where

import Kernel.Prelude
import qualified Prometheus as P
import System.IO.Unsafe (unsafePerformIO)

-- ---------------------------------------------------------------------------
-- Histograms
-- ---------------------------------------------------------------------------

-- | External API call latency by dependency and endpoint.
{-# NOINLINE externalAPILatency #-}
externalAPILatency :: P.Vector P.Label2 P.Histogram
externalAPILatency = unsafePerformIO $
  P.register $
    P.vector ("dependency", "endpoint") $
      P.histogram (P.Info "external_api_call_latency_seconds" "Latency of external API calls") $
        P.exponentialBuckets 0.001 2 15

-- | Payment processing latency by merchant and payment type.
{-# NOINLINE paymentLatency #-}
paymentLatency :: P.Vector P.Label2 P.Histogram
paymentLatency = unsafePerformIO $
  P.register $
    P.vector ("merchant_id", "payment_type") $
      P.histogram (P.Info "payment_processing_latency_seconds" "Payment processing latency") $
        P.exponentialBuckets 0.01 2 12

-- | End-to-end search latency by merchant and city.
{-# NOINLINE searchE2ELatencyHist #-}
searchE2ELatencyHist :: P.Vector P.Label2 P.Histogram
searchE2ELatencyHist = unsafePerformIO $
  P.register $
    P.vector ("merchant_id", "city_id") $
      P.histogram (P.Info "search_e2e_latency_seconds" "End-to-end search latency") $
        P.linearBuckets 0 0.5 40

-- | Driver allocation latency by merchant and city.
{-# NOINLINE allocationLatencyHist #-}
allocationLatencyHist :: P.Vector P.Label2 P.Histogram
allocationLatencyHist = unsafePerformIO $
  P.register $
    P.vector ("merchant_id", "city_id") $
      P.histogram (P.Info "allocation_latency_seconds" "Driver allocation latency") $
        P.linearBuckets 0 1.0 30

-- | Database query latency by table and operation.
{-# NOINLINE dbQueryLatencyHist #-}
dbQueryLatencyHist :: P.Vector P.Label2 P.Histogram
dbQueryLatencyHist = unsafePerformIO $
  P.register $
    P.vector ("table_name", "operation") $
      P.histogram (P.Info "db_query_latency_seconds" "Database query latency") $
        P.exponentialBuckets 0.001 2 15

-- ---------------------------------------------------------------------------
-- Counters
-- ---------------------------------------------------------------------------

-- | Allocation attempts by result (success/failure/no_drivers).
{-# NOINLINE allocationTotal #-}
allocationTotal :: P.Vector P.Label3 P.Counter
allocationTotal = unsafePerformIO $
  P.register $
    P.vector ("merchant_id", "city_id", "result") $
      P.counter (P.Info "allocation_total" "Total allocation attempts by result")

-- | Payment attempts by result (success/failure).
{-# NOINLINE paymentTotal #-}
paymentTotal :: P.Vector P.Label3 P.Counter
paymentTotal = unsafePerformIO $
  P.register $
    P.vector ("merchant_id", "payment_type", "result") $
      P.counter (P.Info "payment_total" "Total payment attempts by result")

-- | Circuit breaker state transitions.
{-# NOINLINE cbTransitions #-}
cbTransitions :: P.Vector P.Label2 P.Counter
cbTransitions = unsafePerformIO $
  P.register $
    P.vector ("service", "to_state") $
      P.counter (P.Info "circuit_breaker_transitions_total" "Circuit breaker state transitions")

-- | Slow query counter by table and operation.
{-# NOINLINE slowQueryTotal #-}
slowQueryTotal :: P.Vector P.Label2 P.Counter
slowQueryTotal = unsafePerformIO $
  P.register $
    P.vector ("table_name", "operation") $
      P.counter (P.Info "db_slow_queries_total" "Total slow database queries (>100ms)")

-- ---------------------------------------------------------------------------
-- Gauges
-- ---------------------------------------------------------------------------

-- | Currently active rides by merchant and city.
{-# NOINLINE activeRidesGauge #-}
activeRidesGauge :: P.Vector P.Label2 P.Gauge
activeRidesGauge = unsafePerformIO $
  P.register $
    P.vector ("merchant_id", "city_id") $
      P.gauge (P.Info "active_rides_count" "Currently active rides")

-- | Currently online drivers by merchant and city.
{-# NOINLINE onlineDriversGauge #-}
onlineDriversGauge :: P.Vector P.Label2 P.Gauge
onlineDriversGauge = unsafePerformIO $
  P.register $
    P.vector ("merchant_id", "city_id") $
      P.gauge (P.Info "online_drivers_count" "Currently online drivers")

-- | Connection pool utilization by pool name and metric type.
{-# NOINLINE connPoolGauge #-}
connPoolGauge :: P.Vector P.Label2 P.Gauge
connPoolGauge = unsafePerformIO $
  P.register $
    P.vector ("pool_name", "metric_type") $
      P.gauge (P.Info "connection_pool_utilization" "Connection pool utilization metrics")

-- ---------------------------------------------------------------------------
-- Helper functions
-- ---------------------------------------------------------------------------

observeExternalAPILatency :: MonadIO m => Text -> Text -> Double -> m ()
observeExternalAPILatency dependency endpoint duration =
  liftIO $ P.withLabel externalAPILatency (dependency, endpoint) (`P.observe` duration)

observePaymentLatency :: MonadIO m => Text -> Text -> Double -> m ()
observePaymentLatency merchantId paymentType duration =
  liftIO $ P.withLabel paymentLatency (merchantId, paymentType) (`P.observe` duration)

observeSearchE2ELatency :: MonadIO m => Text -> Text -> Double -> m ()
observeSearchE2ELatency merchantId cityId duration =
  liftIO $ P.withLabel searchE2ELatencyHist (merchantId, cityId) (`P.observe` duration)

observeAllocationLatency :: MonadIO m => Text -> Text -> Double -> m ()
observeAllocationLatency merchantId cityId duration =
  liftIO $ P.withLabel allocationLatencyHist (merchantId, cityId) (`P.observe` duration)

observeDbQueryLatency :: MonadIO m => Text -> Text -> Double -> m ()
observeDbQueryLatency tableName operation duration =
  liftIO $ P.withLabel dbQueryLatencyHist (tableName, operation) (`P.observe` duration)

incrementAllocationCounter :: MonadIO m => Text -> Text -> Text -> m ()
incrementAllocationCounter merchantId cityId result =
  liftIO $ P.withLabel allocationTotal (merchantId, cityId, result) P.incCounter

incrementPaymentCounter :: MonadIO m => Text -> Text -> Text -> m ()
incrementPaymentCounter merchantId paymentType result =
  liftIO $ P.withLabel paymentTotal (merchantId, paymentType, result) P.incCounter

incrementCircuitBreakerTransition :: MonadIO m => Text -> Text -> m ()
incrementCircuitBreakerTransition service toState =
  liftIO $ P.withLabel cbTransitions (service, toState) P.incCounter

incrementSlowQueryCounter :: MonadIO m => Text -> Text -> m ()
incrementSlowQueryCounter tableName operation =
  liftIO $ P.withLabel slowQueryTotal (tableName, operation) P.incCounter

incrementActiveRides :: MonadIO m => Text -> Text -> m ()
incrementActiveRides merchantId cityId =
  liftIO $ P.withLabel activeRidesGauge (merchantId, cityId) P.incGauge

decrementActiveRides :: MonadIO m => Text -> Text -> m ()
decrementActiveRides merchantId cityId =
  liftIO $ P.withLabel activeRidesGauge (merchantId, cityId) P.decGauge

setActiveRides :: MonadIO m => Text -> Text -> Double -> m ()
setActiveRides merchantId cityId count =
  liftIO $ P.withLabel activeRidesGauge (merchantId, cityId) (`P.setGauge` count)

incrementOnlineDrivers :: MonadIO m => Text -> Text -> m ()
incrementOnlineDrivers merchantId cityId =
  liftIO $ P.withLabel onlineDriversGauge (merchantId, cityId) P.incGauge

decrementOnlineDrivers :: MonadIO m => Text -> Text -> m ()
decrementOnlineDrivers merchantId cityId =
  liftIO $ P.withLabel onlineDriversGauge (merchantId, cityId) P.decGauge

setOnlineDrivers :: MonadIO m => Text -> Text -> Double -> m ()
setOnlineDrivers merchantId cityId count =
  liftIO $ P.withLabel onlineDriversGauge (merchantId, cityId) (`P.setGauge` count)

setConnectionPoolUtilization :: MonadIO m => Text -> Text -> Double -> m ()
setConnectionPoolUtilization poolName metricType value =
  liftIO $ P.withLabel connPoolGauge (poolName, metricType) (`P.setGauge` value)
