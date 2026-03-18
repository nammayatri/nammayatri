{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Metrics.BAPMetrics.Types
  ( HasBAPMetrics,
    BAPMetricsContainer (..),
    registerBAPMetricsContainer,
    registerDurationMetricFRFS,
  )
where

import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (DeploymentVersion)
import Kernel.Utils.Common
import Prometheus as P

type HasBAPMetrics m r = HasFlowEnv m r ["bapMetrics" ::: BAPMetricsContainer, "version" ::: DeploymentVersion]

data BAPMetricsContainer = BAPMetricsContainer
  { searchRequestCounter :: SearchRequestCounterMetric,
    rideCreatedCounter :: RideCreatedCounterMetric,
    busScanSearchRequestCounter :: BusScanSearchRequestCounterMetric,
    searchDurationTimeout :: Seconds,
    searchDuration :: SearchDurationMetric,
    searchDurationFRFS :: DurationMetric,
    selectDurationFRFS :: DurationMetric,
    initDurationFRFS :: DurationMetric,
    confirmDurationFRFS :: DurationMetric,
    cancelDurationFRFS :: DurationMetric,
    createOrderDurationFRFS :: DurationMetric,
    initDuration :: DurationMetric,
    confirmDuration :: DurationMetric,
    busScannerCounter :: BusScannetCounterMetric,
    fleetRouteMapMissingCounter :: FleetRouteMapMissingCounterMetric,
    vehicleNoEtaCounter :: VehicleNoEtaCounterMetric,
    -- Payment journey metrics (Fix #22)
    paymentAttemptCounter :: PaymentAttemptCounterMetric,
    paymentPageLoadDuration :: PaymentPageLoadDurationMetric,
    hyperSDKInitCounter :: HyperSDKInitCounterMetric,
    -- Booking funnel metrics (Fix #23)
    bookingStageCounter :: BookingStageCounterMetric,
    bookingStageDuration :: BookingStageDurationMetric,
    bookingE2ECounter :: BookingE2ECounterMetric,
    -- QR scan metrics (Fix #24)
    qrPayloadSize :: QRPayloadSizeMetric,
    qrGenerationCounter :: QRGenerationCounterMetric,
    qrScanAttemptCounter :: QRScanAttemptCounterMetric
  }

type SearchRequestCounterMetric = P.Vector P.Label3 P.Counter

type BusScannetCounterMetric = P.Vector P.Label4 P.Counter

type FleetRouteMapMissingCounterMetric = P.Vector P.Label4 P.Counter

type RideCreatedCounterMetric = P.Vector P.Label4 P.Counter

type SearchDurationMetric = (P.Vector P.Label2 P.Histogram, P.Vector P.Label2 P.Counter)

type DurationMetric = (P.Vector P.Label3 P.Histogram, P.Vector P.Label3 P.Counter)

type VehicleNoEtaCounterMetric = P.Vector P.Label4 P.Counter

type BusScanSearchRequestCounterMetric = P.Vector P.Label3 P.Counter

type PaymentAttemptCounterMetric = P.Vector P.Label3 P.Counter

type PaymentPageLoadDurationMetric = (P.Vector P.Label2 P.Histogram, P.Vector P.Label2 P.Counter)

type HyperSDKInitCounterMetric = P.Vector P.Label2 P.Counter

type BookingStageCounterMetric = P.Vector P.Label3 P.Counter

type BookingStageDurationMetric = (P.Vector P.Label2 P.Histogram, P.Vector P.Label2 P.Counter)

type BookingE2ECounterMetric = P.Vector P.Label1 P.Counter

type QRPayloadSizeMetric = P.Vector P.Label2 P.Histogram

type QRGenerationCounterMetric = P.Vector P.Label2 P.Counter

type QRScanAttemptCounterMetric = P.Vector P.Label2 P.Counter

registerBAPMetricsContainer :: Seconds -> IO BAPMetricsContainer
registerBAPMetricsContainer searchDurationTimeout = do
  searchRequestCounter <- registerSearchRequestCounterMetric
  busScannerCounter <- registerBusScannetCounterMetric
  fleetRouteMapMissingCounter <- registerFleetRouteMapMissingCounterMetric
  vehicleNoEtaCounter <- registerVehicleNoEtaCounterMetric
  busScanSearchRequestCounter <- registerBusScanSearchRequestCounterMetric
  rideCreatedCounter <- registerRideCreatedCounterMetric
  searchDuration <- registerSearchDurationMetric searchDurationTimeout
  searchDurationFRFS <- registerDurationMetricFRFS searchDurationTimeout "merchant_name" "version" "merchantOperatingCityId" "beckn_search_frfs_round_trip" "beckn_search_frfs_round_trip_failure_counter"
  selectDurationFRFS <- registerDurationMetricFRFS searchDurationTimeout "merchant_name" "version" "merchantOperatingCityId" "beckn_select_frfs_round_trip" "beckn_select_frfs_round_trip_failure_counter"
  initDurationFRFS <- registerDurationMetricFRFS searchDurationTimeout "merchant_name" "version" "merchantOperatingCityId" "beckn_init_frfs_round_trip" "beckn_init_frfs_round_trip_failure_counter"
  confirmDurationFRFS <- registerDurationMetricFRFS searchDurationTimeout "merchant_name" "version" "merchantOperatingCityId" "beckn_confirm_frfs_round_trip" "beckn_confirm_frfs_round_trip_failure_counter"
  cancelDurationFRFS <- registerDurationMetricFRFS searchDurationTimeout "merchant_name" "version" "merchantOperatingCityId" "beckn_cancel_frfs_round_trip" "beckn_cancel_frfs_round_trip_failure_counter"
  initDuration <- registerDurationMetric searchDurationTimeout "merchant_name" "version" "merchantOperatingCityId" "beckn_init_round_trip" "beckn_init_round_trip_failure_counter"
  confirmDuration <- registerDurationMetric searchDurationTimeout "merchant_name" "version" "merchantOperatingCityId" "beckn_confirm_round_trip" "beckn_confirm_round_trip_failure_counter"
  createOrderDurationFRFS <- registerDurationMetricFRFS searchDurationTimeout "merchant_name" "version" "merchantOperatingCityId" "beckn_create_order_frfs_round_trip" "beckn_create_order_frfs_round_trip_failure_counter"
  -- Payment journey metrics (Fix #22)
  paymentAttemptCounter <- registerPaymentAttemptCounterMetric
  paymentPageLoadDuration <- registerPaymentPageLoadDurationMetric searchDurationTimeout
  hyperSDKInitCounter <- registerHyperSDKInitCounterMetric
  -- Booking funnel metrics (Fix #23)
  bookingStageCounter <- registerBookingStageCounterMetric
  bookingStageDuration <- registerBookingStageDurationMetric searchDurationTimeout
  bookingE2ECounter <- registerBookingE2ECounterMetric
  -- QR scan metrics (Fix #24)
  qrPayloadSize <- registerQRPayloadSizeMetric
  qrGenerationCounter <- registerQRGenerationCounterMetric
  qrScanAttemptCounter <- registerQRScanAttemptCounterMetric
  return $ BAPMetricsContainer {..}

registerSearchRequestCounterMetric :: IO SearchRequestCounterMetric
registerSearchRequestCounterMetric = P.register $ P.vector ("merchant_name", "version", "merchantOperatingCityId") $ P.counter $ P.Info "search_request_count" ""

registerBusScannetCounterMetric :: IO BusScannetCounterMetric
registerBusScannetCounterMetric = P.register $ P.vector ("merchant_name", "version", "merchantOperatingCityId", "vehicle_number") $ P.counter $ P.Info "scanned_bus_counter" ""

registerFleetRouteMapMissingCounterMetric :: IO FleetRouteMapMissingCounterMetric
registerFleetRouteMapMissingCounterMetric = P.register $ P.vector ("merchant_name", "version", "merchantOperatingCityId", "vehicle_number") $ P.counter $ P.Info "fleet_route_map_missing_counter" ""

registerVehicleNoEtaCounterMetric :: IO VehicleNoEtaCounterMetric
registerVehicleNoEtaCounterMetric = P.register $ P.vector ("merchant_name", "version", "merchantOperatingCityId", "source") $ P.counter $ P.Info "vehicle_no_eta_count" ""

registerBusScanSearchRequestCounterMetric :: IO BusScanSearchRequestCounterMetric
registerBusScanSearchRequestCounterMetric = P.register $ P.vector ("merchant_name", "version", "merchantOperatingCityId") $ P.counter $ P.Info "bus_scan_search_request_count" ""

registerRideCreatedCounterMetric :: IO RideCreatedCounterMetric
registerRideCreatedCounterMetric = P.register $ P.vector ("merchant_id", "version", "category", "merchantOperatingCityId") $ P.counter $ P.Info "ride_created_count" ""

registerSearchDurationMetric :: Seconds -> IO SearchDurationMetric
registerSearchDurationMetric searchDurationTimeout = do
  let bucketsCount = (getSeconds searchDurationTimeout + 1) * 2
  searchDurationHistogram <- P.register . P.vector ("merchant_name", "version") . P.histogram (P.Info "beckn_search_round_trip" "") $ P.linearBuckets 0 0.5 bucketsCount
  failureCounter <- P.register . P.vector ("merchant_name", "version") $ P.counter $ P.Info "beckn_search_round_trip_failure_counter" ""
  return (searchDurationHistogram, failureCounter)

registerDurationMetricFRFS :: Seconds -> Text -> Text -> Text -> Text -> Text -> IO DurationMetric
registerDurationMetricFRFS = registerDurationMetric

registerDurationMetric :: Seconds -> Text -> Text -> Text -> Text -> Text -> IO DurationMetric
registerDurationMetric durationTimeout merchantName version merchantOperatingCityId roundTrip roundTripFailureCounter = do
  let bucketsCount = (getSeconds durationTimeout + 1) * 2
  durationHistogram <- P.register . P.vector (merchantName, version, merchantOperatingCityId) . P.histogram (P.Info roundTrip "") $ P.linearBuckets 0 0.5 bucketsCount
  failureCounter <- P.register . P.vector (merchantName, version, merchantOperatingCityId) $ P.counter $ P.Info roundTripFailureCounter ""
  return (durationHistogram, failureCounter)

-- Fix #22: Payment journey metrics

registerPaymentAttemptCounterMetric :: IO PaymentAttemptCounterMetric
registerPaymentAttemptCounterMetric = P.register $ P.vector ("payment_method", "transit_mode", "result") $ P.counter $ P.Info "payment_attempt_total" ""

registerPaymentPageLoadDurationMetric :: Seconds -> IO PaymentPageLoadDurationMetric
registerPaymentPageLoadDurationMetric timeout = do
  let bucketsCount = (getSeconds timeout + 1) * 2
  hist <- P.register . P.vector ("platform", "result") . P.histogram (P.Info "payment_page_load_duration_seconds" "") $ P.linearBuckets 0 0.5 bucketsCount
  failCounter <- P.register . P.vector ("platform", "result") $ P.counter $ P.Info "payment_page_load_failure_counter" ""
  return (hist, failCounter)

registerHyperSDKInitCounterMetric :: IO HyperSDKInitCounterMetric
registerHyperSDKInitCounterMetric = P.register $ P.vector ("platform", "result") $ P.counter $ P.Info "hypersdk_init_total" ""

-- Fix #23: Booking funnel metrics

registerBookingStageCounterMetric :: IO BookingStageCounterMetric
registerBookingStageCounterMetric = P.register $ P.vector ("transit_mode", "stage", "result") $ P.counter $ P.Info "booking_stage_total" ""

registerBookingStageDurationMetric :: Seconds -> IO BookingStageDurationMetric
registerBookingStageDurationMetric timeout = do
  let bucketsCount = (getSeconds timeout + 1) * 2
  hist <- P.register . P.vector ("transit_mode", "stage") . P.histogram (P.Info "booking_stage_duration_seconds" "") $ P.linearBuckets 0 0.5 bucketsCount
  failCounter <- P.register . P.vector ("transit_mode", "stage") $ P.counter $ P.Info "booking_stage_duration_failure_counter" ""
  return (hist, failCounter)

registerBookingE2ECounterMetric :: IO BookingE2ECounterMetric
registerBookingE2ECounterMetric = P.register $ P.vector "transit_mode" $ P.counter $ P.Info "booking_e2e_success_total" ""

-- Fix #24: QR scan metrics

registerQRPayloadSizeMetric :: IO QRPayloadSizeMetric
registerQRPayloadSizeMetric = P.register . P.vector ("transit_mode", "journey_type") . P.histogram (P.Info "qr_payload_size_bytes" "") $ P.linearBuckets 0 50 20

registerQRGenerationCounterMetric :: IO QRGenerationCounterMetric
registerQRGenerationCounterMetric = P.register $ P.vector ("transit_mode", "qr_version") $ P.counter $ P.Info "qr_generation_total" ""

registerQRScanAttemptCounterMetric :: IO QRScanAttemptCounterMetric
registerQRScanAttemptCounterMetric = P.register $ P.vector ("station", "result") $ P.counter $ P.Info "qr_scan_attempt_total" ""
