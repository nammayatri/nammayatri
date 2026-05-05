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
    ticketInitializedCounter :: TicketCounterMetric,
    ticketConfirmedCounter :: TicketCounterMetric,
    vehicleHistoricCounter :: VehicleHistoricCounterMetric,
    vehicleScheduleBasedActiveTripCounter :: VehicleScheduleBasedActiveTripCounterMetric,
    vehicleWaybillStatusCounter :: VehicleWaybillStatusCounterMetric
  }

type SearchRequestCounterMetric = P.Vector P.Label3 P.Counter

type BusScannetCounterMetric = P.Vector P.Label4 P.Counter

type FleetRouteMapMissingCounterMetric = P.Vector P.Label4 P.Counter

type RideCreatedCounterMetric = P.Vector P.Label4 P.Counter

type SearchDurationMetric = (P.Vector P.Label2 P.Histogram, P.Vector P.Label2 P.Counter)

type DurationMetric = (P.Vector P.Label3 P.Histogram, P.Vector P.Label3 P.Counter)

type VehicleNoEtaCounterMetric = P.Vector P.Label4 P.Counter

type BusScanSearchRequestCounterMetric = P.Vector P.Label3 P.Counter

type TicketCounterMetric = P.Vector P.Label5 P.Counter

type VehicleHistoricCounterMetric = P.Vector P.Label3 P.Counter

type VehicleScheduleBasedActiveTripCounterMetric = P.Vector P.Label3 P.Counter

type VehicleWaybillStatusCounterMetric = P.Vector P.Label4 P.Counter

registerBAPMetricsContainer :: Seconds -> IO BAPMetricsContainer
registerBAPMetricsContainer searchDurationTimeout = do
  searchRequestCounter <- registerSearchRequestCounterMetric
  busScannerCounter <- registerBusScannetCounterMetric
  fleetRouteMapMissingCounter <- registerFleetRouteMapMissingCounterMetric
  vehicleNoEtaCounter <- registerVehicleNoEtaCounterMetric
  busScanSearchRequestCounter <- registerBusScanSearchRequestCounterMetric
  rideCreatedCounter <- registerRideCreatedCounterMetric
  ticketInitializedCounter <- registerTicketInitializedCounterMetric
  ticketConfirmedCounter <- registerTicketConfirmedCounterMetric
  vehicleHistoricCounter <- registerVehicleHistoricCounterMetric
  vehicleScheduleBasedActiveTripCounter <- registerVehicleScheduleBasedActiveTripCounterMetric
  vehicleWaybillStatusCounter <- registerVehicleWaybillStatusCounterMetric
  searchDuration <- registerSearchDurationMetric searchDurationTimeout
  searchDurationFRFS <- registerDurationMetricFRFS searchDurationTimeout "merchant_name" "version" "merchantOperatingCityId" "beckn_search_frfs_round_trip" "beckn_search_frfs_round_trip_failure_counter"
  selectDurationFRFS <- registerDurationMetricFRFS searchDurationTimeout "merchant_name" "version" "merchantOperatingCityId" "beckn_select_frfs_round_trip" "beckn_select_frfs_round_trip_failure_counter"
  initDurationFRFS <- registerDurationMetricFRFS searchDurationTimeout "merchant_name" "version" "merchantOperatingCityId" "beckn_init_frfs_round_trip" "beckn_init_frfs_round_trip_failure_counter"
  confirmDurationFRFS <- registerDurationMetricFRFS searchDurationTimeout "merchant_name" "version" "merchantOperatingCityId" "beckn_confirm_frfs_round_trip" "beckn_confirm_frfs_round_trip_failure_counter"
  cancelDurationFRFS <- registerDurationMetricFRFS searchDurationTimeout "merchant_name" "version" "merchantOperatingCityId" "beckn_cancel_frfs_round_trip" "beckn_cancel_frfs_round_trip_failure_counter"
  initDuration <- registerDurationMetric searchDurationTimeout "merchant_name" "version" "merchantOperatingCityId" "beckn_init_round_trip" "beckn_init_round_trip_failure_counter"
  confirmDuration <- registerDurationMetric searchDurationTimeout "merchant_name" "version" "merchantOperatingCityId" "beckn_confirm_round_trip" "beckn_confirm_round_trip_failure_counter"
  createOrderDurationFRFS <- registerDurationMetricFRFS searchDurationTimeout "merchant_name" "version" "merchantOperatingCityId" "beckn_create_order_frfs_round_trip" "beckn_create_order_frfs_round_trip_failure_counter"
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

registerTicketInitializedCounterMetric :: IO TicketCounterMetric
registerTicketInitializedCounterMetric = P.register $ P.vector ("merchant_name", "version", "merchantOperatingCityId", "vehicleType", "serviceTierType") $ P.counter $ P.Info "ticket_initialized_counter" ""

registerTicketConfirmedCounterMetric :: IO TicketCounterMetric
registerTicketConfirmedCounterMetric = P.register $ P.vector ("merchant_name", "version", "merchantOperatingCityId", "vehicleType", "serviceTierType") $ P.counter $ P.Info "ticket_confirmed_counter" ""

registerVehicleHistoricCounterMetric :: IO VehicleHistoricCounterMetric
registerVehicleHistoricCounterMetric = P.register $ P.vector ("merchant_name", "version", "merchantOperatingCityId") $ P.counter $ P.Info "vehicle_historic_count" ""

registerVehicleScheduleBasedActiveTripCounterMetric :: IO VehicleScheduleBasedActiveTripCounterMetric
registerVehicleScheduleBasedActiveTripCounterMetric = P.register $ P.vector ("merchant_name", "version", "merchantOperatingCityId") $ P.counter $ P.Info "vehicle_schedule_based_active_trip_count" ""

registerVehicleWaybillStatusCounterMetric :: IO VehicleWaybillStatusCounterMetric
registerVehicleWaybillStatusCounterMetric = P.register $ P.vector ("merchant_name", "version", "merchantOperatingCityId", "waybill_status") $ P.counter $ P.Info "vehicle_waybill_status_count" ""

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
