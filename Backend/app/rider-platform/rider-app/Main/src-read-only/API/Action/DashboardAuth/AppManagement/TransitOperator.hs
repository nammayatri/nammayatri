{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.TransitOperator
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.TransitOperator
import qualified "beckn-spec" BecknV2.OnDemand.Enums
import qualified Data.Aeson
import qualified Domain.Action.Dashboard.AppManagement.TransitOperator
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified "this" SharedLogic.External.Nandi.TransitV2Types
import qualified "this" SharedLogic.External.Nandi.Types
import qualified Tools.ActorInfo
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("transitOperator" :> (TransitOperatorGetRow :<|> TransitOperatorGetAllRows :<|> TransitOperatorDeleteRow :<|> TransitOperatorUpsertRow :<|> TransitOperatorUpsertRows :<|> TransitOperatorQueryRows :<|> TransitOperatorGetServiceTypes :<|> TransitOperatorGetRoutes :<|> TransitOperatorGetDepots :<|> TransitOperatorGetShiftTypes :<|> TransitOperatorGetScheduleNumbers :<|> TransitOperatorGetDayTypes :<|> TransitOperatorGetTripTypes :<|> TransitOperatorGetBreakTypes :<|> TransitOperatorGetTripDetails :<|> TransitOperatorGetFleets :<|> TransitOperatorGetConductor :<|> TransitOperatorGetDriver :<|> TransitOperatorGetDeviceIds :<|> TransitOperatorGetTabletIds :<|> TransitOperatorGetOperators :<|> TransitOperatorUpdateWaybillStatus :<|> TransitOperatorUpdateWaybillFleet :<|> TransitOperatorUpdateWaybillDetails :<|> TransitOperatorUpdateWaybillTablet :<|> TransitOperatorGetWaybills :<|> TransitOperatorGetDeviceVehicleMappingList :<|> TransitOperatorUpsertDeviceVehicleMapping :<|> TransitOperatorUnblockBus :<|> TransitOperatorSearchStops :<|> TransitOperatorNearbyStops :<|> TransitOperatorBulkReplaceStops :<|> TransitOperatorRouteStops :<|> TransitOperatorInsertRouteStop :<|> TransitOperatorReprocessRoutes :<|> TransitOperatorExportRouteStopMapping :<|> TransitOperatorQueryVehicle :<|> TransitOperatorUpsertVehicles :<|> TransitOperatorDeleteVehicle :<|> TransitOperatorGetScheduleTripRepeat :<|> TransitOperatorSetScheduleTripRepeat :<|> TransitOperatorV2ListTripGroups :<|> TransitOperatorV2UpsertTripGroup :<|> TransitOperatorV2GetTripGroup :<|> TransitOperatorV2DeleteTripGroup :<|> TransitOperatorV2ListTrips :<|> TransitOperatorV2UpsertTrips :<|> TransitOperatorV2DeleteTrip :<|> TransitOperatorV2ListDutyRepeats :<|> TransitOperatorV2UpsertDutyRepeat :<|> TransitOperatorV2DeleteDutyRepeat :<|> TransitOperatorV2PreviewDutyRepeats :<|> TransitOperatorV2GenerateDutyRepeats :<|> TransitOperatorV2ListDutyGroups :<|> TransitOperatorV2CreateDutyGroup :<|> TransitOperatorV2GetDutyGroup :<|> TransitOperatorV2ListDuties :<|> TransitOperatorV2GetDutyGroupLogs :<|> TransitOperatorV2UpdateDutyGroupVehicle :<|> TransitOperatorV2UpdateDutyGroupCrew :<|> TransitOperatorV2SetDutyGroupActive :<|> TransitOperatorV2DeleteDutyGroup :<|> TransitOperatorV2UpdateDutyCrew :<|> TransitOperatorV2DeleteDuty :<|> TransitOperatorV2ListGenerationFailures :<|> TransitOperatorV2ResolveGenerationFailure))

type TransitOperatorGetRow =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_ROW"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetRow
  )

type TransitOperatorGetAllRows =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_ALL_ROWS"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetAllRows
  )

type TransitOperatorDeleteRow =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_DELETE_ROW"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorDeleteRow
  )

type TransitOperatorUpsertRow =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UPSERT_ROW"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorUpsertRow
  )

type TransitOperatorUpsertRows =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UPSERT_ROWS"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorUpsertRows
  )

type TransitOperatorQueryRows =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_QUERY_ROWS"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorQueryRows
  )

type TransitOperatorGetServiceTypes =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_SERVICE_TYPES"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetServiceTypes
  )

type TransitOperatorGetRoutes =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_ROUTES"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetRoutes
  )

type TransitOperatorGetDepots =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_DEPOTS"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetDepots
  )

type TransitOperatorGetShiftTypes =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_SHIFT_TYPES"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetShiftTypes
  )

type TransitOperatorGetScheduleNumbers =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_SCHEDULE_NUMBERS"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetScheduleNumbers
  )

type TransitOperatorGetDayTypes =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_DAY_TYPES"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetDayTypes
  )

type TransitOperatorGetTripTypes =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_TRIP_TYPES"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetTripTypes
  )

type TransitOperatorGetBreakTypes =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_BREAK_TYPES"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetBreakTypes
  )

type TransitOperatorGetTripDetails =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_TRIP_DETAILS"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetTripDetails
  )

type TransitOperatorGetFleets =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_FLEETS"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetFleets
  )

type TransitOperatorGetConductor =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_CONDUCTOR"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetConductor
  )

type TransitOperatorGetDriver =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_DRIVER"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetDriver
  )

type TransitOperatorGetDeviceIds =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_DEVICE_IDS"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetDeviceIds
  )

type TransitOperatorGetTabletIds =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_TABLET_IDS"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetTabletIds
  )

type TransitOperatorGetOperators =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_OPERATORS"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetOperators
  )

type TransitOperatorUpdateWaybillStatus =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UPDATE_WAYBILL_STATUS"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorUpdateWaybillStatus
  )

type TransitOperatorUpdateWaybillFleet =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UPDATE_WAYBILL_FLEET"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorUpdateWaybillFleet
  )

type TransitOperatorUpdateWaybillDetails =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UPDATE_WAYBILL_DETAILS"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorUpdateWaybillDetails
  )

type TransitOperatorUpdateWaybillTablet =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UPDATE_WAYBILL_TABLET"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorUpdateWaybillTablet
  )

type TransitOperatorGetWaybills =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_WAYBILLS"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetWaybills
  )

type TransitOperatorGetDeviceVehicleMappingList =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_DEVICE_VEHICLE_MAPPING_LIST"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetDeviceVehicleMappingList
  )

type TransitOperatorUpsertDeviceVehicleMapping =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UPSERT_DEVICE_VEHICLE_MAPPING"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorUpsertDeviceVehicleMapping
  )

type TransitOperatorUnblockBus =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UNBLOCK_BUS"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorUnblockBus
  )

type TransitOperatorSearchStops =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_SEARCH_STOPS"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorSearchStops
  )

type TransitOperatorNearbyStops =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_NEARBY_STOPS"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorNearbyStops
  )

type TransitOperatorBulkReplaceStops =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_BULK_REPLACE_STOPS"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorBulkReplaceStops
  )

type TransitOperatorRouteStops =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_ROUTE_STOPS"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorRouteStops
  )

type TransitOperatorInsertRouteStop =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_INSERT_ROUTE_STOP"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorInsertRouteStop
  )

type TransitOperatorReprocessRoutes =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_REPROCESS_ROUTES"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorReprocessRoutes
  )

type TransitOperatorExportRouteStopMapping =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_EXPORT_ROUTE_STOP_MAPPING"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorExportRouteStopMapping
  )

type TransitOperatorQueryVehicle =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_QUERY_VEHICLE"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorQueryVehicle
  )

type TransitOperatorUpsertVehicles =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UPSERT_VEHICLES"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorUpsertVehicles
  )

type TransitOperatorDeleteVehicle =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_DELETE_VEHICLE"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorDeleteVehicle
  )

type TransitOperatorGetScheduleTripRepeat =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_SCHEDULE_TRIP_REPEAT"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetScheduleTripRepeat
  )

type TransitOperatorSetScheduleTripRepeat =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_SET_SCHEDULE_TRIP_REPEAT"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorSetScheduleTripRepeat
  )

type TransitOperatorV2ListTripGroups =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_LIST_TRIP_GROUPS"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2ListTripGroups
  )

type TransitOperatorV2UpsertTripGroup =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_UPSERT_TRIP_GROUP"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2UpsertTripGroup
  )

type TransitOperatorV2GetTripGroup =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_GET_TRIP_GROUP"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2GetTripGroup
  )

type TransitOperatorV2DeleteTripGroup =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_DELETE_TRIP_GROUP"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2DeleteTripGroup
  )

type TransitOperatorV2ListTrips =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_LIST_TRIPS"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2ListTrips
  )

type TransitOperatorV2UpsertTrips =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_UPSERT_TRIPS"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2UpsertTrips
  )

type TransitOperatorV2DeleteTrip =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_DELETE_TRIP"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2DeleteTrip
  )

type TransitOperatorV2ListDutyRepeats =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_LIST_DUTY_REPEATS"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2ListDutyRepeats
  )

type TransitOperatorV2UpsertDutyRepeat =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_UPSERT_DUTY_REPEAT"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2UpsertDutyRepeat
  )

type TransitOperatorV2DeleteDutyRepeat =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_DELETE_DUTY_REPEAT"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2DeleteDutyRepeat
  )

type TransitOperatorV2PreviewDutyRepeats =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_PREVIEW_DUTY_REPEATS"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2PreviewDutyRepeats
  )

type TransitOperatorV2GenerateDutyRepeats =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_GENERATE_DUTY_REPEATS"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2GenerateDutyRepeats
  )

type TransitOperatorV2ListDutyGroups =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_LIST_DUTY_GROUPS"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2ListDutyGroups
  )

type TransitOperatorV2CreateDutyGroup =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_CREATE_DUTY_GROUP"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2CreateDutyGroup
  )

type TransitOperatorV2GetDutyGroup =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_GET_DUTY_GROUP"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2GetDutyGroup
  )

type TransitOperatorV2ListDuties =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_LIST_DUTIES"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2ListDuties
  )

type TransitOperatorV2GetDutyGroupLogs =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_GET_DUTY_GROUP_LOGS"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2GetDutyGroupLogs
  )

type TransitOperatorV2UpdateDutyGroupVehicle =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_UPDATE_DUTY_GROUP_VEHICLE"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2UpdateDutyGroupVehicle
  )

type TransitOperatorV2UpdateDutyGroupCrew =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_UPDATE_DUTY_GROUP_CREW"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2UpdateDutyGroupCrew
  )

type TransitOperatorV2SetDutyGroupActive =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_SET_DUTY_GROUP_ACTIVE"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2SetDutyGroupActive
  )

type TransitOperatorV2DeleteDutyGroup =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_DELETE_DUTY_GROUP"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2DeleteDutyGroup
  )

type TransitOperatorV2UpdateDutyCrew =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_UPDATE_DUTY_CREW"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2UpdateDutyCrew
  )

type TransitOperatorV2DeleteDuty =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_DELETE_DUTY"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2DeleteDuty
  )

type TransitOperatorV2ListGenerationFailures =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_LIST_GENERATION_FAILURES"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2ListGenerationFailures
  )

type TransitOperatorV2ResolveGenerationFailure =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_RESOLVE_GENERATION_FAILURE"
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2ResolveGenerationFailure
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = transitOperatorGetRow merchantId city :<|> transitOperatorGetAllRows merchantId city :<|> transitOperatorDeleteRow merchantId city :<|> transitOperatorUpsertRow merchantId city :<|> transitOperatorUpsertRows merchantId city :<|> transitOperatorQueryRows merchantId city :<|> transitOperatorGetServiceTypes merchantId city :<|> transitOperatorGetRoutes merchantId city :<|> transitOperatorGetDepots merchantId city :<|> transitOperatorGetShiftTypes merchantId city :<|> transitOperatorGetScheduleNumbers merchantId city :<|> transitOperatorGetDayTypes merchantId city :<|> transitOperatorGetTripTypes merchantId city :<|> transitOperatorGetBreakTypes merchantId city :<|> transitOperatorGetTripDetails merchantId city :<|> transitOperatorGetFleets merchantId city :<|> transitOperatorGetConductor merchantId city :<|> transitOperatorGetDriver merchantId city :<|> transitOperatorGetDeviceIds merchantId city :<|> transitOperatorGetTabletIds merchantId city :<|> transitOperatorGetOperators merchantId city :<|> transitOperatorUpdateWaybillStatus merchantId city :<|> transitOperatorUpdateWaybillFleet merchantId city :<|> transitOperatorUpdateWaybillDetails merchantId city :<|> transitOperatorUpdateWaybillTablet merchantId city :<|> transitOperatorGetWaybills merchantId city :<|> transitOperatorGetDeviceVehicleMappingList merchantId city :<|> transitOperatorUpsertDeviceVehicleMapping merchantId city :<|> transitOperatorUnblockBus merchantId city :<|> transitOperatorSearchStops merchantId city :<|> transitOperatorNearbyStops merchantId city :<|> transitOperatorBulkReplaceStops merchantId city :<|> transitOperatorRouteStops merchantId city :<|> transitOperatorInsertRouteStop merchantId city :<|> transitOperatorReprocessRoutes merchantId city :<|> transitOperatorExportRouteStopMapping merchantId city :<|> transitOperatorQueryVehicle merchantId city :<|> transitOperatorUpsertVehicles merchantId city :<|> transitOperatorDeleteVehicle merchantId city :<|> transitOperatorGetScheduleTripRepeat merchantId city :<|> transitOperatorSetScheduleTripRepeat merchantId city :<|> transitOperatorV2ListTripGroups merchantId city :<|> transitOperatorV2UpsertTripGroup merchantId city :<|> transitOperatorV2GetTripGroup merchantId city :<|> transitOperatorV2DeleteTripGroup merchantId city :<|> transitOperatorV2ListTrips merchantId city :<|> transitOperatorV2UpsertTrips merchantId city :<|> transitOperatorV2DeleteTrip merchantId city :<|> transitOperatorV2ListDutyRepeats merchantId city :<|> transitOperatorV2UpsertDutyRepeat merchantId city :<|> transitOperatorV2DeleteDutyRepeat merchantId city :<|> transitOperatorV2PreviewDutyRepeats merchantId city :<|> transitOperatorV2GenerateDutyRepeats merchantId city :<|> transitOperatorV2ListDutyGroups merchantId city :<|> transitOperatorV2CreateDutyGroup merchantId city :<|> transitOperatorV2GetDutyGroup merchantId city :<|> transitOperatorV2ListDuties merchantId city :<|> transitOperatorV2GetDutyGroupLogs merchantId city :<|> transitOperatorV2UpdateDutyGroupVehicle merchantId city :<|> transitOperatorV2UpdateDutyGroupCrew merchantId city :<|> transitOperatorV2SetDutyGroupActive merchantId city :<|> transitOperatorV2DeleteDutyGroup merchantId city :<|> transitOperatorV2UpdateDutyCrew merchantId city :<|> transitOperatorV2DeleteDuty merchantId city :<|> transitOperatorV2ListGenerationFailures merchantId city :<|> transitOperatorV2ResolveGenerationFailure merchantId city

transitOperatorGetRow :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.Types.NandiRow)
transitOperatorGetRow a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetRow a6 a5 a3 a2 a1

transitOperatorGetAllRows :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.NandiRow])
transitOperatorGetAllRows a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a5 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetAllRows a7 a6 a4 a3 a2 a1

transitOperatorDeleteRow :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Data.Aeson.Value -> Environment.FlowHandler SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorDeleteRow a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_DELETE_ROW" a4 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorDeleteRow a6 a5 a3 a2 a1
    )

transitOperatorUpsertRow :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Data.Aeson.Value -> Environment.FlowHandler SharedLogic.External.Nandi.Types.NandiRow)
transitOperatorUpsertRow a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UPSERT_ROW" a5 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a5 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorUpsertRow a7 a6 a4 a3 a2 a1
    )

transitOperatorUpsertRows :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> [Data.Aeson.Value] -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.NandiRow])
transitOperatorUpsertRows a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UPSERT_ROWS" a5 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a5 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorUpsertRows a7 a6 a4 a3 a2 a1
    )

transitOperatorQueryRows :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.QueryBody -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.NandiRow])
transitOperatorQueryRows a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_QUERY_ROWS" a4 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorQueryRows a6 a5 a3 a2 a1
    )

transitOperatorGetServiceTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.ServiceType])
transitOperatorGetServiceTypes a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetServiceTypes a4 a3 a1

transitOperatorGetRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.NandiRoute])
transitOperatorGetRoutes a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetRoutes a4 a3 a1

transitOperatorGetDepots :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.Depot])
transitOperatorGetDepots a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetDepots a4 a3 a1

transitOperatorGetShiftTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.ShiftType])
transitOperatorGetShiftTypes a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetShiftTypes a4 a3 a1

transitOperatorGetScheduleNumbers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.ScheduleNumber])
transitOperatorGetScheduleNumbers a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetScheduleNumbers a4 a3 a1

transitOperatorGetDayTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.DayType])
transitOperatorGetDayTypes a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetDayTypes a4 a3 a1

transitOperatorGetTripTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.TripType])
transitOperatorGetTripTypes a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetTripTypes a4 a3 a1

transitOperatorGetBreakTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.BreakType])
transitOperatorGetBreakTypes a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetBreakTypes a4 a3 a1

transitOperatorGetTripDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.NandiTripDetail])
transitOperatorGetTripDetails a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetTripDetails a5 a4 a2 a1

transitOperatorGetFleets :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.Fleet])
transitOperatorGetFleets a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetFleets a6 a5 a3 a2 a1

transitOperatorGetConductor :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.Types.Employee)
transitOperatorGetConductor a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetConductor a5 a4 a2 a1

transitOperatorGetDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.Types.Employee)
transitOperatorGetDriver a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetDriver a5 a4 a2 a1

transitOperatorGetDeviceIds :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [Kernel.Prelude.Text])
transitOperatorGetDeviceIds a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetDeviceIds a4 a3 a1

transitOperatorGetTabletIds :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [Kernel.Prelude.Text])
transitOperatorGetTabletIds a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetTabletIds a4 a3 a1

transitOperatorGetOperators :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> SharedLogic.External.Nandi.Types.OperatorRole -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.Employee])
transitOperatorGetOperators a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetOperators a5 a4 a2 a1

transitOperatorUpdateWaybillStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillStatusReq -> Environment.FlowHandler SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorUpdateWaybillStatus a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UPDATE_WAYBILL_STATUS" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorUpdateWaybillStatus a5 a4 a2 a1
    )

transitOperatorUpdateWaybillFleet :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillFleetReq -> Environment.FlowHandler SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorUpdateWaybillFleet a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UPDATE_WAYBILL_FLEET" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorUpdateWaybillFleet a5 a4 a2 a1
    )

transitOperatorUpdateWaybillDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillDetailsReq -> Environment.FlowHandler SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorUpdateWaybillDetails a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UPDATE_WAYBILL_DETAILS" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorUpdateWaybillDetails a5 a4 a2 a1
    )

transitOperatorUpdateWaybillTablet :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillTabletReq -> Environment.FlowHandler SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorUpdateWaybillTablet a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UPDATE_WAYBILL_TABLET" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorUpdateWaybillTablet a5 a4 a2 a1
    )

transitOperatorGetWaybills :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.NandiWaybillRow])
transitOperatorGetWaybills a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetWaybills a6 a5 a3 a2 a1

transitOperatorGetDeviceVehicleMappingList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler API.Types.Dashboard.AppManagement.TransitOperator.DeviceVehicleMappingListRes)
transitOperatorGetDeviceVehicleMappingList a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a1 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetDeviceVehicleMappingList a3 a2

transitOperatorUpsertDeviceVehicleMapping :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.Dashboard.AppManagement.TransitOperator.UpsertDeviceVehicleMappingReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.TransitOperator.UpsertDeviceVehicleMappingResp)
transitOperatorUpsertDeviceVehicleMapping a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UPSERT_DEVICE_VEHICLE_MAPPING" a2 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorUpsertDeviceVehicleMapping a4 a3 a1
    )

transitOperatorUnblockBus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
transitOperatorUnblockBus a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UNBLOCK_BUS" a2 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorUnblockBus a4 a3 a1
    )

transitOperatorSearchStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.EnrichedStop])
transitOperatorSearchStops a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a5 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorSearchStops a7 a6 a4 a3 a2 a1

transitOperatorNearbyStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Double) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Double -> Kernel.Prelude.Double -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.EnrichedStop])
transitOperatorNearbyStops a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a7 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorNearbyStops a9 a8 a6 a5 a4 a3 a2 a1

transitOperatorBulkReplaceStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.BulkReplaceReq -> Environment.FlowHandler SharedLogic.External.Nandi.Types.BulkReplaceResult)
transitOperatorBulkReplaceStops a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_BULK_REPLACE_STOPS" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorBulkReplaceStops a5 a4 a2 a1
    )

transitOperatorRouteStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.Types.RouteStopsResponse)
transitOperatorRouteStops a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorRouteStops a5 a4 a2 a1

transitOperatorInsertRouteStop :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.InsertRouteStopReq -> Environment.FlowHandler SharedLogic.External.Nandi.Types.InsertRouteStopResp)
transitOperatorInsertRouteStop a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_INSERT_ROUTE_STOP" a4 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorInsertRouteStop a6 a5 a3 a2 a1
    )

transitOperatorReprocessRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.ReprocessReq -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.ReprocessResult])
transitOperatorReprocessRoutes a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_REPROCESS_ROUTES" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorReprocessRoutes a5 a4 a2 a1
    )

transitOperatorExportRouteStopMapping :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.RouteStopMappingExport])
transitOperatorExportRouteStopMapping a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorExportRouteStopMapping a4 a3 a1

transitOperatorQueryVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.Fleet])
transitOperatorQueryVehicle a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a5 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorQueryVehicle a7 a6 a4 a3 a2 a1

transitOperatorUpsertVehicles :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> [SharedLogic.External.Nandi.Types.VehicleUpsertRequest] -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.Fleet])
transitOperatorUpsertVehicles a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UPSERT_VEHICLES" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorUpsertVehicles a5 a4 a2 a1
    )

transitOperatorDeleteVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Kernel.Prelude.Text -> Environment.FlowHandler SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorDeleteVehicle a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_DELETE_VEHICLE" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorDeleteVehicle a5 a4 a2 a1
    )

transitOperatorGetScheduleTripRepeat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.Types.ScheduleTripRepeatConfig)
transitOperatorGetScheduleTripRepeat a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetScheduleTripRepeat a5 a4 a2 a1

transitOperatorSetScheduleTripRepeat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.SetScheduleTripRepeatReq -> Environment.FlowHandler SharedLogic.External.Nandi.Types.ScheduleTripRepeatConfig)
transitOperatorSetScheduleTripRepeat a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_SET_SCHEDULE_TRIP_REPEAT" a4 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorSetScheduleTripRepeat a6 a5 a3 a2 a1
    )

transitOperatorV2ListTripGroups :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2TripGroupPage)
transitOperatorV2ListTripGroups a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a14 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2ListTripGroups a16 a15 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a14)) a1

transitOperatorV2UpsertTripGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpsertTripGroupReq -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2TripGroup)
transitOperatorV2UpsertTripGroup a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_UPSERT_TRIP_GROUP" a4 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2UpsertTripGroup a6 a5 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a2 a1
    )

transitOperatorV2GetTripGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2TripGroup)
transitOperatorV2GetTripGroup a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2GetTripGroup a6 a5 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a1

transitOperatorV2DeleteTripGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp)
transitOperatorV2DeleteTripGroup a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_DELETE_TRIP_GROUP" a4 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2DeleteTripGroup a6 a5 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a1
    )

transitOperatorV2ListTrips :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.TransitV2Types.V2Trip])
transitOperatorV2ListTrips a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2ListTrips a6 a5 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a1

transitOperatorV2UpsertTrips :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpsertTripsReq -> Environment.FlowHandler [SharedLogic.External.Nandi.TransitV2Types.V2Trip])
transitOperatorV2UpsertTrips a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_UPSERT_TRIPS" a5 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a5 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2UpsertTrips a7 a6 a4 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a5)) a2 a1
    )

transitOperatorV2DeleteTrip :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp)
transitOperatorV2DeleteTrip a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_DELETE_TRIP" a4 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2DeleteTrip a6 a5 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a1
    )

transitOperatorV2ListDutyRepeats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2DutyRepeatPage)
transitOperatorV2ListDutyRepeats a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a12 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2ListDutyRepeats a14 a13 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a12)) a1

transitOperatorV2UpsertDutyRepeat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpsertDutyRepeatReq -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2UpsertDutyRepeatResp)
transitOperatorV2UpsertDutyRepeat a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_UPSERT_DUTY_REPEAT" a4 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2UpsertDutyRepeat a6 a5 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a2 a1
    )

transitOperatorV2DeleteDutyRepeat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp)
transitOperatorV2DeleteDutyRepeat a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_DELETE_DUTY_REPEAT" a4 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2DeleteDutyRepeat a6 a5 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a1
    )

transitOperatorV2PreviewDutyRepeats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2GenerateReq -> Environment.FlowHandler [SharedLogic.External.Nandi.TransitV2Types.V2GenerateEntry])
transitOperatorV2PreviewDutyRepeats a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_PREVIEW_DUTY_REPEATS" a4 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2PreviewDutyRepeats a6 a5 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a2 a1
    )

transitOperatorV2GenerateDutyRepeats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2GenerateReq -> Environment.FlowHandler [SharedLogic.External.Nandi.TransitV2Types.V2GenerateEntry])
transitOperatorV2GenerateDutyRepeats a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_GENERATE_DUTY_REPEATS" a4 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2GenerateDutyRepeats a6 a5 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a2 a1
    )

transitOperatorV2ListDutyGroups :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupPage)
transitOperatorV2ListDutyGroups a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a14 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2ListDutyGroups a16 a15 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a14)) a1

transitOperatorV2CreateDutyGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2CreateDutyGroupReq -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupDetail)
transitOperatorV2CreateDutyGroup a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_CREATE_DUTY_GROUP" a4 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2CreateDutyGroup a6 a5 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a2 a1
    )

transitOperatorV2GetDutyGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupDetail)
transitOperatorV2GetDutyGroup a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2GetDutyGroup a6 a5 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a1

transitOperatorV2ListDuties :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.TransitV2Types.V2Duty])
transitOperatorV2ListDuties a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2ListDuties a6 a5 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a1

transitOperatorV2GetDutyGroupLogs :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.TransitV2Types.V2DutyEventLog])
transitOperatorV2GetDutyGroupLogs a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2GetDutyGroupLogs a6 a5 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a1

transitOperatorV2UpdateDutyGroupVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpdateVehicleReq -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2DutyGroup)
transitOperatorV2UpdateDutyGroupVehicle a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_UPDATE_DUTY_GROUP_VEHICLE" a5 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a5 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2UpdateDutyGroupVehicle a7 a6 a4 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a5)) a2 a1
    )

transitOperatorV2UpdateDutyGroupCrew :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpdateCrewReq -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupDetail)
transitOperatorV2UpdateDutyGroupCrew a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_UPDATE_DUTY_GROUP_CREW" a5 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a5 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2UpdateDutyGroupCrew a7 a6 a4 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a5)) a2 a1
    )

transitOperatorV2SetDutyGroupActive :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2SetActiveReq -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2DutyGroup)
transitOperatorV2SetDutyGroupActive a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_SET_DUTY_GROUP_ACTIVE" a5 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a5 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2SetDutyGroupActive a7 a6 a4 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a5)) a2 a1
    )

transitOperatorV2DeleteDutyGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp)
transitOperatorV2DeleteDutyGroup a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_DELETE_DUTY_GROUP" a4 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2DeleteDutyGroup a6 a5 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a1
    )

transitOperatorV2UpdateDutyCrew :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpdateCrewReq -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2Duty)
transitOperatorV2UpdateDutyCrew a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_UPDATE_DUTY_CREW" a5 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a5 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2UpdateDutyCrew a7 a6 a4 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a5)) a2 a1
    )

transitOperatorV2DeleteDuty :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp)
transitOperatorV2DeleteDuty a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_DELETE_DUTY" a4 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2DeleteDuty a6 a5 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a1
    )

transitOperatorV2ListGenerationFailures :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2DutyEventLogPage)
transitOperatorV2ListGenerationFailures a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a6 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2ListGenerationFailures a8 a7 a5 a4 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a6)) a1

transitOperatorV2ResolveGenerationFailure :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2DutyEventLog)
transitOperatorV2ResolveGenerationFailure a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_V2_RESOLVE_GENERATION_FAILURE" a4 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2ResolveGenerationFailure a6 a5 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a1
    )
