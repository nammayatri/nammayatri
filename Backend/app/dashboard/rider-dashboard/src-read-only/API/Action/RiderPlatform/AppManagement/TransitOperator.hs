{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.AppManagement.TransitOperator
  ( API,
    handler,
  )
where

import qualified "rider-app" API.Types.Dashboard.AppManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement.TransitOperator
import qualified "beckn-spec" BecknV2.OnDemand.Enums
import qualified Data.Aeson
import qualified Domain.Action.RiderPlatform.AppManagement.TransitOperator
import "rider-app" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified "rider-app" SharedLogic.External.Nandi.TransitV2Types
import qualified "rider-app" SharedLogic.External.Nandi.Types
import Storage.Beam.CommonInstances ()

type API = ("transitOperator" :> (TransitOperatorGetRow :<|> TransitOperatorGetAllRows :<|> TransitOperatorDeleteRow :<|> TransitOperatorUpsertRow :<|> TransitOperatorUpsertRows :<|> TransitOperatorQueryRows :<|> TransitOperatorGetServiceTypes :<|> TransitOperatorGetRoutes :<|> TransitOperatorGetDepots :<|> TransitOperatorGetShiftTypes :<|> TransitOperatorGetScheduleNumbers :<|> TransitOperatorGetDayTypes :<|> TransitOperatorGetTripTypes :<|> TransitOperatorGetBreakTypes :<|> TransitOperatorGetTripDetails :<|> TransitOperatorGetFleets :<|> TransitOperatorGetConductor :<|> TransitOperatorGetDriver :<|> TransitOperatorGetDeviceIds :<|> TransitOperatorGetTabletIds :<|> TransitOperatorGetOperators :<|> TransitOperatorUpdateWaybillStatus :<|> TransitOperatorUpdateWaybillFleet :<|> TransitOperatorUpdateWaybillDetails :<|> TransitOperatorUpdateWaybillTablet :<|> TransitOperatorGetWaybills :<|> TransitOperatorGetDeviceVehicleMappingList :<|> TransitOperatorUpsertDeviceVehicleMapping :<|> TransitOperatorUnblockBus :<|> TransitOperatorSearchStops :<|> TransitOperatorNearbyStops :<|> TransitOperatorBulkReplaceStops :<|> TransitOperatorRouteStops :<|> TransitOperatorInsertRouteStop :<|> TransitOperatorReprocessRoutes :<|> TransitOperatorExportRouteStopMapping :<|> TransitOperatorQueryVehicle :<|> TransitOperatorUpsertVehicles :<|> TransitOperatorDeleteVehicle :<|> TransitOperatorGetScheduleTripRepeat :<|> TransitOperatorSetScheduleTripRepeat :<|> TransitOperatorV2ListTripGroups :<|> TransitOperatorV2UpsertTripGroup :<|> TransitOperatorV2GetTripGroup :<|> TransitOperatorV2DeleteTripGroup :<|> TransitOperatorV2ListTrips :<|> TransitOperatorV2UpsertTrips :<|> TransitOperatorV2DeleteTrip :<|> TransitOperatorV2ListDutyRepeats :<|> TransitOperatorV2UpsertDutyRepeat :<|> TransitOperatorV2DeleteDutyRepeat :<|> TransitOperatorV2PreviewDutyRepeats :<|> TransitOperatorV2GenerateDutyRepeats :<|> TransitOperatorV2ListDutyGroups :<|> TransitOperatorV2CreateDutyGroup :<|> TransitOperatorV2GetDutyGroup :<|> TransitOperatorV2ListDuties :<|> TransitOperatorV2GetDutyGroupLogs :<|> TransitOperatorV2UpdateDutyGroupVehicle :<|> TransitOperatorV2UpdateDutyGroupCrew :<|> TransitOperatorV2SetDutyGroupActive :<|> TransitOperatorV2DeleteDutyGroup :<|> TransitOperatorV2UpdateDutyCrew :<|> TransitOperatorV2DeleteDuty :<|> TransitOperatorV2ListGenerationFailures :<|> TransitOperatorV2ResolveGenerationFailure))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = transitOperatorGetRow merchantId city :<|> transitOperatorGetAllRows merchantId city :<|> transitOperatorDeleteRow merchantId city :<|> transitOperatorUpsertRow merchantId city :<|> transitOperatorUpsertRows merchantId city :<|> transitOperatorQueryRows merchantId city :<|> transitOperatorGetServiceTypes merchantId city :<|> transitOperatorGetRoutes merchantId city :<|> transitOperatorGetDepots merchantId city :<|> transitOperatorGetShiftTypes merchantId city :<|> transitOperatorGetScheduleNumbers merchantId city :<|> transitOperatorGetDayTypes merchantId city :<|> transitOperatorGetTripTypes merchantId city :<|> transitOperatorGetBreakTypes merchantId city :<|> transitOperatorGetTripDetails merchantId city :<|> transitOperatorGetFleets merchantId city :<|> transitOperatorGetConductor merchantId city :<|> transitOperatorGetDriver merchantId city :<|> transitOperatorGetDeviceIds merchantId city :<|> transitOperatorGetTabletIds merchantId city :<|> transitOperatorGetOperators merchantId city :<|> transitOperatorUpdateWaybillStatus merchantId city :<|> transitOperatorUpdateWaybillFleet merchantId city :<|> transitOperatorUpdateWaybillDetails merchantId city :<|> transitOperatorUpdateWaybillTablet merchantId city :<|> transitOperatorGetWaybills merchantId city :<|> transitOperatorGetDeviceVehicleMappingList merchantId city :<|> transitOperatorUpsertDeviceVehicleMapping merchantId city :<|> transitOperatorUnblockBus merchantId city :<|> transitOperatorSearchStops merchantId city :<|> transitOperatorNearbyStops merchantId city :<|> transitOperatorBulkReplaceStops merchantId city :<|> transitOperatorRouteStops merchantId city :<|> transitOperatorInsertRouteStop merchantId city :<|> transitOperatorReprocessRoutes merchantId city :<|> transitOperatorExportRouteStopMapping merchantId city :<|> transitOperatorQueryVehicle merchantId city :<|> transitOperatorUpsertVehicles merchantId city :<|> transitOperatorDeleteVehicle merchantId city :<|> transitOperatorGetScheduleTripRepeat merchantId city :<|> transitOperatorSetScheduleTripRepeat merchantId city :<|> transitOperatorV2ListTripGroups merchantId city :<|> transitOperatorV2UpsertTripGroup merchantId city :<|> transitOperatorV2GetTripGroup merchantId city :<|> transitOperatorV2DeleteTripGroup merchantId city :<|> transitOperatorV2ListTrips merchantId city :<|> transitOperatorV2UpsertTrips merchantId city :<|> transitOperatorV2DeleteTrip merchantId city :<|> transitOperatorV2ListDutyRepeats merchantId city :<|> transitOperatorV2UpsertDutyRepeat merchantId city :<|> transitOperatorV2DeleteDutyRepeat merchantId city :<|> transitOperatorV2PreviewDutyRepeats merchantId city :<|> transitOperatorV2GenerateDutyRepeats merchantId city :<|> transitOperatorV2ListDutyGroups merchantId city :<|> transitOperatorV2CreateDutyGroup merchantId city :<|> transitOperatorV2GetDutyGroup merchantId city :<|> transitOperatorV2ListDuties merchantId city :<|> transitOperatorV2GetDutyGroupLogs merchantId city :<|> transitOperatorV2UpdateDutyGroupVehicle merchantId city :<|> transitOperatorV2UpdateDutyGroupCrew merchantId city :<|> transitOperatorV2SetDutyGroupActive merchantId city :<|> transitOperatorV2DeleteDutyGroup merchantId city :<|> transitOperatorV2UpdateDutyCrew merchantId city :<|> transitOperatorV2DeleteDuty merchantId city :<|> transitOperatorV2ListGenerationFailures merchantId city :<|> transitOperatorV2ResolveGenerationFailure merchantId city

type TransitOperatorGetRow =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_ROW))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetRow
  )

type TransitOperatorGetAllRows =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_ALL_ROWS))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetAllRows
  )

type TransitOperatorDeleteRow =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_DELETE_ROW))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorDeleteRow
  )

type TransitOperatorUpsertRow =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_UPSERT_ROW))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorUpsertRow
  )

type TransitOperatorUpsertRows =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_UPSERT_ROWS))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorUpsertRows
  )

type TransitOperatorQueryRows =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_QUERY_ROWS))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorQueryRows
  )

type TransitOperatorGetServiceTypes =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_SERVICE_TYPES))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetServiceTypes
  )

type TransitOperatorGetRoutes =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_ROUTES))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetRoutes
  )

type TransitOperatorGetDepots =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_DEPOTS))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetDepots
  )

type TransitOperatorGetShiftTypes =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_SHIFT_TYPES))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetShiftTypes
  )

type TransitOperatorGetScheduleNumbers =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_SCHEDULE_NUMBERS))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetScheduleNumbers
  )

type TransitOperatorGetDayTypes =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_DAY_TYPES))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetDayTypes
  )

type TransitOperatorGetTripTypes =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_TRIP_TYPES))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetTripTypes
  )

type TransitOperatorGetBreakTypes =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_BREAK_TYPES))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetBreakTypes
  )

type TransitOperatorGetTripDetails =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_TRIP_DETAILS))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetTripDetails
  )

type TransitOperatorGetFleets =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_FLEETS))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetFleets
  )

type TransitOperatorGetConductor =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_CONDUCTOR))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetConductor
  )

type TransitOperatorGetDriver =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_DRIVER))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetDriver
  )

type TransitOperatorGetDeviceIds =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_DEVICE_IDS))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetDeviceIds
  )

type TransitOperatorGetTabletIds =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_TABLET_IDS))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetTabletIds
  )

type TransitOperatorGetOperators =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_OPERATORS))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetOperators
  )

type TransitOperatorUpdateWaybillStatus =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_UPDATE_WAYBILL_STATUS))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorUpdateWaybillStatus
  )

type TransitOperatorUpdateWaybillFleet =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_UPDATE_WAYBILL_FLEET))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorUpdateWaybillFleet
  )

type TransitOperatorUpdateWaybillDetails =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_UPDATE_WAYBILL_DETAILS))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorUpdateWaybillDetails
  )

type TransitOperatorUpdateWaybillTablet =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_UPDATE_WAYBILL_TABLET))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorUpdateWaybillTablet
  )

type TransitOperatorGetWaybills =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_WAYBILLS))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetWaybills
  )

type TransitOperatorGetDeviceVehicleMappingList =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_DEVICE_VEHICLE_MAPPING_LIST))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetDeviceVehicleMappingList
  )

type TransitOperatorUpsertDeviceVehicleMapping =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_UPSERT_DEVICE_VEHICLE_MAPPING))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorUpsertDeviceVehicleMapping
  )

type TransitOperatorUnblockBus =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_UNBLOCK_BUS))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorUnblockBus
  )

type TransitOperatorSearchStops =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_SEARCH_STOPS))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorSearchStops
  )

type TransitOperatorNearbyStops =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_NEARBY_STOPS))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorNearbyStops
  )

type TransitOperatorBulkReplaceStops =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_BULK_REPLACE_STOPS))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorBulkReplaceStops
  )

type TransitOperatorRouteStops =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_ROUTE_STOPS))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorRouteStops
  )

type TransitOperatorInsertRouteStop =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_INSERT_ROUTE_STOP))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorInsertRouteStop
  )

type TransitOperatorReprocessRoutes =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_REPROCESS_ROUTES))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorReprocessRoutes
  )

type TransitOperatorExportRouteStopMapping =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_EXPORT_ROUTE_STOP_MAPPING))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorExportRouteStopMapping
  )

type TransitOperatorQueryVehicle =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_QUERY_VEHICLE))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorQueryVehicle
  )

type TransitOperatorUpsertVehicles =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_UPSERT_VEHICLES))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorUpsertVehicles
  )

type TransitOperatorDeleteVehicle =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_DELETE_VEHICLE))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorDeleteVehicle
  )

type TransitOperatorGetScheduleTripRepeat =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_SCHEDULE_TRIP_REPEAT))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetScheduleTripRepeat
  )

type TransitOperatorSetScheduleTripRepeat =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_SET_SCHEDULE_TRIP_REPEAT))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorSetScheduleTripRepeat
  )

type TransitOperatorV2ListTripGroups =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_V2_LIST_TRIP_GROUPS))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2ListTripGroups
  )

type TransitOperatorV2UpsertTripGroup =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_V2_UPSERT_TRIP_GROUP))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2UpsertTripGroup
  )

type TransitOperatorV2GetTripGroup =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_V2_GET_TRIP_GROUP))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2GetTripGroup
  )

type TransitOperatorV2DeleteTripGroup =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_V2_DELETE_TRIP_GROUP))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2DeleteTripGroup
  )

type TransitOperatorV2ListTrips =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_V2_LIST_TRIPS))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2ListTrips
  )

type TransitOperatorV2UpsertTrips =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_V2_UPSERT_TRIPS))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2UpsertTrips
  )

type TransitOperatorV2DeleteTrip =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_V2_DELETE_TRIP))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2DeleteTrip
  )

type TransitOperatorV2ListDutyRepeats =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_V2_LIST_DUTY_REPEATS))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2ListDutyRepeats
  )

type TransitOperatorV2UpsertDutyRepeat =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_V2_UPSERT_DUTY_REPEAT))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2UpsertDutyRepeat
  )

type TransitOperatorV2DeleteDutyRepeat =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_V2_DELETE_DUTY_REPEAT))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2DeleteDutyRepeat
  )

type TransitOperatorV2PreviewDutyRepeats =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_V2_PREVIEW_DUTY_REPEATS))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2PreviewDutyRepeats
  )

type TransitOperatorV2GenerateDutyRepeats =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_V2_GENERATE_DUTY_REPEATS))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2GenerateDutyRepeats
  )

type TransitOperatorV2ListDutyGroups =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_V2_LIST_DUTY_GROUPS))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2ListDutyGroups
  )

type TransitOperatorV2CreateDutyGroup =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_V2_CREATE_DUTY_GROUP))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2CreateDutyGroup
  )

type TransitOperatorV2GetDutyGroup =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_V2_GET_DUTY_GROUP))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2GetDutyGroup
  )

type TransitOperatorV2ListDuties =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_V2_LIST_DUTIES))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2ListDuties
  )

type TransitOperatorV2GetDutyGroupLogs =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_V2_GET_DUTY_GROUP_LOGS))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2GetDutyGroupLogs
  )

type TransitOperatorV2UpdateDutyGroupVehicle =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_V2_UPDATE_DUTY_GROUP_VEHICLE))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2UpdateDutyGroupVehicle
  )

type TransitOperatorV2UpdateDutyGroupCrew =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_V2_UPDATE_DUTY_GROUP_CREW))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2UpdateDutyGroupCrew
  )

type TransitOperatorV2SetDutyGroupActive =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_V2_SET_DUTY_GROUP_ACTIVE))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2SetDutyGroupActive
  )

type TransitOperatorV2DeleteDutyGroup =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_V2_DELETE_DUTY_GROUP))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2DeleteDutyGroup
  )

type TransitOperatorV2UpdateDutyCrew =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_V2_UPDATE_DUTY_CREW))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2UpdateDutyCrew
  )

type TransitOperatorV2DeleteDuty =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_V2_DELETE_DUTY))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2DeleteDuty
  )

type TransitOperatorV2ListGenerationFailures =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_V2_LIST_GENERATION_FAILURES))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2ListGenerationFailures
  )

type TransitOperatorV2ResolveGenerationFailure =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR) / ('API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_V2_RESOLVE_GENERATION_FAILURE))
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorV2ResolveGenerationFailure
  )

transitOperatorGetRow :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.Types.NandiRow)
transitOperatorGetRow merchantShortId opCity apiTokenInfo column table vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetRow merchantShortId opCity apiTokenInfo column table vehicleCategory

transitOperatorGetAllRows :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.NandiRow])
transitOperatorGetAllRows merchantShortId opCity apiTokenInfo limit offset table vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetAllRows merchantShortId opCity apiTokenInfo limit offset table vehicleCategory

transitOperatorDeleteRow :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Data.Aeson.Value -> Environment.FlowHandler SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorDeleteRow merchantShortId opCity apiTokenInfo table vehicleCategory req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorDeleteRow merchantShortId opCity apiTokenInfo table vehicleCategory req

transitOperatorUpsertRow :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Data.Aeson.Value -> Environment.FlowHandler SharedLogic.External.Nandi.Types.NandiRow)
transitOperatorUpsertRow merchantShortId opCity apiTokenInfo toRegen table vehicleCategory req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorUpsertRow merchantShortId opCity apiTokenInfo toRegen table vehicleCategory req

transitOperatorUpsertRows :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> [Data.Aeson.Value] -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.NandiRow])
transitOperatorUpsertRows merchantShortId opCity apiTokenInfo toRegen table vehicleCategory req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorUpsertRows merchantShortId opCity apiTokenInfo toRegen table vehicleCategory req

transitOperatorQueryRows :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.QueryBody -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.NandiRow])
transitOperatorQueryRows merchantShortId opCity apiTokenInfo table vehicleCategory req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorQueryRows merchantShortId opCity apiTokenInfo table vehicleCategory req

transitOperatorGetServiceTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.ServiceType])
transitOperatorGetServiceTypes merchantShortId opCity apiTokenInfo vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetServiceTypes merchantShortId opCity apiTokenInfo vehicleCategory

transitOperatorGetRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.NandiRoute])
transitOperatorGetRoutes merchantShortId opCity apiTokenInfo vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetRoutes merchantShortId opCity apiTokenInfo vehicleCategory

transitOperatorGetDepots :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.Depot])
transitOperatorGetDepots merchantShortId opCity apiTokenInfo vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetDepots merchantShortId opCity apiTokenInfo vehicleCategory

transitOperatorGetShiftTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.ShiftType])
transitOperatorGetShiftTypes merchantShortId opCity apiTokenInfo vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetShiftTypes merchantShortId opCity apiTokenInfo vehicleCategory

transitOperatorGetScheduleNumbers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.ScheduleNumber])
transitOperatorGetScheduleNumbers merchantShortId opCity apiTokenInfo vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetScheduleNumbers merchantShortId opCity apiTokenInfo vehicleCategory

transitOperatorGetDayTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.DayType])
transitOperatorGetDayTypes merchantShortId opCity apiTokenInfo vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetDayTypes merchantShortId opCity apiTokenInfo vehicleCategory

transitOperatorGetTripTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.TripType])
transitOperatorGetTripTypes merchantShortId opCity apiTokenInfo vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetTripTypes merchantShortId opCity apiTokenInfo vehicleCategory

transitOperatorGetBreakTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.BreakType])
transitOperatorGetBreakTypes merchantShortId opCity apiTokenInfo vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetBreakTypes merchantShortId opCity apiTokenInfo vehicleCategory

transitOperatorGetTripDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.NandiTripDetail])
transitOperatorGetTripDetails merchantShortId opCity apiTokenInfo scheduleNumber vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetTripDetails merchantShortId opCity apiTokenInfo scheduleNumber vehicleCategory

transitOperatorGetFleets :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.Fleet])
transitOperatorGetFleets merchantShortId opCity apiTokenInfo limit offset vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetFleets merchantShortId opCity apiTokenInfo limit offset vehicleCategory

transitOperatorGetConductor :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.Types.Employee)
transitOperatorGetConductor merchantShortId opCity apiTokenInfo token vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetConductor merchantShortId opCity apiTokenInfo token vehicleCategory

transitOperatorGetDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.Types.Employee)
transitOperatorGetDriver merchantShortId opCity apiTokenInfo token vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetDriver merchantShortId opCity apiTokenInfo token vehicleCategory

transitOperatorGetDeviceIds :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [Kernel.Prelude.Text])
transitOperatorGetDeviceIds merchantShortId opCity apiTokenInfo vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetDeviceIds merchantShortId opCity apiTokenInfo vehicleCategory

transitOperatorGetTabletIds :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [Kernel.Prelude.Text])
transitOperatorGetTabletIds merchantShortId opCity apiTokenInfo vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetTabletIds merchantShortId opCity apiTokenInfo vehicleCategory

transitOperatorGetOperators :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> SharedLogic.External.Nandi.Types.OperatorRole -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.Employee])
transitOperatorGetOperators merchantShortId opCity apiTokenInfo role vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetOperators merchantShortId opCity apiTokenInfo role vehicleCategory

transitOperatorUpdateWaybillStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillStatusReq -> Environment.FlowHandler SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorUpdateWaybillStatus merchantShortId opCity apiTokenInfo vehicleCategory req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorUpdateWaybillStatus merchantShortId opCity apiTokenInfo vehicleCategory req

transitOperatorUpdateWaybillFleet :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillFleetReq -> Environment.FlowHandler SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorUpdateWaybillFleet merchantShortId opCity apiTokenInfo vehicleCategory req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorUpdateWaybillFleet merchantShortId opCity apiTokenInfo vehicleCategory req

transitOperatorUpdateWaybillDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillDetailsReq -> Environment.FlowHandler SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorUpdateWaybillDetails merchantShortId opCity apiTokenInfo vehicleCategory req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorUpdateWaybillDetails merchantShortId opCity apiTokenInfo vehicleCategory req

transitOperatorUpdateWaybillTablet :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillTabletReq -> Environment.FlowHandler SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorUpdateWaybillTablet merchantShortId opCity apiTokenInfo vehicleCategory req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorUpdateWaybillTablet merchantShortId opCity apiTokenInfo vehicleCategory req

transitOperatorGetWaybills :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.NandiWaybillRow])
transitOperatorGetWaybills merchantShortId opCity apiTokenInfo limit offset vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetWaybills merchantShortId opCity apiTokenInfo limit offset vehicleCategory

transitOperatorGetDeviceVehicleMappingList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Environment.FlowHandler API.Types.Dashboard.AppManagement.TransitOperator.DeviceVehicleMappingListRes)
transitOperatorGetDeviceVehicleMappingList merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetDeviceVehicleMappingList merchantShortId opCity apiTokenInfo

transitOperatorUpsertDeviceVehicleMapping :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.Dashboard.AppManagement.TransitOperator.UpsertDeviceVehicleMappingReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.TransitOperator.UpsertDeviceVehicleMappingResp)
transitOperatorUpsertDeviceVehicleMapping merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorUpsertDeviceVehicleMapping merchantShortId opCity apiTokenInfo req

transitOperatorUnblockBus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
transitOperatorUnblockBus merchantShortId opCity apiTokenInfo vehicleNumber = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorUnblockBus merchantShortId opCity apiTokenInfo vehicleNumber

transitOperatorSearchStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.EnrichedStop])
transitOperatorSearchStops merchantShortId opCity apiTokenInfo limit withRoutes q vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorSearchStops merchantShortId opCity apiTokenInfo limit withRoutes q vehicleCategory

transitOperatorNearbyStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Double) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Double -> Kernel.Prelude.Double -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.EnrichedStop])
transitOperatorNearbyStops merchantShortId opCity apiTokenInfo limit radius withRoutes lat lon vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorNearbyStops merchantShortId opCity apiTokenInfo limit radius withRoutes lat lon vehicleCategory

transitOperatorBulkReplaceStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.BulkReplaceReq -> Environment.FlowHandler SharedLogic.External.Nandi.Types.BulkReplaceResult)
transitOperatorBulkReplaceStops merchantShortId opCity apiTokenInfo vehicleCategory req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorBulkReplaceStops merchantShortId opCity apiTokenInfo vehicleCategory req

transitOperatorRouteStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.Types.RouteStopsResponse)
transitOperatorRouteStops merchantShortId opCity apiTokenInfo routeId vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorRouteStops merchantShortId opCity apiTokenInfo routeId vehicleCategory

transitOperatorInsertRouteStop :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.InsertRouteStopReq -> Environment.FlowHandler SharedLogic.External.Nandi.Types.InsertRouteStopResp)
transitOperatorInsertRouteStop merchantShortId opCity apiTokenInfo routeId vehicleCategory req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorInsertRouteStop merchantShortId opCity apiTokenInfo routeId vehicleCategory req

transitOperatorReprocessRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.ReprocessReq -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.ReprocessResult])
transitOperatorReprocessRoutes merchantShortId opCity apiTokenInfo vehicleCategory req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorReprocessRoutes merchantShortId opCity apiTokenInfo vehicleCategory req

transitOperatorExportRouteStopMapping :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.RouteStopMappingExport])
transitOperatorExportRouteStopMapping merchantShortId opCity apiTokenInfo vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorExportRouteStopMapping merchantShortId opCity apiTokenInfo vehicleCategory

transitOperatorQueryVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.Fleet])
transitOperatorQueryVehicle merchantShortId opCity apiTokenInfo fleetNo tagNumber vehicleNo vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorQueryVehicle merchantShortId opCity apiTokenInfo fleetNo tagNumber vehicleNo vehicleCategory

transitOperatorUpsertVehicles :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> [SharedLogic.External.Nandi.Types.VehicleUpsertRequest] -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.Fleet])
transitOperatorUpsertVehicles merchantShortId opCity apiTokenInfo vehicleCategory req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorUpsertVehicles merchantShortId opCity apiTokenInfo vehicleCategory req

transitOperatorDeleteVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> Kernel.Prelude.Text -> Environment.FlowHandler SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorDeleteVehicle merchantShortId opCity apiTokenInfo vehicleCategory vehicleId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorDeleteVehicle merchantShortId opCity apiTokenInfo vehicleCategory vehicleId

transitOperatorGetScheduleTripRepeat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.Types.ScheduleTripRepeatConfig)
transitOperatorGetScheduleTripRepeat merchantShortId opCity apiTokenInfo scheduleTripId vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetScheduleTripRepeat merchantShortId opCity apiTokenInfo scheduleTripId vehicleCategory

transitOperatorSetScheduleTripRepeat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.SetScheduleTripRepeatReq -> Environment.FlowHandler SharedLogic.External.Nandi.Types.ScheduleTripRepeatConfig)
transitOperatorSetScheduleTripRepeat merchantShortId opCity apiTokenInfo scheduleTripId vehicleCategory req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorSetScheduleTripRepeat merchantShortId opCity apiTokenInfo scheduleTripId vehicleCategory req

transitOperatorV2ListTripGroups :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2TripGroupPage)
transitOperatorV2ListTripGroups merchantShortId opCity apiTokenInfo code conductorTokenNumber depotId driverTokenNumber limit offset operatorId search shift tripType vehicleNumber zone vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorV2ListTripGroups merchantShortId opCity apiTokenInfo code conductorTokenNumber depotId driverTokenNumber limit offset operatorId search shift tripType vehicleNumber zone vehicleCategory

transitOperatorV2UpsertTripGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpsertTripGroupReq -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2TripGroup)
transitOperatorV2UpsertTripGroup merchantShortId opCity apiTokenInfo operatorId vehicleCategory req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorV2UpsertTripGroup merchantShortId opCity apiTokenInfo operatorId vehicleCategory req

transitOperatorV2GetTripGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2TripGroup)
transitOperatorV2GetTripGroup merchantShortId opCity apiTokenInfo tripGroupId operatorId vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorV2GetTripGroup merchantShortId opCity apiTokenInfo tripGroupId operatorId vehicleCategory

transitOperatorV2DeleteTripGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp)
transitOperatorV2DeleteTripGroup merchantShortId opCity apiTokenInfo tripGroupId operatorId vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorV2DeleteTripGroup merchantShortId opCity apiTokenInfo tripGroupId operatorId vehicleCategory

transitOperatorV2ListTrips :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.TransitV2Types.V2Trip])
transitOperatorV2ListTrips merchantShortId opCity apiTokenInfo tripGroupId operatorId vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorV2ListTrips merchantShortId opCity apiTokenInfo tripGroupId operatorId vehicleCategory

transitOperatorV2UpsertTrips :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpsertTripsReq -> Environment.FlowHandler [SharedLogic.External.Nandi.TransitV2Types.V2Trip])
transitOperatorV2UpsertTrips merchantShortId opCity apiTokenInfo tripGroupId operatorId vehicleCategory req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorV2UpsertTrips merchantShortId opCity apiTokenInfo tripGroupId operatorId vehicleCategory req

transitOperatorV2DeleteTrip :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp)
transitOperatorV2DeleteTrip merchantShortId opCity apiTokenInfo tripId operatorId vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorV2DeleteTrip merchantShortId opCity apiTokenInfo tripId operatorId vehicleCategory

transitOperatorV2ListDutyRepeats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2DutyRepeatPage)
transitOperatorV2ListDutyRepeats merchantShortId opCity apiTokenInfo code conductorTokenNumber driverTokenNumber limit offset operatorId repeatStatus search tripGroupId vehicleNumber vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorV2ListDutyRepeats merchantShortId opCity apiTokenInfo code conductorTokenNumber driverTokenNumber limit offset operatorId repeatStatus search tripGroupId vehicleNumber vehicleCategory

transitOperatorV2UpsertDutyRepeat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpsertDutyRepeatReq -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2UpsertDutyRepeatResp)
transitOperatorV2UpsertDutyRepeat merchantShortId opCity apiTokenInfo operatorId vehicleCategory req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorV2UpsertDutyRepeat merchantShortId opCity apiTokenInfo operatorId vehicleCategory req

transitOperatorV2DeleteDutyRepeat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp)
transitOperatorV2DeleteDutyRepeat merchantShortId opCity apiTokenInfo dutyRepeatId operatorId vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorV2DeleteDutyRepeat merchantShortId opCity apiTokenInfo dutyRepeatId operatorId vehicleCategory

transitOperatorV2PreviewDutyRepeats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2GenerateReq -> Environment.FlowHandler [SharedLogic.External.Nandi.TransitV2Types.V2GenerateEntry])
transitOperatorV2PreviewDutyRepeats merchantShortId opCity apiTokenInfo operatorId vehicleCategory req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorV2PreviewDutyRepeats merchantShortId opCity apiTokenInfo operatorId vehicleCategory req

transitOperatorV2GenerateDutyRepeats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2GenerateReq -> Environment.FlowHandler [SharedLogic.External.Nandi.TransitV2Types.V2GenerateEntry])
transitOperatorV2GenerateDutyRepeats merchantShortId opCity apiTokenInfo operatorId vehicleCategory req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorV2GenerateDutyRepeats merchantShortId opCity apiTokenInfo operatorId vehicleCategory req

transitOperatorV2ListDutyGroups :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupPage)
transitOperatorV2ListDutyGroups merchantShortId opCity apiTokenInfo code conductorTokenNumber depotId driverTokenNumber isActive limit offset operationDate operatorId search tripGroupId vehicleNumber vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorV2ListDutyGroups merchantShortId opCity apiTokenInfo code conductorTokenNumber depotId driverTokenNumber isActive limit offset operationDate operatorId search tripGroupId vehicleNumber vehicleCategory

transitOperatorV2CreateDutyGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2CreateDutyGroupReq -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupDetail)
transitOperatorV2CreateDutyGroup merchantShortId opCity apiTokenInfo operatorId vehicleCategory req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorV2CreateDutyGroup merchantShortId opCity apiTokenInfo operatorId vehicleCategory req

transitOperatorV2GetDutyGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupDetail)
transitOperatorV2GetDutyGroup merchantShortId opCity apiTokenInfo dutyGroupId operatorId vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorV2GetDutyGroup merchantShortId opCity apiTokenInfo dutyGroupId operatorId vehicleCategory

transitOperatorV2ListDuties :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.TransitV2Types.V2Duty])
transitOperatorV2ListDuties merchantShortId opCity apiTokenInfo dutyGroupId operatorId vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorV2ListDuties merchantShortId opCity apiTokenInfo dutyGroupId operatorId vehicleCategory

transitOperatorV2GetDutyGroupLogs :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.TransitV2Types.V2DutyEventLog])
transitOperatorV2GetDutyGroupLogs merchantShortId opCity apiTokenInfo dutyGroupId operatorId vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorV2GetDutyGroupLogs merchantShortId opCity apiTokenInfo dutyGroupId operatorId vehicleCategory

transitOperatorV2UpdateDutyGroupVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpdateVehicleReq -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2DutyGroup)
transitOperatorV2UpdateDutyGroupVehicle merchantShortId opCity apiTokenInfo dutyGroupId operatorId vehicleCategory req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorV2UpdateDutyGroupVehicle merchantShortId opCity apiTokenInfo dutyGroupId operatorId vehicleCategory req

transitOperatorV2UpdateDutyGroupCrew :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpdateCrewReq -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupDetail)
transitOperatorV2UpdateDutyGroupCrew merchantShortId opCity apiTokenInfo dutyGroupId operatorId vehicleCategory req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorV2UpdateDutyGroupCrew merchantShortId opCity apiTokenInfo dutyGroupId operatorId vehicleCategory req

transitOperatorV2SetDutyGroupActive :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2SetActiveReq -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2DutyGroup)
transitOperatorV2SetDutyGroupActive merchantShortId opCity apiTokenInfo dutyGroupId operatorId vehicleCategory req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorV2SetDutyGroupActive merchantShortId opCity apiTokenInfo dutyGroupId operatorId vehicleCategory req

transitOperatorV2DeleteDutyGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp)
transitOperatorV2DeleteDutyGroup merchantShortId opCity apiTokenInfo dutyGroupId operatorId vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorV2DeleteDutyGroup merchantShortId opCity apiTokenInfo dutyGroupId operatorId vehicleCategory

transitOperatorV2UpdateDutyCrew :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpdateCrewReq -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2Duty)
transitOperatorV2UpdateDutyCrew merchantShortId opCity apiTokenInfo dutyId operatorId vehicleCategory req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorV2UpdateDutyCrew merchantShortId opCity apiTokenInfo dutyId operatorId vehicleCategory req

transitOperatorV2DeleteDuty :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp)
transitOperatorV2DeleteDuty merchantShortId opCity apiTokenInfo dutyId operatorId vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorV2DeleteDuty merchantShortId opCity apiTokenInfo dutyId operatorId vehicleCategory

transitOperatorV2ListGenerationFailures :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2DutyEventLogPage)
transitOperatorV2ListGenerationFailures merchantShortId opCity apiTokenInfo limit offset operatorId resolved vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorV2ListGenerationFailures merchantShortId opCity apiTokenInfo limit offset operatorId resolved vehicleCategory

transitOperatorV2ResolveGenerationFailure :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2DutyEventLog)
transitOperatorV2ResolveGenerationFailure merchantShortId opCity apiTokenInfo failureId operatorId vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorV2ResolveGenerationFailure merchantShortId opCity apiTokenInfo failureId operatorId vehicleCategory
