{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.AppManagement.TransitOperator
  ( API.Types.Dashboard.AppManagement.TransitOperator.API,
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.TransitOperator.API)
handler merchantId city = transitOperatorGetRow merchantId city :<|> transitOperatorGetAllRows merchantId city :<|> transitOperatorDeleteRow merchantId city :<|> transitOperatorUpsertRow merchantId city :<|> transitOperatorUpsertRows merchantId city :<|> transitOperatorQueryRows merchantId city :<|> transitOperatorGetServiceTypes merchantId city :<|> transitOperatorGetRoutes merchantId city :<|> transitOperatorGetDepots merchantId city :<|> transitOperatorGetShiftTypes merchantId city :<|> transitOperatorGetScheduleNumbers merchantId city :<|> transitOperatorGetDayTypes merchantId city :<|> transitOperatorGetTripTypes merchantId city :<|> transitOperatorGetBreakTypes merchantId city :<|> transitOperatorGetTripDetails merchantId city :<|> transitOperatorGetFleets merchantId city :<|> transitOperatorGetConductor merchantId city :<|> transitOperatorGetDriver merchantId city :<|> transitOperatorGetDeviceIds merchantId city :<|> transitOperatorGetTabletIds merchantId city :<|> transitOperatorGetOperators merchantId city :<|> transitOperatorUpdateWaybillStatus merchantId city :<|> transitOperatorUpdateWaybillFleet merchantId city :<|> transitOperatorUpdateWaybillDetails merchantId city :<|> transitOperatorUpdateWaybillTablet merchantId city :<|> transitOperatorGetWaybills merchantId city :<|> transitOperatorGetDeviceVehicleMappingList merchantId city :<|> transitOperatorUpsertDeviceVehicleMapping merchantId city :<|> transitOperatorUnblockBus merchantId city :<|> transitOperatorSearchStops merchantId city :<|> transitOperatorNearbyStops merchantId city :<|> transitOperatorBulkReplaceStops merchantId city :<|> transitOperatorRouteStops merchantId city :<|> transitOperatorInsertRouteStop merchantId city :<|> transitOperatorReprocessRoutes merchantId city :<|> transitOperatorExportRouteStopMapping merchantId city :<|> transitOperatorQueryVehicle merchantId city :<|> transitOperatorUpsertVehicles merchantId city :<|> transitOperatorDeleteVehicle merchantId city :<|> transitOperatorGetScheduleTripRepeat merchantId city :<|> transitOperatorSetScheduleTripRepeat merchantId city :<|> transitOperatorV2ListTripGroups merchantId city :<|> transitOperatorV2UpsertTripGroup merchantId city :<|> transitOperatorV2GetTripGroup merchantId city :<|> transitOperatorV2DeleteTripGroup merchantId city :<|> transitOperatorV2ListTrips merchantId city :<|> transitOperatorV2UpsertTrips merchantId city :<|> transitOperatorV2DeleteTrip merchantId city :<|> transitOperatorV2ListDutyRepeats merchantId city :<|> transitOperatorV2UpsertDutyRepeat merchantId city :<|> transitOperatorV2DeleteDutyRepeat merchantId city :<|> transitOperatorV2PreviewDutyRepeats merchantId city :<|> transitOperatorV2GenerateDutyRepeats merchantId city :<|> transitOperatorV2ListDutyGroups merchantId city :<|> transitOperatorV2CreateDutyGroup merchantId city :<|> transitOperatorV2GetDutyGroup merchantId city :<|> transitOperatorV2ListDuties merchantId city :<|> transitOperatorV2GetDutyGroupLogs merchantId city :<|> transitOperatorV2UpdateDutyGroupVehicle merchantId city :<|> transitOperatorV2UpdateDutyGroupCrew merchantId city :<|> transitOperatorV2SetDutyGroupActive merchantId city :<|> transitOperatorV2DeleteDutyGroup merchantId city :<|> transitOperatorV2UpdateDutyCrew merchantId city :<|> transitOperatorV2DeleteDuty merchantId city :<|> transitOperatorV2ListGenerationFailures merchantId city :<|> transitOperatorV2ResolveGenerationFailure merchantId city

transitOperatorGetRow :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.Types.NandiRow)
transitOperatorGetRow a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetRow a5 a4 a3 a2 a1

transitOperatorGetAllRows :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.NandiRow])
transitOperatorGetAllRows a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetAllRows a6 a5 a4 a3 a2 a1

transitOperatorDeleteRow :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Data.Aeson.Value -> Environment.FlowHandler SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorDeleteRow a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorDeleteRow a5 a4 a3 a2 a1

transitOperatorUpsertRow :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Data.Aeson.Value -> Environment.FlowHandler SharedLogic.External.Nandi.Types.NandiRow)
transitOperatorUpsertRow a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorUpsertRow a6 a5 a4 a3 a2 a1

transitOperatorUpsertRows :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> [Data.Aeson.Value] -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.NandiRow])
transitOperatorUpsertRows a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorUpsertRows a6 a5 a4 a3 a2 a1

transitOperatorQueryRows :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.QueryBody -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.NandiRow])
transitOperatorQueryRows a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorQueryRows a5 a4 a3 a2 a1

transitOperatorGetServiceTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.ServiceType])
transitOperatorGetServiceTypes a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetServiceTypes a3 a2 a1

transitOperatorGetRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.NandiRoute])
transitOperatorGetRoutes a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetRoutes a3 a2 a1

transitOperatorGetDepots :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.Depot])
transitOperatorGetDepots a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetDepots a3 a2 a1

transitOperatorGetShiftTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.ShiftType])
transitOperatorGetShiftTypes a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetShiftTypes a3 a2 a1

transitOperatorGetScheduleNumbers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.ScheduleNumber])
transitOperatorGetScheduleNumbers a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetScheduleNumbers a3 a2 a1

transitOperatorGetDayTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.DayType])
transitOperatorGetDayTypes a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetDayTypes a3 a2 a1

transitOperatorGetTripTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.TripType])
transitOperatorGetTripTypes a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetTripTypes a3 a2 a1

transitOperatorGetBreakTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.BreakType])
transitOperatorGetBreakTypes a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetBreakTypes a3 a2 a1

transitOperatorGetTripDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.NandiTripDetail])
transitOperatorGetTripDetails a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetTripDetails a4 a3 a2 a1

transitOperatorGetFleets :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.Fleet])
transitOperatorGetFleets a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetFleets a5 a4 a3 a2 a1

transitOperatorGetConductor :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.Types.Employee)
transitOperatorGetConductor a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetConductor a4 a3 a2 a1

transitOperatorGetDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.Types.Employee)
transitOperatorGetDriver a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetDriver a4 a3 a2 a1

transitOperatorGetDeviceIds :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [Kernel.Prelude.Text])
transitOperatorGetDeviceIds a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetDeviceIds a3 a2 a1

transitOperatorGetTabletIds :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [Kernel.Prelude.Text])
transitOperatorGetTabletIds a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetTabletIds a3 a2 a1

transitOperatorGetOperators :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> SharedLogic.External.Nandi.Types.OperatorRole -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.Employee])
transitOperatorGetOperators a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetOperators a4 a3 a2 a1

transitOperatorUpdateWaybillStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillStatusReq -> Environment.FlowHandler SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorUpdateWaybillStatus a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorUpdateWaybillStatus a4 a3 a2 a1

transitOperatorUpdateWaybillFleet :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillFleetReq -> Environment.FlowHandler SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorUpdateWaybillFleet a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorUpdateWaybillFleet a4 a3 a2 a1

transitOperatorUpdateWaybillDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillDetailsReq -> Environment.FlowHandler SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorUpdateWaybillDetails a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorUpdateWaybillDetails a4 a3 a2 a1

transitOperatorUpdateWaybillTablet :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillTabletReq -> Environment.FlowHandler SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorUpdateWaybillTablet a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorUpdateWaybillTablet a4 a3 a2 a1

transitOperatorGetWaybills :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.NandiWaybillRow])
transitOperatorGetWaybills a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetWaybills a5 a4 a3 a2 a1

transitOperatorGetDeviceVehicleMappingList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler API.Types.Dashboard.AppManagement.TransitOperator.DeviceVehicleMappingListRes)
transitOperatorGetDeviceVehicleMappingList a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetDeviceVehicleMappingList a2 a1

transitOperatorUpsertDeviceVehicleMapping :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.TransitOperator.UpsertDeviceVehicleMappingReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.TransitOperator.UpsertDeviceVehicleMappingResp)
transitOperatorUpsertDeviceVehicleMapping a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorUpsertDeviceVehicleMapping a3 a2 a1

transitOperatorUnblockBus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
transitOperatorUnblockBus a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorUnblockBus a3 a2 a1

transitOperatorSearchStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.EnrichedStop])
transitOperatorSearchStops a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorSearchStops a6 a5 a4 a3 a2 a1

transitOperatorNearbyStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Double) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Double -> Kernel.Prelude.Double -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.EnrichedStop])
transitOperatorNearbyStops a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorNearbyStops a8 a7 a6 a5 a4 a3 a2 a1

transitOperatorBulkReplaceStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.BulkReplaceReq -> Environment.FlowHandler SharedLogic.External.Nandi.Types.BulkReplaceResult)
transitOperatorBulkReplaceStops a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorBulkReplaceStops a4 a3 a2 a1

transitOperatorRouteStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.Types.RouteStopsResponse)
transitOperatorRouteStops a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorRouteStops a4 a3 a2 a1

transitOperatorInsertRouteStop :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.InsertRouteStopReq -> Environment.FlowHandler SharedLogic.External.Nandi.Types.InsertRouteStopResp)
transitOperatorInsertRouteStop a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorInsertRouteStop a5 a4 a3 a2 a1

transitOperatorReprocessRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.ReprocessReq -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.ReprocessResult])
transitOperatorReprocessRoutes a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorReprocessRoutes a4 a3 a2 a1

transitOperatorExportRouteStopMapping :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.RouteStopMappingExport])
transitOperatorExportRouteStopMapping a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorExportRouteStopMapping a3 a2 a1

transitOperatorQueryVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.Fleet])
transitOperatorQueryVehicle a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorQueryVehicle a6 a5 a4 a3 a2 a1

transitOperatorUpsertVehicles :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> [SharedLogic.External.Nandi.Types.VehicleUpsertRequest] -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.Fleet])
transitOperatorUpsertVehicles a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorUpsertVehicles a4 a3 a2 a1

transitOperatorDeleteVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Kernel.Prelude.Text -> Environment.FlowHandler SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorDeleteVehicle a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorDeleteVehicle a4 a3 a2 a1

transitOperatorGetScheduleTripRepeat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.Types.ScheduleTripRepeatConfig)
transitOperatorGetScheduleTripRepeat a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetScheduleTripRepeat a4 a3 a2 a1

transitOperatorSetScheduleTripRepeat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.SetScheduleTripRepeatReq -> Environment.FlowHandler SharedLogic.External.Nandi.Types.ScheduleTripRepeatConfig)
transitOperatorSetScheduleTripRepeat a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorSetScheduleTripRepeat a5 a4 a3 a2 a1

transitOperatorV2ListTripGroups :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2TripGroupPage)
transitOperatorV2ListTripGroups a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a2) $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2ListTripGroups a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

transitOperatorV2UpsertTripGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpsertTripGroupReq -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2TripGroup)
transitOperatorV2UpsertTripGroup a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a3) $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2UpsertTripGroup a6 a5 a4 a3 a2 a1

transitOperatorV2GetTripGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2TripGroup)
transitOperatorV2GetTripGroup a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a2) $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2GetTripGroup a6 a5 a4 a3 a2 a1

transitOperatorV2DeleteTripGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp)
transitOperatorV2DeleteTripGroup a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a2) $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2DeleteTripGroup a6 a5 a4 a3 a2 a1

transitOperatorV2ListTrips :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.TransitV2Types.V2Trip])
transitOperatorV2ListTrips a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a2) $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2ListTrips a6 a5 a4 a3 a2 a1

transitOperatorV2UpsertTrips :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpsertTripsReq -> Environment.FlowHandler [SharedLogic.External.Nandi.TransitV2Types.V2Trip])
transitOperatorV2UpsertTrips a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a3) $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2UpsertTrips a7 a6 a5 a4 a3 a2 a1

transitOperatorV2DeleteTrip :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp)
transitOperatorV2DeleteTrip a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a2) $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2DeleteTrip a6 a5 a4 a3 a2 a1

transitOperatorV2ListDutyRepeats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2DutyRepeatPage)
transitOperatorV2ListDutyRepeats a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a2) $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2ListDutyRepeats a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

transitOperatorV2UpsertDutyRepeat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpsertDutyRepeatReq -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2UpsertDutyRepeatResp)
transitOperatorV2UpsertDutyRepeat a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a3) $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2UpsertDutyRepeat a6 a5 a4 a3 a2 a1

transitOperatorV2DeleteDutyRepeat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp)
transitOperatorV2DeleteDutyRepeat a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a2) $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2DeleteDutyRepeat a6 a5 a4 a3 a2 a1

transitOperatorV2PreviewDutyRepeats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2GenerateReq -> Environment.FlowHandler [SharedLogic.External.Nandi.TransitV2Types.V2GenerateEntry])
transitOperatorV2PreviewDutyRepeats a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a3) $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2PreviewDutyRepeats a6 a5 a4 a3 a2 a1

transitOperatorV2GenerateDutyRepeats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2GenerateReq -> Environment.FlowHandler [SharedLogic.External.Nandi.TransitV2Types.V2GenerateEntry])
transitOperatorV2GenerateDutyRepeats a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a3) $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2GenerateDutyRepeats a6 a5 a4 a3 a2 a1

transitOperatorV2ListDutyGroups :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupPage)
transitOperatorV2ListDutyGroups a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a2) $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2ListDutyGroups a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

transitOperatorV2CreateDutyGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2CreateDutyGroupReq -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupDetail)
transitOperatorV2CreateDutyGroup a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a3) $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2CreateDutyGroup a6 a5 a4 a3 a2 a1

transitOperatorV2GetDutyGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupDetail)
transitOperatorV2GetDutyGroup a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a2) $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2GetDutyGroup a6 a5 a4 a3 a2 a1

transitOperatorV2ListDuties :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.TransitV2Types.V2Duty])
transitOperatorV2ListDuties a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a2) $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2ListDuties a6 a5 a4 a3 a2 a1

transitOperatorV2GetDutyGroupLogs :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.TransitV2Types.V2DutyEventLog])
transitOperatorV2GetDutyGroupLogs a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a2) $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2GetDutyGroupLogs a6 a5 a4 a3 a2 a1

transitOperatorV2UpdateDutyGroupVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpdateVehicleReq -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2DutyGroup)
transitOperatorV2UpdateDutyGroupVehicle a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a3) $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2UpdateDutyGroupVehicle a7 a6 a5 a4 a3 a2 a1

transitOperatorV2UpdateDutyGroupCrew :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpdateCrewReq -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupDetail)
transitOperatorV2UpdateDutyGroupCrew a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a3) $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2UpdateDutyGroupCrew a7 a6 a5 a4 a3 a2 a1

transitOperatorV2SetDutyGroupActive :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2SetActiveReq -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2DutyGroup)
transitOperatorV2SetDutyGroupActive a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a3) $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2SetDutyGroupActive a7 a6 a5 a4 a3 a2 a1

transitOperatorV2DeleteDutyGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp)
transitOperatorV2DeleteDutyGroup a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a2) $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2DeleteDutyGroup a6 a5 a4 a3 a2 a1

transitOperatorV2UpdateDutyCrew :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpdateCrewReq -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2Duty)
transitOperatorV2UpdateDutyCrew a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a3) $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2UpdateDutyCrew a7 a6 a5 a4 a3 a2 a1

transitOperatorV2DeleteDuty :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp)
transitOperatorV2DeleteDuty a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a2) $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2DeleteDuty a6 a5 a4 a3 a2 a1

transitOperatorV2ListGenerationFailures :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2DutyEventLogPage)
transitOperatorV2ListGenerationFailures a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a2) $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2ListGenerationFailures a8 a7 a6 a5 a4 a3 a2 a1

transitOperatorV2ResolveGenerationFailure :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.TransitV2Types.V2DutyEventLog)
transitOperatorV2ResolveGenerationFailure a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a2) $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorV2ResolveGenerationFailure a6 a5 a4 a3 a2 a1
