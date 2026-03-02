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
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified "this" SharedLogic.External.Nandi.Types
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.TransitOperator.API)
handler merchantId city = transitOperatorGetRow merchantId city :<|> transitOperatorGetAllRows merchantId city :<|> transitOperatorDeleteRow merchantId city :<|> transitOperatorUpsertRow merchantId city :<|> transitOperatorGetServiceTypes merchantId city :<|> transitOperatorGetRoutes merchantId city :<|> transitOperatorGetDepots merchantId city :<|> transitOperatorGetShiftTypes merchantId city :<|> transitOperatorGetScheduleNumbers merchantId city :<|> transitOperatorGetDayTypes merchantId city :<|> transitOperatorGetTripTypes merchantId city :<|> transitOperatorGetBreakTypes merchantId city :<|> transitOperatorGetTripDetails merchantId city :<|> transitOperatorGetFleets merchantId city :<|> transitOperatorGetConductor merchantId city :<|> transitOperatorGetDriver merchantId city :<|> transitOperatorGetDeviceIds merchantId city :<|> transitOperatorGetTabletIds merchantId city :<|> transitOperatorGetOperators merchantId city :<|> transitOperatorUpdateWaybillStatus merchantId city :<|> transitOperatorUpdateWaybillFleet merchantId city :<|> transitOperatorUpdateWaybillTablet merchantId city :<|> transitOperatorGetWaybills merchantId city

transitOperatorGetRow :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.Types.NandiRow)
transitOperatorGetRow a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetRow a5 a4 a3 a2 a1

transitOperatorGetAllRows :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.NandiRow])
transitOperatorGetAllRows a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetAllRows a6 a5 a4 a3 a2 a1

transitOperatorDeleteRow :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Data.Aeson.Value -> Environment.FlowHandler SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorDeleteRow a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorDeleteRow a5 a4 a3 a2 a1

transitOperatorUpsertRow :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Data.Aeson.Value -> Environment.FlowHandler SharedLogic.External.Nandi.Types.NandiRow)
transitOperatorUpsertRow a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorUpsertRow a5 a4 a3 a2 a1

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

transitOperatorGetFleets :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.Fleet])
transitOperatorGetFleets a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetFleets a3 a2 a1

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

transitOperatorUpdateWaybillTablet :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillTabletReq -> Environment.FlowHandler SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorUpdateWaybillTablet a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorUpdateWaybillTablet a4 a3 a2 a1

transitOperatorGetWaybills :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.NandiWaybillRow])
transitOperatorGetWaybills a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.TransitOperator.transitOperatorGetWaybills a5 a4 a3 a2 a1
