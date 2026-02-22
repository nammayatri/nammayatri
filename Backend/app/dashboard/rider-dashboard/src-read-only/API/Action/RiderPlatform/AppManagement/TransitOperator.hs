{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.AppManagement.TransitOperator
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement.TransitOperator
import qualified "beckn-spec" BecknV2.OnDemand.Enums
import qualified Data.Aeson
import qualified Domain.Action.RiderPlatform.AppManagement.TransitOperator
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified "rider-app" SharedLogic.External.Nandi.Types
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("transitOperator" :> (TransitOperatorGetRow :<|> TransitOperatorGetAllRows :<|> TransitOperatorDeleteRow :<|> TransitOperatorUpsertRow :<|> TransitOperatorGetServiceTypes :<|> TransitOperatorGetRoutes :<|> TransitOperatorGetDepots :<|> TransitOperatorGetShiftTypes :<|> TransitOperatorGetScheduleNumbers :<|> TransitOperatorGetDayTypes :<|> TransitOperatorGetTripTypes :<|> TransitOperatorGetBreakTypes :<|> TransitOperatorGetTripDetails :<|> TransitOperatorGetFleets :<|> TransitOperatorGetConductor :<|> TransitOperatorGetDriver :<|> TransitOperatorGetDeviceIds :<|> TransitOperatorGetTabletIds :<|> TransitOperatorGetOperators :<|> TransitOperatorUpdateWaybillStatus :<|> TransitOperatorUpdateWaybillFleet :<|> TransitOperatorUpdateWaybillTablet :<|> TransitOperatorGetWaybills))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = transitOperatorGetRow merchantId city :<|> transitOperatorGetAllRows merchantId city :<|> transitOperatorDeleteRow merchantId city :<|> transitOperatorUpsertRow merchantId city :<|> transitOperatorGetServiceTypes merchantId city :<|> transitOperatorGetRoutes merchantId city :<|> transitOperatorGetDepots merchantId city :<|> transitOperatorGetShiftTypes merchantId city :<|> transitOperatorGetScheduleNumbers merchantId city :<|> transitOperatorGetDayTypes merchantId city :<|> transitOperatorGetTripTypes merchantId city :<|> transitOperatorGetBreakTypes merchantId city :<|> transitOperatorGetTripDetails merchantId city :<|> transitOperatorGetFleets merchantId city :<|> transitOperatorGetConductor merchantId city :<|> transitOperatorGetDriver merchantId city :<|> transitOperatorGetDeviceIds merchantId city :<|> transitOperatorGetTabletIds merchantId city :<|> transitOperatorGetOperators merchantId city :<|> transitOperatorUpdateWaybillStatus merchantId city :<|> transitOperatorUpdateWaybillFleet merchantId city :<|> transitOperatorUpdateWaybillTablet merchantId city :<|> transitOperatorGetWaybills merchantId city

type TransitOperatorGetRow =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR / 'API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_ROW)
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetRow
  )

type TransitOperatorGetAllRows =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR / 'API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_ALL_ROWS)
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetAllRows
  )

type TransitOperatorDeleteRow =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR / 'API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_DELETE_ROW)
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorDeleteRow
  )

type TransitOperatorUpsertRow =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR / 'API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_UPSERT_ROW)
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorUpsertRow
  )

type TransitOperatorGetServiceTypes =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR / 'API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_SERVICE_TYPES)
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetServiceTypes
  )

type TransitOperatorGetRoutes =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR / 'API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_ROUTES)
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetRoutes
  )

type TransitOperatorGetDepots =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR / 'API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_DEPOTS)
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetDepots
  )

type TransitOperatorGetShiftTypes =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR / 'API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_SHIFT_TYPES)
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetShiftTypes
  )

type TransitOperatorGetScheduleNumbers =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR / 'API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_SCHEDULE_NUMBERS)
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetScheduleNumbers
  )

type TransitOperatorGetDayTypes =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR / 'API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_DAY_TYPES)
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetDayTypes
  )

type TransitOperatorGetTripTypes =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR / 'API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_TRIP_TYPES)
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetTripTypes
  )

type TransitOperatorGetBreakTypes =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR / 'API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_BREAK_TYPES)
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetBreakTypes
  )

type TransitOperatorGetTripDetails =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR / 'API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_TRIP_DETAILS)
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetTripDetails
  )

type TransitOperatorGetFleets =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR / 'API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_FLEETS)
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetFleets
  )

type TransitOperatorGetConductor =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR / 'API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_CONDUCTOR)
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetConductor
  )

type TransitOperatorGetDriver =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR / 'API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_DRIVER)
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetDriver
  )

type TransitOperatorGetDeviceIds =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR / 'API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_DEVICE_IDS)
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetDeviceIds
  )

type TransitOperatorGetTabletIds =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR / 'API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_TABLET_IDS)
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetTabletIds
  )

type TransitOperatorGetOperators =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR / 'API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_OPERATORS)
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetOperators
  )

type TransitOperatorUpdateWaybillStatus =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR / 'API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_UPDATE_WAYBILL_STATUS)
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorUpdateWaybillStatus
  )

type TransitOperatorUpdateWaybillFleet =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR / 'API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_UPDATE_WAYBILL_FLEET)
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorUpdateWaybillFleet
  )

type TransitOperatorUpdateWaybillTablet =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR / 'API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_UPDATE_WAYBILL_TABLET)
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorUpdateWaybillTablet
  )

type TransitOperatorGetWaybills =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.TRANSIT_OPERATOR / 'API.Types.Dashboard.AppManagement.TransitOperator.TRANSIT_OPERATOR_GET_WAYBILLS)
      :> API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorGetWaybills
  )

transitOperatorGetRow :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.Types.NandiRow)
transitOperatorGetRow merchantShortId opCity apiTokenInfo column table vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetRow merchantShortId opCity apiTokenInfo column table vehicleCategory

transitOperatorGetAllRows :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.NandiRow])
transitOperatorGetAllRows merchantShortId opCity apiTokenInfo limit offset table vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetAllRows merchantShortId opCity apiTokenInfo limit offset table vehicleCategory

transitOperatorDeleteRow :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Data.Aeson.Value -> Environment.FlowHandler SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorDeleteRow merchantShortId opCity apiTokenInfo table vehicleCategory req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorDeleteRow merchantShortId opCity apiTokenInfo table vehicleCategory req

transitOperatorUpsertRow :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Data.Aeson.Value -> Environment.FlowHandler SharedLogic.External.Nandi.Types.NandiRow)
transitOperatorUpsertRow merchantShortId opCity apiTokenInfo table vehicleCategory req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorUpsertRow merchantShortId opCity apiTokenInfo table vehicleCategory req

transitOperatorGetServiceTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.ServiceType])
transitOperatorGetServiceTypes merchantShortId opCity apiTokenInfo vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetServiceTypes merchantShortId opCity apiTokenInfo vehicleCategory

transitOperatorGetRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.NandiRoute])
transitOperatorGetRoutes merchantShortId opCity apiTokenInfo vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetRoutes merchantShortId opCity apiTokenInfo vehicleCategory

transitOperatorGetDepots :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.Depot])
transitOperatorGetDepots merchantShortId opCity apiTokenInfo vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetDepots merchantShortId opCity apiTokenInfo vehicleCategory

transitOperatorGetShiftTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.ShiftType])
transitOperatorGetShiftTypes merchantShortId opCity apiTokenInfo vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetShiftTypes merchantShortId opCity apiTokenInfo vehicleCategory

transitOperatorGetScheduleNumbers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.ScheduleNumber])
transitOperatorGetScheduleNumbers merchantShortId opCity apiTokenInfo vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetScheduleNumbers merchantShortId opCity apiTokenInfo vehicleCategory

transitOperatorGetDayTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.DayType])
transitOperatorGetDayTypes merchantShortId opCity apiTokenInfo vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetDayTypes merchantShortId opCity apiTokenInfo vehicleCategory

transitOperatorGetTripTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.TripType])
transitOperatorGetTripTypes merchantShortId opCity apiTokenInfo vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetTripTypes merchantShortId opCity apiTokenInfo vehicleCategory

transitOperatorGetBreakTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.BreakType])
transitOperatorGetBreakTypes merchantShortId opCity apiTokenInfo vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetBreakTypes merchantShortId opCity apiTokenInfo vehicleCategory

transitOperatorGetTripDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.NandiTripDetail])
transitOperatorGetTripDetails merchantShortId opCity apiTokenInfo scheduleNumber vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetTripDetails merchantShortId opCity apiTokenInfo scheduleNumber vehicleCategory

transitOperatorGetFleets :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.Fleet])
transitOperatorGetFleets merchantShortId opCity apiTokenInfo vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetFleets merchantShortId opCity apiTokenInfo vehicleCategory

transitOperatorGetConductor :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.Types.Employee)
transitOperatorGetConductor merchantShortId opCity apiTokenInfo token vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetConductor merchantShortId opCity apiTokenInfo token vehicleCategory

transitOperatorGetDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler SharedLogic.External.Nandi.Types.Employee)
transitOperatorGetDriver merchantShortId opCity apiTokenInfo token vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetDriver merchantShortId opCity apiTokenInfo token vehicleCategory

transitOperatorGetDeviceIds :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [Kernel.Prelude.Text])
transitOperatorGetDeviceIds merchantShortId opCity apiTokenInfo vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetDeviceIds merchantShortId opCity apiTokenInfo vehicleCategory

transitOperatorGetTabletIds :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [Kernel.Prelude.Text])
transitOperatorGetTabletIds merchantShortId opCity apiTokenInfo vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetTabletIds merchantShortId opCity apiTokenInfo vehicleCategory

transitOperatorGetOperators :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> SharedLogic.External.Nandi.Types.OperatorRole -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.Employee])
transitOperatorGetOperators merchantShortId opCity apiTokenInfo role vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetOperators merchantShortId opCity apiTokenInfo role vehicleCategory

transitOperatorUpdateWaybillStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillStatusReq -> Environment.FlowHandler SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorUpdateWaybillStatus merchantShortId opCity apiTokenInfo vehicleCategory req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorUpdateWaybillStatus merchantShortId opCity apiTokenInfo vehicleCategory req

transitOperatorUpdateWaybillFleet :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillFleetReq -> Environment.FlowHandler SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorUpdateWaybillFleet merchantShortId opCity apiTokenInfo vehicleCategory req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorUpdateWaybillFleet merchantShortId opCity apiTokenInfo vehicleCategory req

transitOperatorUpdateWaybillTablet :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillTabletReq -> Environment.FlowHandler SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorUpdateWaybillTablet merchantShortId opCity apiTokenInfo vehicleCategory req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorUpdateWaybillTablet merchantShortId opCity apiTokenInfo vehicleCategory req

transitOperatorGetWaybills :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [SharedLogic.External.Nandi.Types.NandiWaybillRow])
transitOperatorGetWaybills merchantShortId opCity apiTokenInfo limit offset vehicleCategory = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.TransitOperator.transitOperatorGetWaybills merchantShortId opCity apiTokenInfo limit offset vehicleCategory
