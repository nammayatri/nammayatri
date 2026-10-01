{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.RiderPlatform.AppManagement.TransitOperator
  ( transitOperatorGetRow,
    transitOperatorGetAllRows,
    transitOperatorDeleteRow,
    transitOperatorUpsertRow,
    transitOperatorUpsertRows,
    transitOperatorGetServiceTypes,
    transitOperatorGetRoutes,
    transitOperatorGetDepots,
    transitOperatorGetShiftTypes,
    transitOperatorGetScheduleNumbers,
    transitOperatorGetDayTypes,
    transitOperatorGetTripTypes,
    transitOperatorGetBreakTypes,
    transitOperatorGetTripDetails,
    transitOperatorGetFleets,
    transitOperatorGetConductor,
    transitOperatorGetDriver,
    transitOperatorGetDeviceIds,
    transitOperatorGetTabletIds,
    transitOperatorGetOperators,
    transitOperatorUpdateWaybillStatus,
    transitOperatorGetScheduleTripRepeat,
    transitOperatorSetScheduleTripRepeat,
    transitOperatorUpdateWaybillFleet,
    transitOperatorUpdateWaybillDetails,
    transitOperatorUpdateWaybillTablet,
    transitOperatorGetWaybills,
    transitOperatorQueryRows,
    transitOperatorGetDeviceVehicleMappingList,
    transitOperatorUpsertDeviceVehicleMapping,
    transitOperatorUnblockBus,
    transitOperatorSearchStops,
    transitOperatorNearbyStops,
    transitOperatorBulkReplaceStops,
    transitOperatorRouteStops,
    transitOperatorInsertRouteStop,
    transitOperatorReprocessRoutes,
    transitOperatorExportRouteStopMapping,
    transitOperatorUpsertVehicles,
    transitOperatorDeleteVehicle,
    transitOperatorQueryVehicle,
    transitOperatorV2ListTripGroups,
    transitOperatorV2UpsertTripGroup,
    transitOperatorV2GetTripGroup,
    transitOperatorV2DeleteTripGroup,
    transitOperatorV2ListTrips,
    transitOperatorV2UpsertTrips,
    transitOperatorV2DeleteTrip,
    transitOperatorV2ListDutyRepeats,
    transitOperatorV2UpsertDutyRepeat,
    transitOperatorV2DeleteDutyRepeat,
    transitOperatorV2PreviewDutyRepeats,
    transitOperatorV2GenerateDutyRepeats,
    transitOperatorV2ListDutyGroups,
    transitOperatorV2CreateDutyGroup,
    transitOperatorV2GetDutyGroup,
    transitOperatorV2ListDuties,
    transitOperatorV2GetDutyGroupLogs,
    transitOperatorV2UpdateDutyGroupVehicle,
    transitOperatorV2UpdateDutyGroupCrew,
    transitOperatorV2SetDutyGroupActive,
    transitOperatorV2DeleteDutyGroup,
    transitOperatorV2UpdateDutyCrew,
    transitOperatorV2DeleteDuty,
    transitOperatorV2ListGenerationFailures,
    transitOperatorV2ResolveGenerationFailure,
  )
where

import qualified API.Client.RiderPlatform.AppManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement.TransitOperator
import qualified "beckn-spec" BecknV2.OnDemand.Enums
import qualified Dashboard.Common
import qualified Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import "rider-app" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified "rider-app" SharedLogic.External.Nandi.TransitV2Types
import qualified "rider-app" SharedLogic.External.Nandi.Types
import qualified "lib-dashboard" SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Merchant

-- Strips the plaintext 'password' field before writing to the transaction log.
-- Hashing only happens in rider-app, so at this layer the request still contains plaintext.
stripPasswordForLog :: Data.Aeson.Value -> Data.Aeson.Value
stripPasswordForLog (Data.Aeson.Object obj) = Data.Aeson.Object (KeyMap.delete (Key.fromText "password") (fmap stripPasswordForLog obj))
stripPasswordForLog (Data.Aeson.Array arr) = Data.Aeson.Array (fmap stripPasswordForLog arr)
stripPasswordForLog v = v

transitOperatorGetRow :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.Types.NandiRow)
transitOperatorGetRow merchantShortId opCity apiTokenInfo column table vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorGetRow) column table vehicleCategory

transitOperatorGetAllRows :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.NandiRow])
transitOperatorGetAllRows merchantShortId opCity apiTokenInfo limit offset table vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorGetAllRows) limit offset table vehicleCategory

transitOperatorDeleteRow :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Data.Aeson.Value -> Environment.Flow SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorDeleteRow merchantShortId opCity apiTokenInfo table vehicleCategory req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorDeleteRow) table vehicleCategory req)

transitOperatorUpsertRow :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Data.Aeson.Value -> Environment.Flow SharedLogic.External.Nandi.Types.NandiRow)
transitOperatorUpsertRow merchantShortId opCity apiTokenInfo toRegen table vehicleCategory req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just (stripPasswordForLog req))
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorUpsertRow) toRegen table vehicleCategory req)

transitOperatorGetServiceTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.ServiceType])
transitOperatorGetServiceTypes merchantShortId opCity apiTokenInfo vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorGetServiceTypes) vehicleCategory

transitOperatorGetRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.NandiRoute])
transitOperatorGetRoutes merchantShortId opCity apiTokenInfo vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorGetRoutes) vehicleCategory

transitOperatorGetDepots :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.Depot])
transitOperatorGetDepots merchantShortId opCity apiTokenInfo vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorGetDepots) vehicleCategory

transitOperatorGetShiftTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.ShiftType])
transitOperatorGetShiftTypes merchantShortId opCity apiTokenInfo vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorGetShiftTypes) vehicleCategory

transitOperatorGetScheduleNumbers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.ScheduleNumber])
transitOperatorGetScheduleNumbers merchantShortId opCity apiTokenInfo vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorGetScheduleNumbers) vehicleCategory

transitOperatorGetDayTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.DayType])
transitOperatorGetDayTypes merchantShortId opCity apiTokenInfo vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorGetDayTypes) vehicleCategory

transitOperatorGetTripTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.TripType])
transitOperatorGetTripTypes merchantShortId opCity apiTokenInfo vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorGetTripTypes) vehicleCategory

transitOperatorGetBreakTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.BreakType])
transitOperatorGetBreakTypes merchantShortId opCity apiTokenInfo vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorGetBreakTypes) vehicleCategory

transitOperatorGetTripDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.NandiTripDetail])
transitOperatorGetTripDetails merchantShortId opCity apiTokenInfo scheduleNumber vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorGetTripDetails) scheduleNumber vehicleCategory

transitOperatorGetFleets :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.Fleet])
transitOperatorGetFleets merchantShortId opCity apiTokenInfo limit offset vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorGetFleets) limit offset vehicleCategory

transitOperatorGetConductor :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.Types.Employee)
transitOperatorGetConductor merchantShortId opCity apiTokenInfo token vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorGetConductor) token vehicleCategory

transitOperatorGetDriver :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.Types.Employee)
transitOperatorGetDriver merchantShortId opCity apiTokenInfo token vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorGetDriver) token vehicleCategory

transitOperatorGetDeviceIds :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [Kernel.Prelude.Text])
transitOperatorGetDeviceIds merchantShortId opCity apiTokenInfo vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorGetDeviceIds) vehicleCategory

transitOperatorGetTabletIds :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [Kernel.Prelude.Text])
transitOperatorGetTabletIds merchantShortId opCity apiTokenInfo vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorGetTabletIds) vehicleCategory

transitOperatorGetOperators :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> SharedLogic.External.Nandi.Types.OperatorRole -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.Employee])
transitOperatorGetOperators merchantShortId opCity apiTokenInfo role vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorGetOperators) role vehicleCategory

transitOperatorUpdateWaybillStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillStatusReq -> Environment.Flow SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorUpdateWaybillStatus merchantShortId opCity apiTokenInfo vehicleCategory req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorUpdateWaybillStatus) vehicleCategory req)

transitOperatorUpdateWaybillFleet :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillFleetReq -> Environment.Flow SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorUpdateWaybillFleet merchantShortId opCity apiTokenInfo vehicleCategory req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorUpdateWaybillFleet) vehicleCategory req)

transitOperatorUpdateWaybillTablet :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillTabletReq -> Environment.Flow SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorUpdateWaybillTablet merchantShortId opCity apiTokenInfo vehicleCategory req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorUpdateWaybillTablet) vehicleCategory req)

transitOperatorGetWaybills :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.NandiWaybillRow])
transitOperatorGetWaybills merchantShortId opCity apiTokenInfo limit offset vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorGetWaybills) limit offset vehicleCategory

transitOperatorQueryRows :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.QueryBody -> Environment.Flow [SharedLogic.External.Nandi.Types.NandiRow])
transitOperatorQueryRows merchantShortId opCity apiTokenInfo table vehicleCategory req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorQueryRows) table vehicleCategory req)

transitOperatorGetDeviceVehicleMappingList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Environment.Flow API.Types.Dashboard.AppManagement.TransitOperator.DeviceVehicleMappingListRes)
transitOperatorGetDeviceVehicleMappingList merchantShortId opCity apiTokenInfo = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorGetDeviceVehicleMappingList)

transitOperatorUpsertDeviceVehicleMapping :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.Dashboard.AppManagement.TransitOperator.UpsertDeviceVehicleMappingReq -> Environment.Flow API.Types.Dashboard.AppManagement.TransitOperator.UpsertDeviceVehicleMappingResp)
transitOperatorUpsertDeviceVehicleMapping merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (Dashboard.Common.addMultipartBoundary "XXX00XXX" . (.transitOperatorDSL.transitOperatorUpsertDeviceVehicleMapping)) req)

transitOperatorUpsertRows :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> [Data.Aeson.Value] -> Environment.Flow [SharedLogic.External.Nandi.Types.NandiRow])
transitOperatorUpsertRows merchantShortId opCity apiTokenInfo toRegen table vehicleCategory req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just (stripPasswordForLog (toJSON req)))
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorUpsertRows) toRegen table vehicleCategory req)

transitOperatorUnblockBus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
transitOperatorUnblockBus merchantShortId opCity apiTokenInfo vehicleNumber = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorUnblockBus) vehicleNumber)

transitOperatorSearchStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.EnrichedStop])
transitOperatorSearchStops merchantShortId opCity apiTokenInfo limit withRoutes q vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorSearchStops) limit withRoutes q vehicleCategory

transitOperatorNearbyStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Double) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Double -> Kernel.Prelude.Double -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.EnrichedStop])
transitOperatorNearbyStops merchantShortId opCity apiTokenInfo limit radius withRoutes lat lon vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorNearbyStops) limit radius withRoutes lat lon vehicleCategory

transitOperatorBulkReplaceStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.BulkReplaceReq -> Environment.Flow SharedLogic.External.Nandi.Types.BulkReplaceResult)
transitOperatorBulkReplaceStops merchantShortId opCity apiTokenInfo vehicleCategory req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorBulkReplaceStops) vehicleCategory req)

transitOperatorRouteStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.Types.RouteStopsResponse)
transitOperatorRouteStops merchantShortId opCity apiTokenInfo routeId vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorRouteStops) routeId vehicleCategory

transitOperatorInsertRouteStop :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.InsertRouteStopReq -> Environment.Flow SharedLogic.External.Nandi.Types.InsertRouteStopResp)
transitOperatorInsertRouteStop merchantShortId opCity apiTokenInfo routeId vehicleCategory req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorInsertRouteStop) routeId vehicleCategory req)

transitOperatorReprocessRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.ReprocessReq -> Environment.Flow [SharedLogic.External.Nandi.Types.ReprocessResult])
transitOperatorReprocessRoutes merchantShortId opCity apiTokenInfo vehicleCategory req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorReprocessRoutes) vehicleCategory req)

transitOperatorExportRouteStopMapping :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.RouteStopMappingExport])
transitOperatorExportRouteStopMapping merchantShortId opCity apiTokenInfo vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorExportRouteStopMapping) vehicleCategory

transitOperatorUpdateWaybillDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillDetailsReq -> Environment.Flow SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorUpdateWaybillDetails merchantShortId opCity apiTokenInfo vehicleCategory req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorUpdateWaybillDetails) vehicleCategory req)

transitOperatorUpsertVehicles :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> [SharedLogic.External.Nandi.Types.VehicleUpsertRequest] -> Environment.Flow [SharedLogic.External.Nandi.Types.Fleet])
transitOperatorUpsertVehicles merchantShortId opCity apiTokenInfo vehicleCategory req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorUpsertVehicles) vehicleCategory req)

transitOperatorDeleteVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> BecknV2.OnDemand.Enums.VehicleCategory -> Kernel.Prelude.Text -> Environment.Flow SharedLogic.External.Nandi.Types.RowsAffectedResp)
transitOperatorDeleteVehicle merchantShortId opCity apiTokenInfo vehicleCategory vehicleId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  -- soft-delete: capture the target in the audit log since query params aren't always retained by access logs.
  let auditPayload = Data.Aeson.object [(Key.fromText "vehicleId", toJSON vehicleId), (Key.fromText "vehicleCategory", toJSON vehicleCategory)]
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just auditPayload)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorDeleteVehicle) vehicleCategory vehicleId)

transitOperatorQueryVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.Types.Fleet])
transitOperatorQueryVehicle merchantShortId opCity apiTokenInfo fleetNo tagNumber vehicleNo vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorQueryVehicle) fleetNo tagNumber vehicleNo vehicleCategory

transitOperatorGetScheduleTripRepeat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.Types.ScheduleTripRepeatConfig)
transitOperatorGetScheduleTripRepeat merchantShortId opCity apiTokenInfo scheduleTripId vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorGetScheduleTripRepeat) scheduleTripId vehicleCategory

transitOperatorSetScheduleTripRepeat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.SetScheduleTripRepeatReq -> Environment.Flow SharedLogic.External.Nandi.Types.ScheduleTripRepeatConfig)
transitOperatorSetScheduleTripRepeat merchantShortId opCity apiTokenInfo scheduleTripId vehicleCategory req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorSetScheduleTripRepeat) scheduleTripId vehicleCategory req)

transitOperatorV2ListTripGroups :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2TripGroupPage)
transitOperatorV2ListTripGroups merchantShortId opCity apiTokenInfo code conductorTokenNumber depotId driverTokenNumber limit offset operatorId search shift tripType vehicleNumber zone vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Prelude.Just apiTokenInfo.personId.getId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorV2ListTripGroups) code conductorTokenNumber depotId driverTokenNumber limit offset operatorId search shift tripType vehicleNumber zone requestorId vehicleCategory

transitOperatorV2UpsertTripGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpsertTripGroupReq -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2TripGroup)
transitOperatorV2UpsertTripGroup merchantShortId opCity apiTokenInfo operatorId vehicleCategory req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Prelude.Just apiTokenInfo.personId.getId
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorV2UpsertTripGroup) operatorId requestorId vehicleCategory req)

transitOperatorV2GetTripGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2TripGroup)
transitOperatorV2GetTripGroup merchantShortId opCity apiTokenInfo tripGroupId operatorId vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Prelude.Just apiTokenInfo.personId.getId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorV2GetTripGroup) tripGroupId operatorId requestorId vehicleCategory

transitOperatorV2DeleteTripGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp)
transitOperatorV2DeleteTripGroup merchantShortId opCity apiTokenInfo tripGroupId operatorId vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Prelude.Just apiTokenInfo.personId.getId
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorV2DeleteTripGroup) tripGroupId operatorId requestorId vehicleCategory)

transitOperatorV2ListTrips :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.TransitV2Types.V2Trip])
transitOperatorV2ListTrips merchantShortId opCity apiTokenInfo tripGroupId operatorId vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Prelude.Just apiTokenInfo.personId.getId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorV2ListTrips) tripGroupId operatorId requestorId vehicleCategory

transitOperatorV2UpsertTrips :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpsertTripsReq -> Environment.Flow [SharedLogic.External.Nandi.TransitV2Types.V2Trip])
transitOperatorV2UpsertTrips merchantShortId opCity apiTokenInfo tripGroupId operatorId vehicleCategory req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Prelude.Just apiTokenInfo.personId.getId
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorV2UpsertTrips) tripGroupId operatorId requestorId vehicleCategory req)

transitOperatorV2DeleteTrip :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp)
transitOperatorV2DeleteTrip merchantShortId opCity apiTokenInfo tripId operatorId vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Prelude.Just apiTokenInfo.personId.getId
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorV2DeleteTrip) tripId operatorId requestorId vehicleCategory)

transitOperatorV2ListDutyRepeats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2DutyRepeatPage)
transitOperatorV2ListDutyRepeats merchantShortId opCity apiTokenInfo code conductorTokenNumber driverTokenNumber limit offset operatorId repeatStatus search tripGroupId vehicleNumber vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Prelude.Just apiTokenInfo.personId.getId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorV2ListDutyRepeats) code conductorTokenNumber driverTokenNumber limit offset operatorId repeatStatus search tripGroupId vehicleNumber requestorId vehicleCategory

transitOperatorV2UpsertDutyRepeat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpsertDutyRepeatReq -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2UpsertDutyRepeatResp)
transitOperatorV2UpsertDutyRepeat merchantShortId opCity apiTokenInfo operatorId vehicleCategory req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Prelude.Just apiTokenInfo.personId.getId
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorV2UpsertDutyRepeat) operatorId requestorId vehicleCategory req)

transitOperatorV2DeleteDutyRepeat :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp)
transitOperatorV2DeleteDutyRepeat merchantShortId opCity apiTokenInfo dutyRepeatId operatorId vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Prelude.Just apiTokenInfo.personId.getId
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorV2DeleteDutyRepeat) dutyRepeatId operatorId requestorId vehicleCategory)

transitOperatorV2PreviewDutyRepeats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2GenerateReq -> Environment.Flow [SharedLogic.External.Nandi.TransitV2Types.V2GenerateEntry])
transitOperatorV2PreviewDutyRepeats merchantShortId opCity apiTokenInfo operatorId vehicleCategory req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Prelude.Just apiTokenInfo.personId.getId
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorV2PreviewDutyRepeats) operatorId requestorId vehicleCategory req)

transitOperatorV2GenerateDutyRepeats :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2GenerateReq -> Environment.Flow [SharedLogic.External.Nandi.TransitV2Types.V2GenerateEntry])
transitOperatorV2GenerateDutyRepeats merchantShortId opCity apiTokenInfo operatorId vehicleCategory req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Prelude.Just apiTokenInfo.personId.getId
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorV2GenerateDutyRepeats) operatorId requestorId vehicleCategory req)

transitOperatorV2ListDutyGroups :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupPage)
transitOperatorV2ListDutyGroups merchantShortId opCity apiTokenInfo code conductorTokenNumber depotId driverTokenNumber isActive limit offset operationDate operatorId search tripGroupId vehicleNumber vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Prelude.Just apiTokenInfo.personId.getId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorV2ListDutyGroups) code conductorTokenNumber depotId driverTokenNumber isActive limit offset operationDate operatorId search tripGroupId vehicleNumber requestorId vehicleCategory

transitOperatorV2CreateDutyGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2CreateDutyGroupReq -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupDetail)
transitOperatorV2CreateDutyGroup merchantShortId opCity apiTokenInfo operatorId vehicleCategory req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Prelude.Just apiTokenInfo.personId.getId
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorV2CreateDutyGroup) operatorId requestorId vehicleCategory req)

transitOperatorV2GetDutyGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupDetail)
transitOperatorV2GetDutyGroup merchantShortId opCity apiTokenInfo dutyGroupId operatorId vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Prelude.Just apiTokenInfo.personId.getId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorV2GetDutyGroup) dutyGroupId operatorId requestorId vehicleCategory

transitOperatorV2ListDuties :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.TransitV2Types.V2Duty])
transitOperatorV2ListDuties merchantShortId opCity apiTokenInfo dutyGroupId operatorId vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Prelude.Just apiTokenInfo.personId.getId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorV2ListDuties) dutyGroupId operatorId requestorId vehicleCategory

transitOperatorV2GetDutyGroupLogs :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [SharedLogic.External.Nandi.TransitV2Types.V2DutyEventLog])
transitOperatorV2GetDutyGroupLogs merchantShortId opCity apiTokenInfo dutyGroupId operatorId vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Prelude.Just apiTokenInfo.personId.getId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorV2GetDutyGroupLogs) dutyGroupId operatorId requestorId vehicleCategory

transitOperatorV2UpdateDutyGroupVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpdateVehicleReq -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2DutyGroup)
transitOperatorV2UpdateDutyGroupVehicle merchantShortId opCity apiTokenInfo dutyGroupId operatorId vehicleCategory req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Prelude.Just apiTokenInfo.personId.getId
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorV2UpdateDutyGroupVehicle) dutyGroupId operatorId requestorId vehicleCategory req)

transitOperatorV2UpdateDutyGroupCrew :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpdateCrewReq -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupDetail)
transitOperatorV2UpdateDutyGroupCrew merchantShortId opCity apiTokenInfo dutyGroupId operatorId vehicleCategory req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Prelude.Just apiTokenInfo.personId.getId
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorV2UpdateDutyGroupCrew) dutyGroupId operatorId requestorId vehicleCategory req)

transitOperatorV2SetDutyGroupActive :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2SetActiveReq -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2DutyGroup)
transitOperatorV2SetDutyGroupActive merchantShortId opCity apiTokenInfo dutyGroupId operatorId vehicleCategory req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Prelude.Just apiTokenInfo.personId.getId
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorV2SetDutyGroupActive) dutyGroupId operatorId requestorId vehicleCategory req)

transitOperatorV2DeleteDutyGroup :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp)
transitOperatorV2DeleteDutyGroup merchantShortId opCity apiTokenInfo dutyGroupId operatorId vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Prelude.Just apiTokenInfo.personId.getId
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorV2DeleteDutyGroup) dutyGroupId operatorId requestorId vehicleCategory)

transitOperatorV2UpdateDutyCrew :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpdateCrewReq -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2Duty)
transitOperatorV2UpdateDutyCrew merchantShortId opCity apiTokenInfo dutyId operatorId vehicleCategory req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Prelude.Just apiTokenInfo.personId.getId
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorV2UpdateDutyCrew) dutyId operatorId requestorId vehicleCategory req)

transitOperatorV2DeleteDuty :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp)
transitOperatorV2DeleteDuty merchantShortId opCity apiTokenInfo dutyId operatorId vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Prelude.Just apiTokenInfo.personId.getId
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorV2DeleteDuty) dutyId operatorId requestorId vehicleCategory)

transitOperatorV2ListGenerationFailures :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2DutyEventLogPage)
transitOperatorV2ListGenerationFailures merchantShortId opCity apiTokenInfo limit offset operatorId resolved vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Prelude.Just apiTokenInfo.personId.getId
  API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorV2ListGenerationFailures) limit offset operatorId resolved requestorId vehicleCategory

transitOperatorV2ResolveGenerationFailure :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow SharedLogic.External.Nandi.TransitV2Types.V2DutyEventLog)
transitOperatorV2ResolveGenerationFailure merchantShortId opCity apiTokenInfo failureId operatorId vehicleCategory = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = Kernel.Prelude.Just apiTokenInfo.personId.getId
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.ActionAPI apiTokenInfo.userActionType) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.RiderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.transitOperatorDSL.transitOperatorV2ResolveGenerationFailure) failureId operatorId requestorId vehicleCategory)
