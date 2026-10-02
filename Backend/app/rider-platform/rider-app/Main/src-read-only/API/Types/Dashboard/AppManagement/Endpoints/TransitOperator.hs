{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.TransitOperator where

import qualified "beckn-spec" BecknV2.OnDemand.Enums
import qualified Data.Aeson
import qualified Data.ByteString.Lazy
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Prelude
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import Servant
import Servant.Client
import qualified "this" SharedLogic.External.Nandi.TransitV2Types
import qualified "this" SharedLogic.External.Nandi.Types

data DeviceVehicleMappingItem = DeviceVehicleMappingItem {createdAt :: Kernel.Prelude.UTCTime, deviceId :: Kernel.Prelude.Text, gtfsId :: Kernel.Prelude.Text, updatedAt :: Kernel.Prelude.UTCTime, vehicleNo :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DeviceVehicleMappingListRes = DeviceVehicleMappingListRes {mappings :: [DeviceVehicleMappingItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype UpsertDeviceVehicleMappingReq = UpsertDeviceVehicleMappingReq {file :: EulerHS.Prelude.FilePath}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpsertDeviceVehicleMappingReq where
  hideSecrets = Kernel.Prelude.identity

data UpsertDeviceVehicleMappingResp = UpsertDeviceVehicleMappingResp {success :: Kernel.Prelude.Text, unprocessedEntries :: [Kernel.Prelude.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("transitOperator" :> (TransitOperatorGetRow :<|> TransitOperatorGetAllRows :<|> TransitOperatorDeleteRow :<|> TransitOperatorUpsertRow :<|> TransitOperatorUpsertRows :<|> TransitOperatorQueryRows :<|> TransitOperatorGetServiceTypes :<|> TransitOperatorGetRoutes :<|> TransitOperatorGetDepots :<|> TransitOperatorGetShiftTypes :<|> TransitOperatorGetScheduleNumbers :<|> TransitOperatorGetDayTypes :<|> TransitOperatorGetTripTypes :<|> TransitOperatorGetBreakTypes :<|> TransitOperatorGetTripDetails :<|> TransitOperatorGetFleets :<|> TransitOperatorGetConductor :<|> TransitOperatorGetDriver :<|> TransitOperatorGetDeviceIds :<|> TransitOperatorGetTabletIds :<|> TransitOperatorGetOperators :<|> TransitOperatorUpdateWaybillStatus :<|> TransitOperatorUpdateWaybillFleet :<|> TransitOperatorUpdateWaybillDetails :<|> TransitOperatorUpdateWaybillTablet :<|> TransitOperatorGetWaybills :<|> TransitOperatorGetDeviceVehicleMappingList :<|> TransitOperatorUpsertDeviceVehicleMapping :<|> TransitOperatorUnblockBus :<|> TransitOperatorSearchStops :<|> TransitOperatorNearbyStops :<|> TransitOperatorBulkReplaceStops :<|> TransitOperatorRouteStops :<|> TransitOperatorInsertRouteStop :<|> TransitOperatorReprocessRoutes :<|> TransitOperatorExportRouteStopMapping :<|> TransitOperatorQueryVehicle :<|> TransitOperatorUpsertVehicles :<|> TransitOperatorDeleteVehicle :<|> TransitOperatorGetScheduleTripRepeat :<|> TransitOperatorSetScheduleTripRepeat :<|> TransitOperatorV2ListTripGroupsHelper :<|> TransitOperatorV2UpsertTripGroupHelper :<|> TransitOperatorV2GetTripGroupHelper :<|> TransitOperatorV2DeleteTripGroupHelper :<|> TransitOperatorV2ListTripsHelper :<|> TransitOperatorV2UpsertTripsHelper :<|> TransitOperatorV2DeleteTripHelper :<|> TransitOperatorV2ListDutyRepeatsHelper :<|> TransitOperatorV2UpsertDutyRepeatHelper :<|> TransitOperatorV2DeleteDutyRepeatHelper :<|> TransitOperatorV2PreviewDutyRepeatsHelper :<|> TransitOperatorV2GenerateDutyRepeatsHelper :<|> TransitOperatorV2ListDutyGroupsHelper :<|> TransitOperatorV2CreateDutyGroupHelper :<|> TransitOperatorV2GetDutyGroupHelper :<|> TransitOperatorV2ListDutiesHelper :<|> TransitOperatorV2GetDutyGroupLogsHelper :<|> TransitOperatorV2UpdateDutyGroupVehicleHelper :<|> TransitOperatorV2UpdateDutyGroupCrewHelper :<|> TransitOperatorV2SetDutyGroupActiveHelper :<|> TransitOperatorV2DeleteDutyGroupHelper :<|> TransitOperatorV2UpdateDutyCrewHelper :<|> TransitOperatorV2DeleteDutyHelper :<|> TransitOperatorV2ListGenerationFailuresHelper :<|> TransitOperatorV2ResolveGenerationFailureHelper))

type TransitOperatorGetRow =
  ( "row" :> QueryParam "column" Kernel.Prelude.Text :> MandatoryQueryParam "table" SharedLogic.External.Nandi.Types.NandiTable
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) SharedLogic.External.Nandi.Types.NandiRow
  )

type TransitOperatorGetAllRows =
  ( "allRows" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> MandatoryQueryParam
           "table"
           SharedLogic.External.Nandi.Types.NandiTable
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           [SharedLogic.External.Nandi.Types.NandiRow]
  )

type TransitOperatorDeleteRow =
  ( "row" :> MandatoryQueryParam "table" SharedLogic.External.Nandi.Types.NandiTable
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Data.Aeson.Value
      :> Delete ('[JSON]) SharedLogic.External.Nandi.Types.RowsAffectedResp
  )

type TransitOperatorUpsertRow =
  ( "row" :> QueryParam "toRegen" Kernel.Prelude.Text :> MandatoryQueryParam "table" SharedLogic.External.Nandi.Types.NandiTable
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Data.Aeson.Value
      :> Post
           ('[JSON])
           SharedLogic.External.Nandi.Types.NandiRow
  )

type TransitOperatorUpsertRows =
  ( "rows" :> QueryParam "toRegen" Kernel.Prelude.Text :> MandatoryQueryParam "table" SharedLogic.External.Nandi.Types.NandiTable
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) [Data.Aeson.Value]
      :> Post
           ('[JSON])
           [SharedLogic.External.Nandi.Types.NandiRow]
  )

type TransitOperatorQueryRows =
  ( "queryRow" :> MandatoryQueryParam "table" SharedLogic.External.Nandi.Types.NandiTable
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) SharedLogic.External.Nandi.Types.QueryBody
      :> Post
           ('[JSON])
           [SharedLogic.External.Nandi.Types.NandiRow]
  )

type TransitOperatorGetServiceTypes = ("serviceTypes" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [SharedLogic.External.Nandi.Types.ServiceType])

type TransitOperatorGetRoutes = ("routes" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [SharedLogic.External.Nandi.Types.NandiRoute])

type TransitOperatorGetDepots = ("depots" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [SharedLogic.External.Nandi.Types.Depot])

type TransitOperatorGetShiftTypes = ("shiftTypes" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [SharedLogic.External.Nandi.Types.ShiftType])

type TransitOperatorGetScheduleNumbers =
  ( "scheduleNumbers" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           [SharedLogic.External.Nandi.Types.ScheduleNumber]
  )

type TransitOperatorGetDayTypes = ("dayTypes" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [SharedLogic.External.Nandi.Types.DayType])

type TransitOperatorGetTripTypes = ("tripTypes" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [SharedLogic.External.Nandi.Types.TripType])

type TransitOperatorGetBreakTypes = ("breakTypes" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [SharedLogic.External.Nandi.Types.BreakType])

type TransitOperatorGetTripDetails =
  ( "tripDetails" :> MandatoryQueryParam "scheduleNumber" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) [SharedLogic.External.Nandi.Types.NandiTripDetail]
  )

type TransitOperatorGetFleets =
  ( "fleets" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) [SharedLogic.External.Nandi.Types.Fleet]
  )

type TransitOperatorGetConductor =
  ( "conductor" :> MandatoryQueryParam "token" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           SharedLogic.External.Nandi.Types.Employee
  )

type TransitOperatorGetDriver =
  ( "driver" :> MandatoryQueryParam "token" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           SharedLogic.External.Nandi.Types.Employee
  )

type TransitOperatorGetDeviceIds = ("deviceIds" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [Kernel.Prelude.Text])

type TransitOperatorGetTabletIds = ("tabletIds" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [Kernel.Prelude.Text])

type TransitOperatorGetOperators =
  ( "operators" :> MandatoryQueryParam "role" SharedLogic.External.Nandi.Types.OperatorRole
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) [SharedLogic.External.Nandi.Types.Employee]
  )

type TransitOperatorUpdateWaybillStatus =
  ( "waybillStatus" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           SharedLogic.External.Nandi.Types.UpdateWaybillStatusReq
      :> Post ('[JSON]) SharedLogic.External.Nandi.Types.RowsAffectedResp
  )

type TransitOperatorUpdateWaybillFleet =
  ( "waybillFleet" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           SharedLogic.External.Nandi.Types.UpdateWaybillFleetReq
      :> Post ('[JSON]) SharedLogic.External.Nandi.Types.RowsAffectedResp
  )

type TransitOperatorUpdateWaybillDetails =
  ( "waybillDetails" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           SharedLogic.External.Nandi.Types.UpdateWaybillDetailsReq
      :> Post ('[JSON]) SharedLogic.External.Nandi.Types.RowsAffectedResp
  )

type TransitOperatorUpdateWaybillTablet =
  ( "waybillTablet" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           SharedLogic.External.Nandi.Types.UpdateWaybillTabletReq
      :> Post ('[JSON]) SharedLogic.External.Nandi.Types.RowsAffectedResp
  )

type TransitOperatorGetWaybills =
  ( "waybills" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) [SharedLogic.External.Nandi.Types.NandiWaybillRow]
  )

type TransitOperatorGetDeviceVehicleMappingList = ("deviceVehicleMapping" :> "list" :> Get ('[JSON]) DeviceVehicleMappingListRes)

type TransitOperatorUpsertDeviceVehicleMapping =
  ( "deviceVehicleMapping" :> "upsert"
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           UpsertDeviceVehicleMappingReq
      :> Post ('[JSON]) UpsertDeviceVehicleMappingResp
  )

type TransitOperatorUnblockBus = ("bus" :> Capture "vehicleNumber" Kernel.Prelude.Text :> "unblock" :> Post ('[JSON]) Kernel.Types.APISuccess.APISuccess)

type TransitOperatorSearchStops =
  ( "stops" :> "search" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "withRoutes" Kernel.Prelude.Bool
      :> MandatoryQueryParam
           "q"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           [SharedLogic.External.Nandi.Types.EnrichedStop]
  )

type TransitOperatorNearbyStops =
  ( "stops" :> "nearby" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "radius" Kernel.Prelude.Double
      :> QueryParam
           "withRoutes"
           Kernel.Prelude.Bool
      :> MandatoryQueryParam "lat" Kernel.Prelude.Double
      :> MandatoryQueryParam
           "lon"
           Kernel.Prelude.Double
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           [SharedLogic.External.Nandi.Types.EnrichedStop]
  )

type TransitOperatorBulkReplaceStops =
  ( "stops" :> "bulkReplace" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           SharedLogic.External.Nandi.Types.BulkReplaceReq
      :> Post ('[JSON]) SharedLogic.External.Nandi.Types.BulkReplaceResult
  )

type TransitOperatorRouteStops =
  ( "routeStops" :> MandatoryQueryParam "routeId" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           SharedLogic.External.Nandi.Types.RouteStopsResponse
  )

type TransitOperatorInsertRouteStop =
  ( "routeStops" :> "insert" :> MandatoryQueryParam "routeId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) SharedLogic.External.Nandi.Types.InsertRouteStopReq
      :> Post
           ('[JSON])
           SharedLogic.External.Nandi.Types.InsertRouteStopResp
  )

type TransitOperatorReprocessRoutes =
  ( "routes" :> "reprocess" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           SharedLogic.External.Nandi.Types.ReprocessReq
      :> Post ('[JSON]) [SharedLogic.External.Nandi.Types.ReprocessResult]
  )

type TransitOperatorExportRouteStopMapping =
  ( "routeStopMapping" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           [SharedLogic.External.Nandi.Types.RouteStopMappingExport]
  )

type TransitOperatorQueryVehicle =
  ( "queryVehicle" :> QueryParam "fleetNo" Kernel.Prelude.Text :> QueryParam "tagNumber" Kernel.Prelude.Text
      :> QueryParam
           "vehicleNo"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           [SharedLogic.External.Nandi.Types.Fleet]
  )

type TransitOperatorUpsertVehicles =
  ( "upsertVehicles" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           [SharedLogic.External.Nandi.Types.VehicleUpsertRequest]
      :> Post ('[JSON]) [SharedLogic.External.Nandi.Types.Fleet]
  )

type TransitOperatorDeleteVehicle =
  ( "deleteVehicle" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> MandatoryQueryParam
           "vehicleId"
           Kernel.Prelude.Text
      :> Delete ('[JSON]) SharedLogic.External.Nandi.Types.RowsAffectedResp
  )

type TransitOperatorGetScheduleTripRepeat =
  ( "scheduleTrip" :> Capture "scheduleTripId" Kernel.Prelude.Text :> "repeat"
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) SharedLogic.External.Nandi.Types.ScheduleTripRepeatConfig
  )

type TransitOperatorSetScheduleTripRepeat =
  ( "scheduleTrip" :> Capture "scheduleTripId" Kernel.Prelude.Text :> "repeat"
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) SharedLogic.External.Nandi.Types.SetScheduleTripRepeatReq
      :> Post
           ('[JSON])
           SharedLogic.External.Nandi.Types.ScheduleTripRepeatConfig
  )

type TransitOperatorV2ListTripGroups =
  ( "v2" :> "tripGroups" :> QueryParam "code" Kernel.Prelude.Text :> QueryParam "conductorTokenNumber" Kernel.Prelude.Text
      :> QueryParam
           "depotId"
           Kernel.Prelude.Text
      :> QueryParam "driverTokenNumber" Kernel.Prelude.Text
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "operatorId"
           Kernel.Prelude.Text
      :> QueryParam
           "search"
           Kernel.Prelude.Text
      :> QueryParam
           "shift"
           Kernel.Prelude.Text
      :> QueryParam
           "tripType"
           Kernel.Prelude.Text
      :> QueryParam
           "vehicleNumber"
           Kernel.Prelude.Text
      :> QueryParam
           "zone"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2TripGroupPage
  )

type TransitOperatorV2ListTripGroupsHelper =
  ( "v2" :> "tripGroups" :> QueryParam "code" Kernel.Prelude.Text :> QueryParam "conductorTokenNumber" Kernel.Prelude.Text
      :> QueryParam
           "depotId"
           Kernel.Prelude.Text
      :> QueryParam "driverTokenNumber" Kernel.Prelude.Text
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "operatorId"
           Kernel.Prelude.Text
      :> QueryParam
           "search"
           Kernel.Prelude.Text
      :> QueryParam
           "shift"
           Kernel.Prelude.Text
      :> QueryParam
           "tripType"
           Kernel.Prelude.Text
      :> QueryParam
           "vehicleNumber"
           Kernel.Prelude.Text
      :> QueryParam
           "zone"
           Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2TripGroupPage
  )

type TransitOperatorV2UpsertTripGroup =
  ( "v2" :> "tripGroups" :> "upsert" :> QueryParam "operatorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) SharedLogic.External.Nandi.TransitV2Types.V2UpsertTripGroupReq
      :> Post
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2TripGroup
  )

type TransitOperatorV2UpsertTripGroupHelper =
  ( "v2" :> "tripGroups" :> "upsert" :> QueryParam "operatorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2UpsertTripGroupReq
      :> Post
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2TripGroup
  )

type TransitOperatorV2GetTripGroup =
  ( "v2" :> "tripGroups" :> Capture "tripGroupId" Kernel.Prelude.Text :> QueryParam "operatorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) SharedLogic.External.Nandi.TransitV2Types.V2TripGroup
  )

type TransitOperatorV2GetTripGroupHelper =
  ( "v2" :> "tripGroups" :> Capture "tripGroupId" Kernel.Prelude.Text :> QueryParam "operatorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2TripGroup
  )

type TransitOperatorV2DeleteTripGroup =
  ( "v2" :> "tripGroups" :> Capture "tripGroupId" Kernel.Prelude.Text :> "delete"
      :> QueryParam
           "operatorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Post
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp
  )

type TransitOperatorV2DeleteTripGroupHelper =
  ( "v2" :> "tripGroups" :> Capture "tripGroupId" Kernel.Prelude.Text :> "delete" :> QueryParam "operatorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Post
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp
  )

type TransitOperatorV2ListTrips =
  ( "v2" :> "tripGroups" :> Capture "tripGroupId" Kernel.Prelude.Text :> "trips" :> QueryParam "operatorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) [SharedLogic.External.Nandi.TransitV2Types.V2Trip]
  )

type TransitOperatorV2ListTripsHelper =
  ( "v2" :> "tripGroups" :> Capture "tripGroupId" Kernel.Prelude.Text :> "trips" :> QueryParam "operatorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           [SharedLogic.External.Nandi.TransitV2Types.V2Trip]
  )

type TransitOperatorV2UpsertTrips =
  ( "v2" :> "tripGroups" :> Capture "tripGroupId" Kernel.Prelude.Text :> "trips" :> "upsert"
      :> QueryParam
           "operatorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2UpsertTripsReq
      :> Post
           ('[JSON])
           [SharedLogic.External.Nandi.TransitV2Types.V2Trip]
  )

type TransitOperatorV2UpsertTripsHelper =
  ( "v2" :> "tripGroups" :> Capture "tripGroupId" Kernel.Prelude.Text :> "trips" :> "upsert"
      :> QueryParam
           "operatorId"
           Kernel.Prelude.Text
      :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2UpsertTripsReq
      :> Post
           ('[JSON])
           [SharedLogic.External.Nandi.TransitV2Types.V2Trip]
  )

type TransitOperatorV2DeleteTrip =
  ( "v2" :> "trips" :> Capture "tripId" Kernel.Prelude.Text :> "delete" :> QueryParam "operatorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Post ('[JSON]) SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp
  )

type TransitOperatorV2DeleteTripHelper =
  ( "v2" :> "trips" :> Capture "tripId" Kernel.Prelude.Text :> "delete" :> QueryParam "operatorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Post
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp
  )

type TransitOperatorV2ListDutyRepeats =
  ( "v2" :> "dutyRepeats" :> QueryParam "code" Kernel.Prelude.Text :> QueryParam "conductorTokenNumber" Kernel.Prelude.Text
      :> QueryParam
           "driverTokenNumber"
           Kernel.Prelude.Text
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "operatorId"
           Kernel.Prelude.Text
      :> QueryParam
           "repeatStatus"
           Kernel.Prelude.Text
      :> QueryParam
           "search"
           Kernel.Prelude.Text
      :> QueryParam
           "tripGroupId"
           Kernel.Prelude.Text
      :> QueryParam
           "vehicleNumber"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2DutyRepeatPage
  )

type TransitOperatorV2ListDutyRepeatsHelper =
  ( "v2" :> "dutyRepeats" :> QueryParam "code" Kernel.Prelude.Text :> QueryParam "conductorTokenNumber" Kernel.Prelude.Text
      :> QueryParam
           "driverTokenNumber"
           Kernel.Prelude.Text
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "operatorId"
           Kernel.Prelude.Text
      :> QueryParam
           "repeatStatus"
           Kernel.Prelude.Text
      :> QueryParam
           "search"
           Kernel.Prelude.Text
      :> QueryParam
           "tripGroupId"
           Kernel.Prelude.Text
      :> QueryParam
           "vehicleNumber"
           Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2DutyRepeatPage
  )

type TransitOperatorV2UpsertDutyRepeat =
  ( "v2" :> "dutyRepeats" :> "upsert" :> QueryParam "operatorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) SharedLogic.External.Nandi.TransitV2Types.V2UpsertDutyRepeatReq
      :> Post
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2UpsertDutyRepeatResp
  )

type TransitOperatorV2UpsertDutyRepeatHelper =
  ( "v2" :> "dutyRepeats" :> "upsert" :> QueryParam "operatorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2UpsertDutyRepeatReq
      :> Post
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2UpsertDutyRepeatResp
  )

type TransitOperatorV2DeleteDutyRepeat =
  ( "v2" :> "dutyRepeats" :> Capture "dutyRepeatId" Kernel.Prelude.Text :> "delete"
      :> QueryParam
           "operatorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Post
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp
  )

type TransitOperatorV2DeleteDutyRepeatHelper =
  ( "v2" :> "dutyRepeats" :> Capture "dutyRepeatId" Kernel.Prelude.Text :> "delete"
      :> QueryParam
           "operatorId"
           Kernel.Prelude.Text
      :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Post
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp
  )

type TransitOperatorV2PreviewDutyRepeats =
  ( "v2" :> "dutyRepeats" :> "preview" :> QueryParam "operatorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) SharedLogic.External.Nandi.TransitV2Types.V2GenerateReq
      :> Post
           ('[JSON])
           [SharedLogic.External.Nandi.TransitV2Types.V2GenerateEntry]
  )

type TransitOperatorV2PreviewDutyRepeatsHelper =
  ( "v2" :> "dutyRepeats" :> "preview" :> QueryParam "operatorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2GenerateReq
      :> Post
           ('[JSON])
           [SharedLogic.External.Nandi.TransitV2Types.V2GenerateEntry]
  )

type TransitOperatorV2GenerateDutyRepeats =
  ( "v2" :> "dutyRepeats" :> "generate" :> QueryParam "operatorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) SharedLogic.External.Nandi.TransitV2Types.V2GenerateReq
      :> Post
           ('[JSON])
           [SharedLogic.External.Nandi.TransitV2Types.V2GenerateEntry]
  )

type TransitOperatorV2GenerateDutyRepeatsHelper =
  ( "v2" :> "dutyRepeats" :> "generate" :> QueryParam "operatorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2GenerateReq
      :> Post
           ('[JSON])
           [SharedLogic.External.Nandi.TransitV2Types.V2GenerateEntry]
  )

type TransitOperatorV2ListDutyGroups =
  ( "v2" :> "dutyGroups" :> QueryParam "code" Kernel.Prelude.Text :> QueryParam "conductorTokenNumber" Kernel.Prelude.Text
      :> QueryParam
           "depotId"
           Kernel.Prelude.Text
      :> QueryParam "driverTokenNumber" Kernel.Prelude.Text
      :> QueryParam
           "isActive"
           Kernel.Prelude.Bool
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "operationDate"
           Kernel.Prelude.Text
      :> QueryParam
           "operatorId"
           Kernel.Prelude.Text
      :> QueryParam
           "search"
           Kernel.Prelude.Text
      :> QueryParam
           "tripGroupId"
           Kernel.Prelude.Text
      :> QueryParam
           "vehicleNumber"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupPage
  )

type TransitOperatorV2ListDutyGroupsHelper =
  ( "v2" :> "dutyGroups" :> QueryParam "code" Kernel.Prelude.Text :> QueryParam "conductorTokenNumber" Kernel.Prelude.Text
      :> QueryParam
           "depotId"
           Kernel.Prelude.Text
      :> QueryParam "driverTokenNumber" Kernel.Prelude.Text
      :> QueryParam
           "isActive"
           Kernel.Prelude.Bool
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "operationDate"
           Kernel.Prelude.Text
      :> QueryParam
           "operatorId"
           Kernel.Prelude.Text
      :> QueryParam
           "search"
           Kernel.Prelude.Text
      :> QueryParam
           "tripGroupId"
           Kernel.Prelude.Text
      :> QueryParam
           "vehicleNumber"
           Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupPage
  )

type TransitOperatorV2CreateDutyGroup =
  ( "v2" :> "dutyGroups" :> "create" :> QueryParam "operatorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) SharedLogic.External.Nandi.TransitV2Types.V2CreateDutyGroupReq
      :> Post
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupDetail
  )

type TransitOperatorV2CreateDutyGroupHelper =
  ( "v2" :> "dutyGroups" :> "create" :> QueryParam "operatorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2CreateDutyGroupReq
      :> Post
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupDetail
  )

type TransitOperatorV2GetDutyGroup =
  ( "v2" :> "dutyGroups" :> Capture "dutyGroupId" Kernel.Prelude.Text :> QueryParam "operatorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupDetail
  )

type TransitOperatorV2GetDutyGroupHelper =
  ( "v2" :> "dutyGroups" :> Capture "dutyGroupId" Kernel.Prelude.Text :> QueryParam "operatorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupDetail
  )

type TransitOperatorV2ListDuties =
  ( "v2" :> "dutyGroups" :> Capture "dutyGroupId" Kernel.Prelude.Text :> "duties"
      :> QueryParam
           "operatorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) [SharedLogic.External.Nandi.TransitV2Types.V2Duty]
  )

type TransitOperatorV2ListDutiesHelper =
  ( "v2" :> "dutyGroups" :> Capture "dutyGroupId" Kernel.Prelude.Text :> "duties" :> QueryParam "operatorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           [SharedLogic.External.Nandi.TransitV2Types.V2Duty]
  )

type TransitOperatorV2GetDutyGroupLogs =
  ( "v2" :> "dutyGroups" :> Capture "dutyGroupId" Kernel.Prelude.Text :> "logs"
      :> QueryParam
           "operatorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           [SharedLogic.External.Nandi.TransitV2Types.V2DutyEventLog]
  )

type TransitOperatorV2GetDutyGroupLogsHelper =
  ( "v2" :> "dutyGroups" :> Capture "dutyGroupId" Kernel.Prelude.Text :> "logs" :> QueryParam "operatorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           [SharedLogic.External.Nandi.TransitV2Types.V2DutyEventLog]
  )

type TransitOperatorV2UpdateDutyGroupVehicle =
  ( "v2" :> "dutyGroups" :> Capture "dutyGroupId" Kernel.Prelude.Text :> "vehicle"
      :> QueryParam
           "operatorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2UpdateVehicleReq
      :> Post
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2DutyGroup
  )

type TransitOperatorV2UpdateDutyGroupVehicleHelper =
  ( "v2" :> "dutyGroups" :> Capture "dutyGroupId" Kernel.Prelude.Text :> "vehicle"
      :> QueryParam
           "operatorId"
           Kernel.Prelude.Text
      :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2UpdateVehicleReq
      :> Post
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2DutyGroup
  )

type TransitOperatorV2UpdateDutyGroupCrew =
  ( "v2" :> "dutyGroups" :> Capture "dutyGroupId" Kernel.Prelude.Text :> "crew"
      :> QueryParam
           "operatorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2UpdateCrewReq
      :> Post
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupDetail
  )

type TransitOperatorV2UpdateDutyGroupCrewHelper =
  ( "v2" :> "dutyGroups" :> Capture "dutyGroupId" Kernel.Prelude.Text :> "crew"
      :> QueryParam
           "operatorId"
           Kernel.Prelude.Text
      :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2UpdateCrewReq
      :> Post
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupDetail
  )

type TransitOperatorV2SetDutyGroupActive =
  ( "v2" :> "dutyGroups" :> Capture "dutyGroupId" Kernel.Prelude.Text :> "active"
      :> QueryParam
           "operatorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2SetActiveReq
      :> Post
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2DutyGroup
  )

type TransitOperatorV2SetDutyGroupActiveHelper =
  ( "v2" :> "dutyGroups" :> Capture "dutyGroupId" Kernel.Prelude.Text :> "active"
      :> QueryParam
           "operatorId"
           Kernel.Prelude.Text
      :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2SetActiveReq
      :> Post
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2DutyGroup
  )

type TransitOperatorV2DeleteDutyGroup =
  ( "v2" :> "dutyGroups" :> Capture "dutyGroupId" Kernel.Prelude.Text :> "delete"
      :> QueryParam
           "operatorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Post
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp
  )

type TransitOperatorV2DeleteDutyGroupHelper =
  ( "v2" :> "dutyGroups" :> Capture "dutyGroupId" Kernel.Prelude.Text :> "delete" :> QueryParam "operatorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Post
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp
  )

type TransitOperatorV2UpdateDutyCrew =
  ( "v2" :> "duties" :> Capture "dutyId" Kernel.Prelude.Text :> "crew" :> QueryParam "operatorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2UpdateCrewReq
      :> Post
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2Duty
  )

type TransitOperatorV2UpdateDutyCrewHelper =
  ( "v2" :> "duties" :> Capture "dutyId" Kernel.Prelude.Text :> "crew" :> QueryParam "operatorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2UpdateCrewReq
      :> Post
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2Duty
  )

type TransitOperatorV2DeleteDuty =
  ( "v2" :> "duties" :> Capture "dutyId" Kernel.Prelude.Text :> "delete" :> QueryParam "operatorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Post ('[JSON]) SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp
  )

type TransitOperatorV2DeleteDutyHelper =
  ( "v2" :> "duties" :> Capture "dutyId" Kernel.Prelude.Text :> "delete" :> QueryParam "operatorId" Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Post
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp
  )

type TransitOperatorV2ListGenerationFailures =
  ( "v2" :> "generationFailures" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "operatorId"
           Kernel.Prelude.Text
      :> QueryParam "resolved" Kernel.Prelude.Bool
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2DutyEventLogPage
  )

type TransitOperatorV2ListGenerationFailuresHelper =
  ( "v2" :> "generationFailures" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "operatorId"
           Kernel.Prelude.Text
      :> QueryParam "resolved" Kernel.Prelude.Bool
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2DutyEventLogPage
  )

type TransitOperatorV2ResolveGenerationFailure =
  ( "v2" :> "generationFailures" :> Capture "failureId" Kernel.Prelude.Text :> "resolve"
      :> QueryParam
           "operatorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Post
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2DutyEventLog
  )

type TransitOperatorV2ResolveGenerationFailureHelper =
  ( "v2" :> "generationFailures" :> Capture "failureId" Kernel.Prelude.Text :> "resolve"
      :> QueryParam
           "operatorId"
           Kernel.Prelude.Text
      :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Post
           ('[JSON])
           SharedLogic.External.Nandi.TransitV2Types.V2DutyEventLog
  )

data TransitOperatorAPIs = TransitOperatorAPIs
  { transitOperatorGetRow :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.Types.NandiRow),
    transitOperatorGetAllRows :: (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.NandiRow]),
    transitOperatorDeleteRow :: (SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Data.Aeson.Value -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.Types.RowsAffectedResp),
    transitOperatorUpsertRow :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Data.Aeson.Value -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.Types.NandiRow),
    transitOperatorUpsertRows :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> [Data.Aeson.Value] -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.NandiRow]),
    transitOperatorQueryRows :: (SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.QueryBody -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.NandiRow]),
    transitOperatorGetServiceTypes :: (BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.ServiceType]),
    transitOperatorGetRoutes :: (BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.NandiRoute]),
    transitOperatorGetDepots :: (BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.Depot]),
    transitOperatorGetShiftTypes :: (BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.ShiftType]),
    transitOperatorGetScheduleNumbers :: (BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.ScheduleNumber]),
    transitOperatorGetDayTypes :: (BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.DayType]),
    transitOperatorGetTripTypes :: (BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.TripType]),
    transitOperatorGetBreakTypes :: (BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.BreakType]),
    transitOperatorGetTripDetails :: (Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.NandiTripDetail]),
    transitOperatorGetFleets :: (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.Fleet]),
    transitOperatorGetConductor :: (Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.Types.Employee),
    transitOperatorGetDriver :: (Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.Types.Employee),
    transitOperatorGetDeviceIds :: (BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [Kernel.Prelude.Text]),
    transitOperatorGetTabletIds :: (BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [Kernel.Prelude.Text]),
    transitOperatorGetOperators :: (SharedLogic.External.Nandi.Types.OperatorRole -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.Employee]),
    transitOperatorUpdateWaybillStatus :: (BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillStatusReq -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.Types.RowsAffectedResp),
    transitOperatorUpdateWaybillFleet :: (BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillFleetReq -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.Types.RowsAffectedResp),
    transitOperatorUpdateWaybillDetails :: (BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillDetailsReq -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.Types.RowsAffectedResp),
    transitOperatorUpdateWaybillTablet :: (BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillTabletReq -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.Types.RowsAffectedResp),
    transitOperatorGetWaybills :: (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.NandiWaybillRow]),
    transitOperatorGetDeviceVehicleMappingList :: (EulerHS.Types.EulerClient DeviceVehicleMappingListRes),
    transitOperatorUpsertDeviceVehicleMapping :: ((Data.ByteString.Lazy.ByteString, UpsertDeviceVehicleMappingReq) -> EulerHS.Types.EulerClient UpsertDeviceVehicleMappingResp),
    transitOperatorUnblockBus :: (Kernel.Prelude.Text -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    transitOperatorSearchStops :: (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.EnrichedStop]),
    transitOperatorNearbyStops :: (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Double) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Double -> Kernel.Prelude.Double -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.EnrichedStop]),
    transitOperatorBulkReplaceStops :: (BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.BulkReplaceReq -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.Types.BulkReplaceResult),
    transitOperatorRouteStops :: (Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.Types.RouteStopsResponse),
    transitOperatorInsertRouteStop :: (Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.InsertRouteStopReq -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.Types.InsertRouteStopResp),
    transitOperatorReprocessRoutes :: (BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.ReprocessReq -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.ReprocessResult]),
    transitOperatorExportRouteStopMapping :: (BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.RouteStopMappingExport]),
    transitOperatorQueryVehicle :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.Fleet]),
    transitOperatorUpsertVehicles :: (BecknV2.OnDemand.Enums.VehicleCategory -> [SharedLogic.External.Nandi.Types.VehicleUpsertRequest] -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.Fleet]),
    transitOperatorDeleteVehicle :: (BecknV2.OnDemand.Enums.VehicleCategory -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.Types.RowsAffectedResp),
    transitOperatorGetScheduleTripRepeat :: (Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.Types.ScheduleTripRepeatConfig),
    transitOperatorSetScheduleTripRepeat :: (Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.SetScheduleTripRepeatReq -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.Types.ScheduleTripRepeatConfig),
    transitOperatorV2ListTripGroups :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.TransitV2Types.V2TripGroupPage),
    transitOperatorV2UpsertTripGroup :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpsertTripGroupReq -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.TransitV2Types.V2TripGroup),
    transitOperatorV2GetTripGroup :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.TransitV2Types.V2TripGroup),
    transitOperatorV2DeleteTripGroup :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp),
    transitOperatorV2ListTrips :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.TransitV2Types.V2Trip]),
    transitOperatorV2UpsertTrips :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpsertTripsReq -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.TransitV2Types.V2Trip]),
    transitOperatorV2DeleteTrip :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp),
    transitOperatorV2ListDutyRepeats :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.TransitV2Types.V2DutyRepeatPage),
    transitOperatorV2UpsertDutyRepeat :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpsertDutyRepeatReq -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.TransitV2Types.V2UpsertDutyRepeatResp),
    transitOperatorV2DeleteDutyRepeat :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp),
    transitOperatorV2PreviewDutyRepeats :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2GenerateReq -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.TransitV2Types.V2GenerateEntry]),
    transitOperatorV2GenerateDutyRepeats :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2GenerateReq -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.TransitV2Types.V2GenerateEntry]),
    transitOperatorV2ListDutyGroups :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupPage),
    transitOperatorV2CreateDutyGroup :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2CreateDutyGroupReq -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupDetail),
    transitOperatorV2GetDutyGroup :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupDetail),
    transitOperatorV2ListDuties :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.TransitV2Types.V2Duty]),
    transitOperatorV2GetDutyGroupLogs :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.TransitV2Types.V2DutyEventLog]),
    transitOperatorV2UpdateDutyGroupVehicle :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpdateVehicleReq -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.TransitV2Types.V2DutyGroup),
    transitOperatorV2UpdateDutyGroupCrew :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpdateCrewReq -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.TransitV2Types.V2DutyGroupDetail),
    transitOperatorV2SetDutyGroupActive :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2SetActiveReq -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.TransitV2Types.V2DutyGroup),
    transitOperatorV2DeleteDutyGroup :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp),
    transitOperatorV2UpdateDutyCrew :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.TransitV2Types.V2UpdateCrewReq -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.TransitV2Types.V2Duty),
    transitOperatorV2DeleteDuty :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.TransitV2Types.V2SuccessResp),
    transitOperatorV2ListGenerationFailures :: (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.TransitV2Types.V2DutyEventLogPage),
    transitOperatorV2ResolveGenerationFailure :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.TransitV2Types.V2DutyEventLog)
  }

mkTransitOperatorAPIs :: (Client EulerHS.Types.EulerClient API -> TransitOperatorAPIs)
mkTransitOperatorAPIs transitOperatorClient = (TransitOperatorAPIs {..})
  where
    transitOperatorGetRow :<|> transitOperatorGetAllRows :<|> transitOperatorDeleteRow :<|> transitOperatorUpsertRow :<|> transitOperatorUpsertRows :<|> transitOperatorQueryRows :<|> transitOperatorGetServiceTypes :<|> transitOperatorGetRoutes :<|> transitOperatorGetDepots :<|> transitOperatorGetShiftTypes :<|> transitOperatorGetScheduleNumbers :<|> transitOperatorGetDayTypes :<|> transitOperatorGetTripTypes :<|> transitOperatorGetBreakTypes :<|> transitOperatorGetTripDetails :<|> transitOperatorGetFleets :<|> transitOperatorGetConductor :<|> transitOperatorGetDriver :<|> transitOperatorGetDeviceIds :<|> transitOperatorGetTabletIds :<|> transitOperatorGetOperators :<|> transitOperatorUpdateWaybillStatus :<|> transitOperatorUpdateWaybillFleet :<|> transitOperatorUpdateWaybillDetails :<|> transitOperatorUpdateWaybillTablet :<|> transitOperatorGetWaybills :<|> transitOperatorGetDeviceVehicleMappingList :<|> transitOperatorUpsertDeviceVehicleMapping :<|> transitOperatorUnblockBus :<|> transitOperatorSearchStops :<|> transitOperatorNearbyStops :<|> transitOperatorBulkReplaceStops :<|> transitOperatorRouteStops :<|> transitOperatorInsertRouteStop :<|> transitOperatorReprocessRoutes :<|> transitOperatorExportRouteStopMapping :<|> transitOperatorQueryVehicle :<|> transitOperatorUpsertVehicles :<|> transitOperatorDeleteVehicle :<|> transitOperatorGetScheduleTripRepeat :<|> transitOperatorSetScheduleTripRepeat :<|> transitOperatorV2ListTripGroups :<|> transitOperatorV2UpsertTripGroup :<|> transitOperatorV2GetTripGroup :<|> transitOperatorV2DeleteTripGroup :<|> transitOperatorV2ListTrips :<|> transitOperatorV2UpsertTrips :<|> transitOperatorV2DeleteTrip :<|> transitOperatorV2ListDutyRepeats :<|> transitOperatorV2UpsertDutyRepeat :<|> transitOperatorV2DeleteDutyRepeat :<|> transitOperatorV2PreviewDutyRepeats :<|> transitOperatorV2GenerateDutyRepeats :<|> transitOperatorV2ListDutyGroups :<|> transitOperatorV2CreateDutyGroup :<|> transitOperatorV2GetDutyGroup :<|> transitOperatorV2ListDuties :<|> transitOperatorV2GetDutyGroupLogs :<|> transitOperatorV2UpdateDutyGroupVehicle :<|> transitOperatorV2UpdateDutyGroupCrew :<|> transitOperatorV2SetDutyGroupActive :<|> transitOperatorV2DeleteDutyGroup :<|> transitOperatorV2UpdateDutyCrew :<|> transitOperatorV2DeleteDuty :<|> transitOperatorV2ListGenerationFailures :<|> transitOperatorV2ResolveGenerationFailure = transitOperatorClient

data TransitOperatorUserActionType
  = TRANSIT_OPERATOR_GET_ROW
  | TRANSIT_OPERATOR_GET_ALL_ROWS
  | TRANSIT_OPERATOR_DELETE_ROW
  | TRANSIT_OPERATOR_UPSERT_ROW
  | TRANSIT_OPERATOR_UPSERT_ROWS
  | TRANSIT_OPERATOR_QUERY_ROWS
  | TRANSIT_OPERATOR_GET_SERVICE_TYPES
  | TRANSIT_OPERATOR_GET_ROUTES
  | TRANSIT_OPERATOR_GET_DEPOTS
  | TRANSIT_OPERATOR_GET_SHIFT_TYPES
  | TRANSIT_OPERATOR_GET_SCHEDULE_NUMBERS
  | TRANSIT_OPERATOR_GET_DAY_TYPES
  | TRANSIT_OPERATOR_GET_TRIP_TYPES
  | TRANSIT_OPERATOR_GET_BREAK_TYPES
  | TRANSIT_OPERATOR_GET_TRIP_DETAILS
  | TRANSIT_OPERATOR_GET_FLEETS
  | TRANSIT_OPERATOR_GET_CONDUCTOR
  | TRANSIT_OPERATOR_GET_DRIVER
  | TRANSIT_OPERATOR_GET_DEVICE_IDS
  | TRANSIT_OPERATOR_GET_TABLET_IDS
  | TRANSIT_OPERATOR_GET_OPERATORS
  | TRANSIT_OPERATOR_UPDATE_WAYBILL_STATUS
  | TRANSIT_OPERATOR_UPDATE_WAYBILL_FLEET
  | TRANSIT_OPERATOR_UPDATE_WAYBILL_DETAILS
  | TRANSIT_OPERATOR_UPDATE_WAYBILL_TABLET
  | TRANSIT_OPERATOR_GET_WAYBILLS
  | TRANSIT_OPERATOR_GET_DEVICE_VEHICLE_MAPPING_LIST
  | TRANSIT_OPERATOR_UPSERT_DEVICE_VEHICLE_MAPPING
  | TRANSIT_OPERATOR_UNBLOCK_BUS
  | TRANSIT_OPERATOR_SEARCH_STOPS
  | TRANSIT_OPERATOR_NEARBY_STOPS
  | TRANSIT_OPERATOR_BULK_REPLACE_STOPS
  | TRANSIT_OPERATOR_ROUTE_STOPS
  | TRANSIT_OPERATOR_INSERT_ROUTE_STOP
  | TRANSIT_OPERATOR_REPROCESS_ROUTES
  | TRANSIT_OPERATOR_EXPORT_ROUTE_STOP_MAPPING
  | TRANSIT_OPERATOR_QUERY_VEHICLE
  | TRANSIT_OPERATOR_UPSERT_VEHICLES
  | TRANSIT_OPERATOR_DELETE_VEHICLE
  | TRANSIT_OPERATOR_GET_SCHEDULE_TRIP_REPEAT
  | TRANSIT_OPERATOR_SET_SCHEDULE_TRIP_REPEAT
  | TRANSIT_OPERATOR_V2_LIST_TRIP_GROUPS
  | TRANSIT_OPERATOR_V2_UPSERT_TRIP_GROUP
  | TRANSIT_OPERATOR_V2_GET_TRIP_GROUP
  | TRANSIT_OPERATOR_V2_DELETE_TRIP_GROUP
  | TRANSIT_OPERATOR_V2_LIST_TRIPS
  | TRANSIT_OPERATOR_V2_UPSERT_TRIPS
  | TRANSIT_OPERATOR_V2_DELETE_TRIP
  | TRANSIT_OPERATOR_V2_LIST_DUTY_REPEATS
  | TRANSIT_OPERATOR_V2_UPSERT_DUTY_REPEAT
  | TRANSIT_OPERATOR_V2_DELETE_DUTY_REPEAT
  | TRANSIT_OPERATOR_V2_PREVIEW_DUTY_REPEATS
  | TRANSIT_OPERATOR_V2_GENERATE_DUTY_REPEATS
  | TRANSIT_OPERATOR_V2_LIST_DUTY_GROUPS
  | TRANSIT_OPERATOR_V2_CREATE_DUTY_GROUP
  | TRANSIT_OPERATOR_V2_GET_DUTY_GROUP
  | TRANSIT_OPERATOR_V2_LIST_DUTIES
  | TRANSIT_OPERATOR_V2_GET_DUTY_GROUP_LOGS
  | TRANSIT_OPERATOR_V2_UPDATE_DUTY_GROUP_VEHICLE
  | TRANSIT_OPERATOR_V2_UPDATE_DUTY_GROUP_CREW
  | TRANSIT_OPERATOR_V2_SET_DUTY_GROUP_ACTIVE
  | TRANSIT_OPERATOR_V2_DELETE_DUTY_GROUP
  | TRANSIT_OPERATOR_V2_UPDATE_DUTY_CREW
  | TRANSIT_OPERATOR_V2_DELETE_DUTY
  | TRANSIT_OPERATOR_V2_LIST_GENERATION_FAILURES
  | TRANSIT_OPERATOR_V2_RESOLVE_GENERATION_FAILURE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''TransitOperatorUserActionType)])
