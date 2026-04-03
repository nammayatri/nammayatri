{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneKindSignatures #-}
module API.Types.Dashboard.AppManagement.Endpoints.TransitOperator where
import EulerHS.Prelude hiding (id, state)
import Servant
import Data.OpenApi (ToSchema)
import Servant.Client
import Kernel.Types.Common
import qualified Kernel.Prelude
import qualified EulerHS.Prelude
import qualified "this" SharedLogic.External.Nandi.Types
import qualified "beckn-spec" BecknV2.OnDemand.Enums
import qualified Data.Aeson
import qualified EulerHS.Types
import qualified Data.Singletons.TH
import qualified Kernel.ServantMultipart
import qualified Data.ByteString.Lazy
import qualified Kernel.Types.HideSecrets



data DeviceVehicleMappingItem
    = DeviceVehicleMappingItem {createdAt :: Kernel.Prelude.UTCTime, deviceId :: Kernel.Prelude.Text, gtfsId :: Kernel.Prelude.Text, updatedAt :: Kernel.Prelude.UTCTime, vehicleNo :: Kernel.Prelude.Text}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data DeviceVehicleMappingListRes
    = DeviceVehicleMappingListRes {mappings :: [DeviceVehicleMappingItem]}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
newtype UpsertDeviceVehicleMappingReq
  = UpsertDeviceVehicleMappingReq {file :: EulerHS.Prelude.FilePath}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
instance Kernel.Types.HideSecrets.HideSecrets UpsertDeviceVehicleMappingReq
    where hideSecrets = Kernel.Prelude.identity
data UpsertDeviceVehicleMappingResp
    = UpsertDeviceVehicleMappingResp {success :: Kernel.Prelude.Text, unprocessedEntries :: [Kernel.Prelude.Text]}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
type API = ("transitOperator" :> (TransitOperatorGetRow :<|> TransitOperatorGetAllRows :<|> TransitOperatorDeleteRow :<|> TransitOperatorUpsertRow :<|> TransitOperatorQueryRows :<|> TransitOperatorGetServiceTypes :<|> TransitOperatorGetRoutes :<|> TransitOperatorGetDepots :<|> TransitOperatorGetShiftTypes :<|> TransitOperatorGetScheduleNumbers :<|> TransitOperatorGetDayTypes :<|> TransitOperatorGetTripTypes :<|> TransitOperatorGetBreakTypes :<|> TransitOperatorGetTripDetails :<|> TransitOperatorGetFleets :<|> TransitOperatorGetConductor :<|> TransitOperatorGetDriver :<|> TransitOperatorGetDeviceIds :<|> TransitOperatorGetTabletIds :<|> TransitOperatorGetOperators :<|> TransitOperatorUpdateWaybillStatus :<|> TransitOperatorUpdateWaybillFleet :<|> TransitOperatorUpdateWaybillTablet :<|> TransitOperatorGetWaybills :<|> TransitOperatorGetDeviceVehicleMappingList :<|> TransitOperatorUpsertDeviceVehicleMapping))
type TransitOperatorGetRow = ("row" :> QueryParam "column" Kernel.Prelude.Text :> MandatoryQueryParam "table" SharedLogic.External.Nandi.Types.NandiTable :> MandatoryQueryParam "vehicleCategory"
                                                                                                                                                                                 BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) SharedLogic.External.Nandi.Types.NandiRow)
type TransitOperatorGetAllRows = ("allRows" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> MandatoryQueryParam "table"
                                                                                                                                                      SharedLogic.External.Nandi.Types.NandiTable :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON])
                                                                                                                                                                                                                                                                                         [SharedLogic.External.Nandi.Types.NandiRow])
type TransitOperatorDeleteRow = ("row" :> MandatoryQueryParam "table" SharedLogic.External.Nandi.Types.NandiTable :> MandatoryQueryParam "vehicleCategory"
                                                                                                                                         BecknV2.OnDemand.Enums.VehicleCategory :> ReqBody ('[JSON]) Data.Aeson.Value :> Delete ('[JSON]) SharedLogic.External.Nandi.Types.RowsAffectedResp)
type TransitOperatorUpsertRow = ("row" :> QueryParam "toRegen" Kernel.Prelude.Text :> MandatoryQueryParam "table" SharedLogic.External.Nandi.Types.NandiTable :> MandatoryQueryParam "vehicleCategory"
                                                                                                                                                                                     BecknV2.OnDemand.Enums.VehicleCategory :> ReqBody ('[JSON]) Data.Aeson.Value :> Post ('[JSON])
                                                                                                                                                                                                                                                                          SharedLogic.External.Nandi.Types.NandiRow)
type TransitOperatorQueryRows = ("queryRow" :> MandatoryQueryParam "table" SharedLogic.External.Nandi.Types.NandiTable :> MandatoryQueryParam "vehicleCategory"
                                                                                                                                              BecknV2.OnDemand.Enums.VehicleCategory :> ReqBody ('[JSON]) SharedLogic.External.Nandi.Types.QueryBody :> Post ('[JSON])
                                                                                                                                                                                                                                                             [SharedLogic.External.Nandi.Types.NandiRow])
type TransitOperatorGetServiceTypes = ("serviceTypes" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [SharedLogic.External.Nandi.Types.ServiceType])
type TransitOperatorGetRoutes = ("routes" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [SharedLogic.External.Nandi.Types.NandiRoute])
type TransitOperatorGetDepots = ("depots" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [SharedLogic.External.Nandi.Types.Depot])
type TransitOperatorGetShiftTypes = ("shiftTypes" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [SharedLogic.External.Nandi.Types.ShiftType])
type TransitOperatorGetScheduleNumbers = ("scheduleNumbers" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON])
                                                                                                                                                   [SharedLogic.External.Nandi.Types.ScheduleNumber])
type TransitOperatorGetDayTypes = ("dayTypes" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [SharedLogic.External.Nandi.Types.DayType])
type TransitOperatorGetTripTypes = ("tripTypes" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [SharedLogic.External.Nandi.Types.TripType])
type TransitOperatorGetBreakTypes = ("breakTypes" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [SharedLogic.External.Nandi.Types.BreakType])
type TransitOperatorGetTripDetails = ("tripDetails" :> MandatoryQueryParam "scheduleNumber" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory"
                                                                                                                                       BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [SharedLogic.External.Nandi.Types.NandiTripDetail])
type TransitOperatorGetFleets = ("fleets" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [SharedLogic.External.Nandi.Types.Fleet])
type TransitOperatorGetConductor = ("conductor" :> MandatoryQueryParam "token" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON])
                                                                                                                                                                                          SharedLogic.External.Nandi.Types.Employee)
type TransitOperatorGetDriver = ("driver" :> MandatoryQueryParam "token" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON])
                                                                                                                                                                                    SharedLogic.External.Nandi.Types.Employee)
type TransitOperatorGetDeviceIds = ("deviceIds" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [Kernel.Prelude.Text])
type TransitOperatorGetTabletIds = ("tabletIds" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [Kernel.Prelude.Text])
type TransitOperatorGetOperators = ("operators" :> MandatoryQueryParam "role" SharedLogic.External.Nandi.Types.OperatorRole :> MandatoryQueryParam "vehicleCategory"
                                                                                                                                                   BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [SharedLogic.External.Nandi.Types.Employee])
type TransitOperatorUpdateWaybillStatus = ("waybillStatus" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> ReqBody ('[JSON])
                                                                                                                                                      SharedLogic.External.Nandi.Types.UpdateWaybillStatusReq :> Post ('[JSON]) SharedLogic.External.Nandi.Types.RowsAffectedResp)
type TransitOperatorUpdateWaybillFleet = ("waybillFleet" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> ReqBody ('[JSON])
                                                                                                                                                    SharedLogic.External.Nandi.Types.UpdateWaybillFleetReq :> Post ('[JSON]) SharedLogic.External.Nandi.Types.RowsAffectedResp)
type TransitOperatorUpdateWaybillTablet = ("waybillTablet" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> ReqBody ('[JSON])
                                                                                                                                                      SharedLogic.External.Nandi.Types.UpdateWaybillTabletReq :> Post ('[JSON]) SharedLogic.External.Nandi.Types.RowsAffectedResp)
type TransitOperatorGetWaybills = ("waybills" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> MandatoryQueryParam "vehicleCategory"
                                                                                                                                                        BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [SharedLogic.External.Nandi.Types.NandiWaybillRow])
type TransitOperatorGetDeviceVehicleMappingList = ("deviceVehicleMapping" :> "list" :> Get ('[JSON]) DeviceVehicleMappingListRes)
type TransitOperatorUpsertDeviceVehicleMapping = ("deviceVehicleMapping" :> "upsert" :> Kernel.ServantMultipart.MultipartForm Kernel.ServantMultipart.Tmp
                                                                                                                              UpsertDeviceVehicleMappingReq :> Post ('[JSON]) UpsertDeviceVehicleMappingResp)
data TransitOperatorAPIs
    = TransitOperatorAPIs {transitOperatorGetRow :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.Types.NandiRow),
                           transitOperatorGetAllRows :: (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.NandiRow]),
                           transitOperatorDeleteRow :: (SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Data.Aeson.Value -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.Types.RowsAffectedResp),
                           transitOperatorUpsertRow :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> SharedLogic.External.Nandi.Types.NandiTable -> BecknV2.OnDemand.Enums.VehicleCategory -> Data.Aeson.Value -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.Types.NandiRow),
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
                           transitOperatorGetFleets :: (BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.Fleet]),
                           transitOperatorGetConductor :: (Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.Types.Employee),
                           transitOperatorGetDriver :: (Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.Types.Employee),
                           transitOperatorGetDeviceIds :: (BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [Kernel.Prelude.Text]),
                           transitOperatorGetTabletIds :: (BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [Kernel.Prelude.Text]),
                           transitOperatorGetOperators :: (SharedLogic.External.Nandi.Types.OperatorRole -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.Employee]),
                           transitOperatorUpdateWaybillStatus :: (BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillStatusReq -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.Types.RowsAffectedResp),
                           transitOperatorUpdateWaybillFleet :: (BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillFleetReq -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.Types.RowsAffectedResp),
                           transitOperatorUpdateWaybillTablet :: (BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillTabletReq -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.Types.RowsAffectedResp),
                           transitOperatorGetWaybills :: (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.NandiWaybillRow]),
                           transitOperatorGetDeviceVehicleMappingList :: (EulerHS.Types.EulerClient DeviceVehicleMappingListRes),
                           transitOperatorUpsertDeviceVehicleMapping :: ((Data.ByteString.Lazy.ByteString, UpsertDeviceVehicleMappingReq) -> EulerHS.Types.EulerClient UpsertDeviceVehicleMappingResp)}
mkTransitOperatorAPIs :: (Client EulerHS.Types.EulerClient API -> TransitOperatorAPIs)
mkTransitOperatorAPIs transitOperatorClient = (TransitOperatorAPIs {..})
                          where transitOperatorGetRow :<|> transitOperatorGetAllRows :<|> transitOperatorDeleteRow :<|> transitOperatorUpsertRow :<|> transitOperatorQueryRows :<|> transitOperatorGetServiceTypes :<|> transitOperatorGetRoutes :<|> transitOperatorGetDepots :<|> transitOperatorGetShiftTypes :<|> transitOperatorGetScheduleNumbers :<|> transitOperatorGetDayTypes :<|> transitOperatorGetTripTypes :<|> transitOperatorGetBreakTypes :<|> transitOperatorGetTripDetails :<|> transitOperatorGetFleets :<|> transitOperatorGetConductor :<|> transitOperatorGetDriver :<|> transitOperatorGetDeviceIds :<|> transitOperatorGetTabletIds :<|> transitOperatorGetOperators :<|> transitOperatorUpdateWaybillStatus :<|> transitOperatorUpdateWaybillFleet :<|> transitOperatorUpdateWaybillTablet :<|> transitOperatorGetWaybills :<|> transitOperatorGetDeviceVehicleMappingList :<|> transitOperatorUpsertDeviceVehicleMapping = transitOperatorClient
data TransitOperatorUserActionType
    = TRANSIT_OPERATOR_GET_ROW
    | TRANSIT_OPERATOR_GET_ALL_ROWS
    | TRANSIT_OPERATOR_DELETE_ROW
    | TRANSIT_OPERATOR_UPSERT_ROW
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
    | TRANSIT_OPERATOR_UPDATE_WAYBILL_TABLET
    | TRANSIT_OPERATOR_GET_WAYBILLS
    | TRANSIT_OPERATOR_GET_DEVICE_VEHICLE_MAPPING_LIST
    | TRANSIT_OPERATOR_UPSERT_DEVICE_VEHICLE_MAPPING
    deriving stock (Show, Read, Generic, Eq, Ord)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''TransitOperatorUserActionType)])

