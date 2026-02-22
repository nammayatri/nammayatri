{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.TransitOperator where

import qualified "beckn-spec" BecknV2.OnDemand.Enums
import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import Servant
import Servant.Client
import qualified "this" SharedLogic.External.Nandi.Types

type API = ("/transitOperator" :> (TransitOperatorGetRow :<|> TransitOperatorGetAllRows :<|> TransitOperatorDeleteRow :<|> TransitOperatorUpsertRow :<|> TransitOperatorGetServiceTypes :<|> TransitOperatorGetRoutes :<|> TransitOperatorGetDepots :<|> TransitOperatorGetShiftTypes :<|> TransitOperatorGetScheduleNumbers :<|> TransitOperatorGetDayTypes :<|> TransitOperatorGetTripTypes :<|> TransitOperatorGetBreakTypes :<|> TransitOperatorGetTripDetails :<|> TransitOperatorGetFleets :<|> TransitOperatorGetConductor :<|> TransitOperatorGetDriver :<|> TransitOperatorGetDeviceIds :<|> TransitOperatorGetTabletIds :<|> TransitOperatorGetOperators :<|> TransitOperatorUpdateWaybillStatus :<|> TransitOperatorUpdateWaybillFleet :<|> TransitOperatorUpdateWaybillTablet :<|> TransitOperatorGetWaybills))

type TransitOperatorGetRow =
  ( "row" :> QueryParam "column" Kernel.Prelude.Text :> MandatoryQueryParam "table" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) Data.Aeson.Value
  )

type TransitOperatorGetAllRows =
  ( "allRows" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> MandatoryQueryParam
           "table"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) [Data.Aeson.Value]
  )

type TransitOperatorDeleteRow =
  ( "row" :> MandatoryQueryParam "table" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Data.Aeson.Value
      :> Delete ('[JSON]) SharedLogic.External.Nandi.Types.RowsAffectedResp
  )

type TransitOperatorUpsertRow =
  ( "row" :> MandatoryQueryParam "table" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Data.Aeson.Value
      :> Post ('[JSON]) Data.Aeson.Value
  )

type TransitOperatorGetServiceTypes = ("serviceTypes" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [SharedLogic.External.Nandi.Types.ServiceType])

type TransitOperatorGetRoutes = ("routes" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [SharedLogic.External.Nandi.Types.NandiRoute])

type TransitOperatorGetDepots = ("depots" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [SharedLogic.External.Nandi.Types.Depot])

type TransitOperatorGetShiftTypes = ("shiftTypes" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [Kernel.Prelude.Text])

type TransitOperatorGetScheduleNumbers =
  ( "scheduleNumbers" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           [SharedLogic.External.Nandi.Types.ScheduleNumber]
  )

type TransitOperatorGetDayTypes = ("dayTypes" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [Kernel.Prelude.Text])

type TransitOperatorGetTripTypes = ("tripTypes" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [Kernel.Prelude.Text])

type TransitOperatorGetBreakTypes = ("breakTypes" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [Kernel.Prelude.Text])

type TransitOperatorGetTripDetails =
  ( "tripDetails" :> MandatoryQueryParam "scheduleNumber" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) [SharedLogic.External.Nandi.Types.NandiTripDetail]
  )

type TransitOperatorGetFleets = ("fleets" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [SharedLogic.External.Nandi.Types.Fleet])

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
  ( "operators" :> MandatoryQueryParam "role" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           [SharedLogic.External.Nandi.Types.Employee]
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
      :> Get ('[JSON]) [Data.Aeson.Value]
  )

data TransitOperatorAPIs = TransitOperatorAPIs
  { transitOperatorGetRow :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Data.Aeson.Value),
    transitOperatorGetAllRows :: (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [Data.Aeson.Value]),
    transitOperatorDeleteRow :: (Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Data.Aeson.Value -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.Types.RowsAffectedResp),
    transitOperatorUpsertRow :: (Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Data.Aeson.Value -> EulerHS.Types.EulerClient Data.Aeson.Value),
    transitOperatorGetServiceTypes :: (BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.ServiceType]),
    transitOperatorGetRoutes :: (BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.NandiRoute]),
    transitOperatorGetDepots :: (BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.Depot]),
    transitOperatorGetShiftTypes :: (BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [Kernel.Prelude.Text]),
    transitOperatorGetScheduleNumbers :: (BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.ScheduleNumber]),
    transitOperatorGetDayTypes :: (BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [Kernel.Prelude.Text]),
    transitOperatorGetTripTypes :: (BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [Kernel.Prelude.Text]),
    transitOperatorGetBreakTypes :: (BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [Kernel.Prelude.Text]),
    transitOperatorGetTripDetails :: (Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.NandiTripDetail]),
    transitOperatorGetFleets :: (BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.Fleet]),
    transitOperatorGetConductor :: (Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.Types.Employee),
    transitOperatorGetDriver :: (Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.Types.Employee),
    transitOperatorGetDeviceIds :: (BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [Kernel.Prelude.Text]),
    transitOperatorGetTabletIds :: (BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [Kernel.Prelude.Text]),
    transitOperatorGetOperators :: (Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [SharedLogic.External.Nandi.Types.Employee]),
    transitOperatorUpdateWaybillStatus :: (BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillStatusReq -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.Types.RowsAffectedResp),
    transitOperatorUpdateWaybillFleet :: (BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillFleetReq -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.Types.RowsAffectedResp),
    transitOperatorUpdateWaybillTablet :: (BecknV2.OnDemand.Enums.VehicleCategory -> SharedLogic.External.Nandi.Types.UpdateWaybillTabletReq -> EulerHS.Types.EulerClient SharedLogic.External.Nandi.Types.RowsAffectedResp),
    transitOperatorGetWaybills :: (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [Data.Aeson.Value])
  }

mkTransitOperatorAPIs :: (Client EulerHS.Types.EulerClient API -> TransitOperatorAPIs)
mkTransitOperatorAPIs transitOperatorClient = (TransitOperatorAPIs {..})
  where
    transitOperatorGetRow :<|> transitOperatorGetAllRows :<|> transitOperatorDeleteRow :<|> transitOperatorUpsertRow :<|> transitOperatorGetServiceTypes :<|> transitOperatorGetRoutes :<|> transitOperatorGetDepots :<|> transitOperatorGetShiftTypes :<|> transitOperatorGetScheduleNumbers :<|> transitOperatorGetDayTypes :<|> transitOperatorGetTripTypes :<|> transitOperatorGetBreakTypes :<|> transitOperatorGetTripDetails :<|> transitOperatorGetFleets :<|> transitOperatorGetConductor :<|> transitOperatorGetDriver :<|> transitOperatorGetDeviceIds :<|> transitOperatorGetTabletIds :<|> transitOperatorGetOperators :<|> transitOperatorUpdateWaybillStatus :<|> transitOperatorUpdateWaybillFleet :<|> transitOperatorUpdateWaybillTablet :<|> transitOperatorGetWaybills = transitOperatorClient

data TransitOperatorUserActionType
  = TRANSIT_OPERATOR_GET_ROW
  | TRANSIT_OPERATOR_GET_ALL_ROWS
  | TRANSIT_OPERATOR_DELETE_ROW
  | TRANSIT_OPERATOR_UPSERT_ROW
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
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''TransitOperatorUserActionType)])
