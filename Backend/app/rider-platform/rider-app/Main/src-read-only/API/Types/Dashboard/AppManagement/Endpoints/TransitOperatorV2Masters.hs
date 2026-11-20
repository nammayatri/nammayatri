{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.TransitOperatorV2Masters where

import qualified "beckn-spec" BecknV2.OnDemand.Enums
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified "bms-backend" Lib.BmsBackend.Transit.Common
import qualified "bms-backend" Lib.BmsBackend.Transit.Masters.Types
import Servant
import Servant.Client

data GtfsRoute = GtfsRoute
  { id :: Kernel.Prelude.Text,
    mode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    number :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    stopCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    tripCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data GtfsRouteStop = GtfsRouteStop
  { isStageStop :: Kernel.Prelude.Bool,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sequenceNum :: Kernel.Prelude.Int,
    stageNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    stopCode :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data GtfsStop = GtfsStop {code :: Kernel.Prelude.Text, lat :: Kernel.Prelude.Double, lon :: Kernel.Prelude.Double, name :: Kernel.Prelude.Maybe Kernel.Prelude.Text, routeCodes :: [Kernel.Prelude.Text]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TransitPerson = TransitPerson
  { email :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    firstName :: Kernel.Prelude.Text,
    id :: Kernel.Prelude.Text,
    lastName :: Kernel.Prelude.Text,
    mobileCountryCode :: Kernel.Prelude.Text,
    mobileNumber :: Kernel.Prelude.Text,
    organizationId :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TransitPersonList = TransitPersonList {items :: [TransitPerson], total :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("transitOperatorV2" :> (TransitOperatorV2ListOrganizationsHelper :<|> TransitOperatorV2GetOrganizationHelper :<|> TransitOperatorV2CreateOrganizationHelper :<|> TransitOperatorV2UpdateOrganizationHelper :<|> TransitOperatorV2DeleteOrganizationHelper :<|> TransitOperatorV2ListDepotsHelper :<|> TransitOperatorV2GetDepotHelper :<|> TransitOperatorV2CreateDepotHelper :<|> TransitOperatorV2UpdateDepotHelper :<|> TransitOperatorV2DeleteDepotHelper :<|> TransitOperatorV2ListVehiclesHelper :<|> TransitOperatorV2GetVehicleHelper :<|> TransitOperatorV2CreateVehicleHelper :<|> TransitOperatorV2UpdateVehicleHelper :<|> TransitOperatorV2DeleteVehicleHelper :<|> TransitOperatorV2BulkVehiclesHelper :<|> TransitOperatorV2ListDevicesHelper :<|> TransitOperatorV2BulkDevicesHelper :<|> TransitOperatorV2DeleteDeviceHelper :<|> TransitOperatorV2ListEmployeesHelper :<|> TransitOperatorV2GetEmployeeHelper :<|> TransitOperatorV2CreateEmployeeHelper :<|> TransitOperatorV2UpdateEmployeeHelper :<|> TransitOperatorV2DeleteEmployeeHelper :<|> TransitOperatorV2BulkEmployeesHelper :<|> TransitOperatorV2EmployeeLoginHelper :<|> TransitOperatorV2EmployeeVerifyHelper :<|> TransitOperatorV2ListDesignationsHelper :<|> TransitOperatorV2ListCrewHelper :<|> TransitOperatorV2GetLookupsHelper :<|> TransitOperatorV2ListServiceTypesHelper :<|> TransitOperatorV2GetServiceTypeHelper :<|> TransitOperatorV2CreateServiceTypeHelper :<|> TransitOperatorV2UpdateServiceTypeHelper :<|> TransitOperatorV2DeleteServiceTypeHelper :<|> TransitOperatorV2ListStationEtasHelper :<|> TransitOperatorV2PutStationEtasHelper :<|> TransitOperatorV2ListGtfsRoutesHelper :<|> TransitOperatorV2ListGtfsRouteStopsHelper :<|> TransitOperatorV2SearchGtfsStopsHelper :<|> TransitOperatorV2ListPersonsHelper :<|> TransitOperatorV2AssignPersonOrganizationHelper))

type TransitOperatorV2ListOrganizations =
  ( "organizations" :> QueryParam "search" Kernel.Prelude.Text :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.OrganizationList
  )

type TransitOperatorV2ListOrganizationsHelper =
  ( "organizations" :> QueryParam "search" Kernel.Prelude.Text :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.OrganizationList
  )

type TransitOperatorV2GetOrganization =
  ( "organizations" :> Capture "organizationId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.Organization
  )

type TransitOperatorV2GetOrganizationHelper =
  ( "organizations" :> Capture "organizationId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.Organization
  )

type TransitOperatorV2CreateOrganization =
  ( "organizations" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.CreateOrganizationReq
      :> Post ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.Organization
  )

type TransitOperatorV2CreateOrganizationHelper =
  ( "organizations" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.CreateOrganizationReq
      :> Post
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.Organization
  )

type TransitOperatorV2UpdateOrganization =
  ( "organizations" :> Capture "organizationId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.UpdateOrganizationReq
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.Organization
  )

type TransitOperatorV2UpdateOrganizationHelper =
  ( "organizations" :> Capture "organizationId" Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.UpdateOrganizationReq
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.Organization
  )

type TransitOperatorV2DeleteOrganization =
  ( "organizations" :> Capture "organizationId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Delete ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type TransitOperatorV2DeleteOrganizationHelper =
  ( "organizations" :> Capture "organizationId" Kernel.Prelude.Text
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Delete ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type TransitOperatorV2ListDepots =
  ( "depots" :> QueryParam "search" Kernel.Prelude.Text :> QueryParam "organizationId" Kernel.Prelude.Text
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.DepotList
  )

type TransitOperatorV2ListDepotsHelper =
  ( "depots" :> QueryParam "search" Kernel.Prelude.Text :> QueryParam "organizationId" Kernel.Prelude.Text
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.DepotList
  )

type TransitOperatorV2GetDepot =
  ( "depots" :> Capture "depotId" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.Depot
  )

type TransitOperatorV2GetDepotHelper =
  ( "depots" :> Capture "depotId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.Depot
  )

type TransitOperatorV2CreateDepot =
  ( "depots" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.CreateDepotReq
      :> Post ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.Depot
  )

type TransitOperatorV2CreateDepotHelper =
  ( "depots" :> QueryParam "requestorId" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.CreateDepotReq
      :> Post
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.Depot
  )

type TransitOperatorV2UpdateDepot =
  ( "depots" :> Capture "depotId" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.UpdateDepotReq
      :> Put ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.Depot
  )

type TransitOperatorV2UpdateDepotHelper =
  ( "depots" :> Capture "depotId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.UpdateDepotReq
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.Depot
  )

type TransitOperatorV2DeleteDepot =
  ( "depots" :> Capture "depotId" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Delete
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type TransitOperatorV2DeleteDepotHelper =
  ( "depots" :> Capture "depotId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Delete ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type TransitOperatorV2ListVehicles =
  ( "vehicles" :> QueryParam "search" Kernel.Prelude.Text :> QueryParam "depotId" Kernel.Prelude.Text
      :> QueryParam
           "serviceTypeId"
           Kernel.Prelude.Text
      :> QueryParam "vehicleNo" Kernel.Prelude.Text
      :> QueryParam "fleetNo" Kernel.Prelude.Text
      :> QueryParam
           "tagNumber"
           Kernel.Prelude.Text
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.VehicleList
  )

type TransitOperatorV2ListVehiclesHelper =
  ( "vehicles" :> QueryParam "search" Kernel.Prelude.Text :> QueryParam "depotId" Kernel.Prelude.Text
      :> QueryParam
           "serviceTypeId"
           Kernel.Prelude.Text
      :> QueryParam "vehicleNo" Kernel.Prelude.Text
      :> QueryParam "fleetNo" Kernel.Prelude.Text
      :> QueryParam
           "tagNumber"
           Kernel.Prelude.Text
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.VehicleList
  )

type TransitOperatorV2GetVehicle =
  ( "vehicles" :> Capture "vehicleId" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.Vehicle
  )

type TransitOperatorV2GetVehicleHelper =
  ( "vehicles" :> Capture "vehicleId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.Vehicle
  )

type TransitOperatorV2CreateVehicle =
  ( "vehicles" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.CreateVehicleReq
      :> Post ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.Vehicle
  )

type TransitOperatorV2CreateVehicleHelper =
  ( "vehicles" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.CreateVehicleReq
      :> Post
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.Vehicle
  )

type TransitOperatorV2UpdateVehicle =
  ( "vehicles" :> Capture "vehicleId" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.UpdateVehicleReq
      :> Put ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.Vehicle
  )

type TransitOperatorV2UpdateVehicleHelper =
  ( "vehicles" :> Capture "vehicleId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.UpdateVehicleReq
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.Vehicle
  )

type TransitOperatorV2DeleteVehicle =
  ( "vehicles" :> Capture "vehicleId" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Delete
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type TransitOperatorV2DeleteVehicleHelper =
  ( "vehicles" :> Capture "vehicleId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Delete ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type TransitOperatorV2BulkVehicles =
  ( "vehicles" :> "bulk" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.BulkVehiclesReq
      :> Post ('[JSON]) Lib.BmsBackend.Transit.Common.BulkResult
  )

type TransitOperatorV2BulkVehiclesHelper =
  ( "vehicles" :> "bulk" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.BulkVehiclesReq
      :> Post
           ('[JSON])
           Lib.BmsBackend.Transit.Common.BulkResult
  )

type TransitOperatorV2ListDevices =
  ( "devices" :> QueryParam "deviceType" Lib.BmsBackend.Transit.Masters.Types.DeviceType :> QueryParam "search" Kernel.Prelude.Text
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.DeviceList
  )

type TransitOperatorV2ListDevicesHelper =
  ( "devices" :> QueryParam "deviceType" Lib.BmsBackend.Transit.Masters.Types.DeviceType :> QueryParam "search" Kernel.Prelude.Text
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.DeviceList
  )

type TransitOperatorV2BulkDevices =
  ( "devices" :> "bulk" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.BulkDevicesReq
      :> Post ('[JSON]) Lib.BmsBackend.Transit.Common.BulkResult
  )

type TransitOperatorV2BulkDevicesHelper =
  ( "devices" :> "bulk" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.BulkDevicesReq
      :> Post
           ('[JSON])
           Lib.BmsBackend.Transit.Common.BulkResult
  )

type TransitOperatorV2DeleteDevice =
  ( "devices" :> Capture "deviceId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> MandatoryQueryParam "deviceType" Lib.BmsBackend.Transit.Masters.Types.DeviceType
      :> Delete ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type TransitOperatorV2DeleteDeviceHelper =
  ( "devices" :> Capture "deviceId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> MandatoryQueryParam
           "deviceType"
           Lib.BmsBackend.Transit.Masters.Types.DeviceType
      :> Delete
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type TransitOperatorV2ListEmployees =
  ( "employees" :> QueryParam "role" Kernel.Prelude.Text :> QueryParam "depotId" Kernel.Prelude.Text :> QueryParam "search" Kernel.Prelude.Text
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.EmployeeList
  )

type TransitOperatorV2ListEmployeesHelper =
  ( "employees" :> QueryParam "role" Kernel.Prelude.Text :> QueryParam "depotId" Kernel.Prelude.Text
      :> QueryParam
           "search"
           Kernel.Prelude.Text
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.EmployeeList
  )

type TransitOperatorV2GetEmployee =
  ( "employees" :> Capture "employeeId" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.Employee
  )

type TransitOperatorV2GetEmployeeHelper =
  ( "employees" :> Capture "employeeId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.Employee
  )

type TransitOperatorV2CreateEmployee =
  ( "employees" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.CreateEmployeeReq
      :> Post ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.Employee
  )

type TransitOperatorV2CreateEmployeeHelper =
  ( "employees" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.CreateEmployeeReq
      :> Post
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.Employee
  )

type TransitOperatorV2UpdateEmployee =
  ( "employees" :> Capture "employeeId" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.UpdateEmployeeReq
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.Employee
  )

type TransitOperatorV2UpdateEmployeeHelper =
  ( "employees" :> Capture "employeeId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.UpdateEmployeeReq
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.Employee
  )

type TransitOperatorV2DeleteEmployee =
  ( "employees" :> Capture "employeeId" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Delete
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

type TransitOperatorV2DeleteEmployeeHelper =
  ( "employees" :> Capture "employeeId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Delete ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type TransitOperatorV2BulkEmployees =
  ( "employees" :> "bulk" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.BulkEmployeesReq
      :> Post ('[JSON]) Lib.BmsBackend.Transit.Common.BulkResult
  )

type TransitOperatorV2BulkEmployeesHelper =
  ( "employees" :> "bulk" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.BulkEmployeesReq
      :> Post
           ('[JSON])
           Lib.BmsBackend.Transit.Common.BulkResult
  )

type TransitOperatorV2EmployeeLogin =
  ( "employees" :> "login" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.EmployeeLoginReq
      :> Post ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.EmployeeLoginResp
  )

type TransitOperatorV2EmployeeLoginHelper =
  ( "employees" :> "login" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.EmployeeLoginReq
      :> Post
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.EmployeeLoginResp
  )

type TransitOperatorV2EmployeeVerify =
  ( "employees" :> "verify" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.EmployeeVerifyReq
      :> Post ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.EmployeeVerifyResp
  )

type TransitOperatorV2EmployeeVerifyHelper =
  ( "employees" :> "verify" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.EmployeeVerifyReq
      :> Post
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.EmployeeVerifyResp
  )

type TransitOperatorV2ListDesignations =
  ( "designations" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           [Lib.BmsBackend.Transit.Masters.Types.Designation]
  )

type TransitOperatorV2ListDesignationsHelper =
  ( "designations" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) [Lib.BmsBackend.Transit.Masters.Types.Designation]
  )

type TransitOperatorV2ListCrew =
  ( "crew" :> QueryParam "role" Kernel.Prelude.Text :> QueryParam "search" Kernel.Prelude.Text :> QueryParam "depotId" Kernel.Prelude.Text
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.CrewMemberList
  )

type TransitOperatorV2ListCrewHelper =
  ( "crew" :> QueryParam "role" Kernel.Prelude.Text :> QueryParam "search" Kernel.Prelude.Text :> QueryParam "depotId" Kernel.Prelude.Text
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.CrewMemberList
  )

type TransitOperatorV2GetLookups = ("lookups" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.Lookups)

type TransitOperatorV2GetLookupsHelper =
  ( "lookups" :> QueryParam "requestorId" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.Lookups
  )

type TransitOperatorV2ListServiceTypes =
  ( "serviceTypes" :> QueryParam "search" Kernel.Prelude.Text :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.ServiceTypeList
  )

type TransitOperatorV2ListServiceTypesHelper =
  ( "serviceTypes" :> QueryParam "search" Kernel.Prelude.Text :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.ServiceTypeList
  )

type TransitOperatorV2GetServiceType =
  ( "serviceTypes" :> Capture "serviceTypeId" Kernel.Prelude.Text :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.ServiceType
  )

type TransitOperatorV2GetServiceTypeHelper =
  ( "serviceTypes" :> Capture "serviceTypeId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.ServiceType
  )

type TransitOperatorV2CreateServiceType =
  ( "serviceTypes" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.CreateServiceTypeReq
      :> Post ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.ServiceType
  )

type TransitOperatorV2CreateServiceTypeHelper =
  ( "serviceTypes" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.CreateServiceTypeReq
      :> Post
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.ServiceType
  )

type TransitOperatorV2UpdateServiceType =
  ( "serviceTypes" :> Capture "serviceTypeId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.UpdateServiceTypeReq
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.ServiceType
  )

type TransitOperatorV2UpdateServiceTypeHelper =
  ( "serviceTypes" :> Capture "serviceTypeId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.UpdateServiceTypeReq
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.ServiceType
  )

type TransitOperatorV2DeleteServiceType =
  ( "serviceTypes" :> Capture "serviceTypeId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Delete ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type TransitOperatorV2DeleteServiceTypeHelper =
  ( "serviceTypes" :> Capture "serviceTypeId" Kernel.Prelude.Text :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Delete ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type TransitOperatorV2ListStationEtas =
  ( "stationEtas" :> QueryParam "sourceStationCode" Kernel.Prelude.Text :> QueryParam "destinationStationCode" Kernel.Prelude.Text
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.StationEtaList
  )

type TransitOperatorV2ListStationEtasHelper =
  ( "stationEtas" :> QueryParam "sourceStationCode" Kernel.Prelude.Text :> QueryParam "destinationStationCode" Kernel.Prelude.Text
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.StationEtaList
  )

type TransitOperatorV2PutStationEtas =
  ( "stationEtas" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.PutStationEtasReq
      :> Put ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.StationEtaUpsertResult
  )

type TransitOperatorV2PutStationEtasHelper =
  ( "stationEtas" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> ReqBody ('[JSON]) Lib.BmsBackend.Transit.Masters.Types.PutStationEtasReq
      :> Put
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.StationEtaUpsertResult
  )

type TransitOperatorV2ListGtfsRoutes = ("gtfs" :> "routes" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory :> Get ('[JSON]) [GtfsRoute])

type TransitOperatorV2ListGtfsRoutesHelper =
  ( "gtfs" :> "routes" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) [GtfsRoute]
  )

type TransitOperatorV2ListGtfsRouteStops =
  ( "gtfs" :> "routes" :> Capture "routeId" Kernel.Prelude.Text :> "stops"
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) [GtfsRouteStop]
  )

type TransitOperatorV2ListGtfsRouteStopsHelper =
  ( "gtfs" :> "routes" :> Capture "routeId" Kernel.Prelude.Text :> "stops"
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> Get ('[JSON]) [GtfsRouteStop]
  )

type TransitOperatorV2SearchGtfsStops =
  ( "gtfs" :> "stops" :> "search" :> MandatoryQueryParam "vehicleCategory" BecknV2.OnDemand.Enums.VehicleCategory
      :> MandatoryQueryParam
           "q"
           Kernel.Prelude.Text
      :> Get ('[JSON]) [GtfsStop]
  )

type TransitOperatorV2SearchGtfsStopsHelper =
  ( "gtfs" :> "stops" :> "search" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> MandatoryQueryParam
           "vehicleCategory"
           BecknV2.OnDemand.Enums.VehicleCategory
      :> MandatoryQueryParam "q" Kernel.Prelude.Text
      :> Get ('[JSON]) [GtfsStop]
  )

type TransitOperatorV2ListPersons =
  ( "persons" :> QueryParam "search" Kernel.Prelude.Text :> QueryParam "organizationId" Kernel.Prelude.Text
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> Get ('[JSON]) TransitPersonList
  )

type TransitOperatorV2ListPersonsHelper =
  ( "persons" :> QueryParam "search" Kernel.Prelude.Text :> QueryParam "organizationId" Kernel.Prelude.Text
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam "requestorId" Kernel.Prelude.Text
      :> Get
           ('[JSON])
           TransitPersonList
  )

type TransitOperatorV2AssignPersonOrganization =
  ( "persons" :> Capture "personId" Kernel.Prelude.Text :> "organization"
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.AssignPersonOrganizationReq
      :> Put ('[JSON]) Kernel.Types.APISuccess.APISuccess
  )

type TransitOperatorV2AssignPersonOrganizationHelper =
  ( "persons" :> Capture "personId" Kernel.Prelude.Text :> "organization" :> QueryParam "requestorId" Kernel.Prelude.Text
      :> ReqBody
           ('[JSON])
           Lib.BmsBackend.Transit.Masters.Types.AssignPersonOrganizationReq
      :> Put
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

data TransitOperatorV2MastersAPIs = TransitOperatorV2MastersAPIs
  { transitOperatorV2ListOrganizations :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Masters.Types.OrganizationList),
    transitOperatorV2GetOrganization :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Masters.Types.Organization),
    transitOperatorV2CreateOrganization :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.CreateOrganizationReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Masters.Types.Organization),
    transitOperatorV2UpdateOrganization :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.UpdateOrganizationReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Masters.Types.Organization),
    transitOperatorV2DeleteOrganization :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    transitOperatorV2ListDepots :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Masters.Types.DepotList),
    transitOperatorV2GetDepot :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Masters.Types.Depot),
    transitOperatorV2CreateDepot :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.CreateDepotReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Masters.Types.Depot),
    transitOperatorV2UpdateDepot :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.UpdateDepotReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Masters.Types.Depot),
    transitOperatorV2DeleteDepot :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    transitOperatorV2ListVehicles :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Masters.Types.VehicleList),
    transitOperatorV2GetVehicle :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Masters.Types.Vehicle),
    transitOperatorV2CreateVehicle :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.CreateVehicleReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Masters.Types.Vehicle),
    transitOperatorV2UpdateVehicle :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.UpdateVehicleReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Masters.Types.Vehicle),
    transitOperatorV2DeleteVehicle :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    transitOperatorV2BulkVehicles :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.BulkVehiclesReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Common.BulkResult),
    transitOperatorV2ListDevices :: (Kernel.Prelude.Maybe (Lib.BmsBackend.Transit.Masters.Types.DeviceType) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Masters.Types.DeviceList),
    transitOperatorV2BulkDevices :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.BulkDevicesReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Common.BulkResult),
    transitOperatorV2DeleteDevice :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.DeviceType -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    transitOperatorV2ListEmployees :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Masters.Types.EmployeeList),
    transitOperatorV2GetEmployee :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Masters.Types.Employee),
    transitOperatorV2CreateEmployee :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.CreateEmployeeReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Masters.Types.Employee),
    transitOperatorV2UpdateEmployee :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.UpdateEmployeeReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Masters.Types.Employee),
    transitOperatorV2DeleteEmployee :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    transitOperatorV2BulkEmployees :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.BulkEmployeesReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Common.BulkResult),
    transitOperatorV2EmployeeLogin :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.EmployeeLoginReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Masters.Types.EmployeeLoginResp),
    transitOperatorV2EmployeeVerify :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.EmployeeVerifyReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Masters.Types.EmployeeVerifyResp),
    transitOperatorV2ListDesignations :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [Lib.BmsBackend.Transit.Masters.Types.Designation]),
    transitOperatorV2ListCrew :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Masters.Types.CrewMemberList),
    transitOperatorV2GetLookups :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Masters.Types.Lookups),
    transitOperatorV2ListServiceTypes :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Masters.Types.ServiceTypeList),
    transitOperatorV2GetServiceType :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Masters.Types.ServiceType),
    transitOperatorV2CreateServiceType :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.CreateServiceTypeReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Masters.Types.ServiceType),
    transitOperatorV2UpdateServiceType :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.UpdateServiceTypeReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Masters.Types.ServiceType),
    transitOperatorV2DeleteServiceType :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess),
    transitOperatorV2ListStationEtas :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Masters.Types.StationEtaList),
    transitOperatorV2PutStationEtas :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.PutStationEtasReq -> EulerHS.Types.EulerClient Lib.BmsBackend.Transit.Masters.Types.StationEtaUpsertResult),
    transitOperatorV2ListGtfsRoutes :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [GtfsRoute]),
    transitOperatorV2ListGtfsRouteStops :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> EulerHS.Types.EulerClient [GtfsRouteStop]),
    transitOperatorV2SearchGtfsStops :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Kernel.Prelude.Text -> EulerHS.Types.EulerClient [GtfsStop]),
    transitOperatorV2ListPersons :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> EulerHS.Types.EulerClient TransitPersonList),
    transitOperatorV2AssignPersonOrganization :: (Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Lib.BmsBackend.Transit.Masters.Types.AssignPersonOrganizationReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)
  }

mkTransitOperatorV2MastersAPIs :: (Client EulerHS.Types.EulerClient API -> TransitOperatorV2MastersAPIs)
mkTransitOperatorV2MastersAPIs transitOperatorV2MastersClient = (TransitOperatorV2MastersAPIs {..})
  where
    transitOperatorV2ListOrganizations :<|> transitOperatorV2GetOrganization :<|> transitOperatorV2CreateOrganization :<|> transitOperatorV2UpdateOrganization :<|> transitOperatorV2DeleteOrganization :<|> transitOperatorV2ListDepots :<|> transitOperatorV2GetDepot :<|> transitOperatorV2CreateDepot :<|> transitOperatorV2UpdateDepot :<|> transitOperatorV2DeleteDepot :<|> transitOperatorV2ListVehicles :<|> transitOperatorV2GetVehicle :<|> transitOperatorV2CreateVehicle :<|> transitOperatorV2UpdateVehicle :<|> transitOperatorV2DeleteVehicle :<|> transitOperatorV2BulkVehicles :<|> transitOperatorV2ListDevices :<|> transitOperatorV2BulkDevices :<|> transitOperatorV2DeleteDevice :<|> transitOperatorV2ListEmployees :<|> transitOperatorV2GetEmployee :<|> transitOperatorV2CreateEmployee :<|> transitOperatorV2UpdateEmployee :<|> transitOperatorV2DeleteEmployee :<|> transitOperatorV2BulkEmployees :<|> transitOperatorV2EmployeeLogin :<|> transitOperatorV2EmployeeVerify :<|> transitOperatorV2ListDesignations :<|> transitOperatorV2ListCrew :<|> transitOperatorV2GetLookups :<|> transitOperatorV2ListServiceTypes :<|> transitOperatorV2GetServiceType :<|> transitOperatorV2CreateServiceType :<|> transitOperatorV2UpdateServiceType :<|> transitOperatorV2DeleteServiceType :<|> transitOperatorV2ListStationEtas :<|> transitOperatorV2PutStationEtas :<|> transitOperatorV2ListGtfsRoutes :<|> transitOperatorV2ListGtfsRouteStops :<|> transitOperatorV2SearchGtfsStops :<|> transitOperatorV2ListPersons :<|> transitOperatorV2AssignPersonOrganization = transitOperatorV2MastersClient

data TransitOperatorV2MastersUserActionType
  = TRANSIT_OPERATOR_V2_LIST_ORGANIZATIONS
  | TRANSIT_OPERATOR_V2_GET_ORGANIZATION
  | TRANSIT_OPERATOR_V2_CREATE_ORGANIZATION
  | TRANSIT_OPERATOR_V2_UPDATE_ORGANIZATION
  | TRANSIT_OPERATOR_V2_DELETE_ORGANIZATION
  | TRANSIT_OPERATOR_V2_LIST_DEPOTS
  | TRANSIT_OPERATOR_V2_GET_DEPOT
  | TRANSIT_OPERATOR_V2_CREATE_DEPOT
  | TRANSIT_OPERATOR_V2_UPDATE_DEPOT
  | TRANSIT_OPERATOR_V2_DELETE_DEPOT
  | TRANSIT_OPERATOR_V2_LIST_VEHICLES
  | TRANSIT_OPERATOR_V2_GET_VEHICLE
  | TRANSIT_OPERATOR_V2_CREATE_VEHICLE
  | TRANSIT_OPERATOR_V2_UPDATE_VEHICLE
  | TRANSIT_OPERATOR_V2_DELETE_VEHICLE
  | TRANSIT_OPERATOR_V2_BULK_VEHICLES
  | TRANSIT_OPERATOR_V2_LIST_DEVICES
  | TRANSIT_OPERATOR_V2_BULK_DEVICES
  | TRANSIT_OPERATOR_V2_DELETE_DEVICE
  | TRANSIT_OPERATOR_V2_LIST_EMPLOYEES
  | TRANSIT_OPERATOR_V2_GET_EMPLOYEE
  | TRANSIT_OPERATOR_V2_CREATE_EMPLOYEE
  | TRANSIT_OPERATOR_V2_UPDATE_EMPLOYEE
  | TRANSIT_OPERATOR_V2_DELETE_EMPLOYEE
  | TRANSIT_OPERATOR_V2_BULK_EMPLOYEES
  | TRANSIT_OPERATOR_V2_EMPLOYEE_LOGIN
  | TRANSIT_OPERATOR_V2_EMPLOYEE_VERIFY
  | TRANSIT_OPERATOR_V2_LIST_DESIGNATIONS
  | TRANSIT_OPERATOR_V2_LIST_CREW
  | TRANSIT_OPERATOR_V2_GET_LOOKUPS
  | TRANSIT_OPERATOR_V2_LIST_SERVICE_TYPES
  | TRANSIT_OPERATOR_V2_GET_SERVICE_TYPE
  | TRANSIT_OPERATOR_V2_CREATE_SERVICE_TYPE
  | TRANSIT_OPERATOR_V2_UPDATE_SERVICE_TYPE
  | TRANSIT_OPERATOR_V2_DELETE_SERVICE_TYPE
  | TRANSIT_OPERATOR_V2_LIST_STATION_ETAS
  | TRANSIT_OPERATOR_V2_PUT_STATION_ETAS
  | TRANSIT_OPERATOR_V2_LIST_GTFS_ROUTES
  | TRANSIT_OPERATOR_V2_LIST_GTFS_ROUTE_STOPS
  | TRANSIT_OPERATOR_V2_SEARCH_GTFS_STOPS
  | TRANSIT_OPERATOR_V2_LIST_PERSONS
  | TRANSIT_OPERATOR_V2_ASSIGN_PERSON_ORGANIZATION
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''TransitOperatorV2MastersUserActionType)])
