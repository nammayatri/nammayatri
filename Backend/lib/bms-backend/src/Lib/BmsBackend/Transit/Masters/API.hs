module Lib.BmsBackend.Transit.Masters.API where

import qualified EulerHS.Types as ET
import Kernel.Prelude
import Lib.BmsBackend.Transit.Common
import Lib.BmsBackend.Transit.Masters.Types
import Servant

type ListOrganizationsAPI =
  "transit"
    :> "organizations"
    :> QueryParam "search" Text
    :> QueryParam "limit" Int
    :> QueryParam "offset" Int
    :> Scoped (Get '[JSON] (ListResponse Organization))

listOrganizationsAPI :: Proxy ListOrganizationsAPI
listOrganizationsAPI = Proxy

listOrganizations :: Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> ET.EulerClient (ListResponse Organization)
listOrganizations = ET.client listOrganizationsAPI

type GetOrganizationAPI =
  "transit"
    :> "organizations"
    :> Capture "organizationId" Text
    :> Scoped (Get '[JSON] Organization)

getOrganizationAPI :: Proxy GetOrganizationAPI
getOrganizationAPI = Proxy

getOrganization :: Text -> Maybe Text -> Maybe Text -> ET.EulerClient Organization
getOrganization = ET.client getOrganizationAPI

type CreateOrganizationAPI =
  "transit"
    :> "organizations"
    :> ReqBody '[JSON] CreateOrganizationReq
    :> Scoped (Post '[JSON] Organization)

createOrganizationAPI :: Proxy CreateOrganizationAPI
createOrganizationAPI = Proxy

createOrganization :: CreateOrganizationReq -> Maybe Text -> Maybe Text -> ET.EulerClient Organization
createOrganization = ET.client createOrganizationAPI

type UpdateOrganizationAPI =
  "transit"
    :> "organizations"
    :> Capture "organizationId" Text
    :> ReqBody '[JSON] UpdateOrganizationReq
    :> Scoped (Patch '[JSON] Organization)

updateOrganizationAPI :: Proxy UpdateOrganizationAPI
updateOrganizationAPI = Proxy

updateOrganization :: Text -> UpdateOrganizationReq -> Maybe Text -> Maybe Text -> ET.EulerClient Organization
updateOrganization = ET.client updateOrganizationAPI

type DeleteOrganizationAPI =
  "transit"
    :> "organizations"
    :> Capture "organizationId" Text
    :> Scoped (Verb 'DELETE 204 '[JSON] NoContent)

deleteOrganizationAPI :: Proxy DeleteOrganizationAPI
deleteOrganizationAPI = Proxy

deleteOrganization :: Text -> Maybe Text -> Maybe Text -> ET.EulerClient NoContent
deleteOrganization = ET.client deleteOrganizationAPI

type ListDepotsAPI =
  Transit ("depots" :> QueryParam "search" Text :> QueryParam "organizationId" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Scoped (Get '[JSON] (ListResponse Depot)))

listDepotsAPI :: Proxy ListDepotsAPI
listDepotsAPI = Proxy

listDepots :: Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> ET.EulerClient (ListResponse Depot)
listDepots = ET.client listDepotsAPI

type GetDepotAPI =
  Transit ("depots" :> Capture "depotId" Text :> Scoped (Get '[JSON] Depot))

getDepotAPI :: Proxy GetDepotAPI
getDepotAPI = Proxy

getDepot :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient Depot
getDepot = ET.client getDepotAPI

type CreateDepotAPI =
  Transit ("depots" :> ReqBody '[JSON] CreateDepotReq :> Scoped (Post '[JSON] Depot))

createDepotAPI :: Proxy CreateDepotAPI
createDepotAPI = Proxy

createDepot :: Text -> CreateDepotReq -> Maybe Text -> Maybe Text -> ET.EulerClient Depot
createDepot = ET.client createDepotAPI

type UpdateDepotAPI =
  Transit ("depots" :> Capture "depotId" Text :> ReqBody '[JSON] UpdateDepotReq :> Scoped (Patch '[JSON] Depot))

updateDepotAPI :: Proxy UpdateDepotAPI
updateDepotAPI = Proxy

updateDepot :: Text -> Text -> UpdateDepotReq -> Maybe Text -> Maybe Text -> ET.EulerClient Depot
updateDepot = ET.client updateDepotAPI

type DeleteDepotAPI =
  Transit ("depots" :> Capture "depotId" Text :> Scoped (Verb 'DELETE 204 '[JSON] NoContent))

deleteDepotAPI :: Proxy DeleteDepotAPI
deleteDepotAPI = Proxy

deleteDepot :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient NoContent
deleteDepot = ET.client deleteDepotAPI

type ListVehiclesAPI =
  Transit ("vehicles" :> QueryParam "search" Text :> QueryParam "depotId" Text :> QueryParam "serviceTypeId" Text :> QueryParam "vehicleNo" Text :> QueryParam "fleetNo" Text :> QueryParam "tagNumber" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Scoped (Get '[JSON] (ListResponse Vehicle)))

listVehiclesAPI :: Proxy ListVehiclesAPI
listVehiclesAPI = Proxy

listVehicles :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> ET.EulerClient (ListResponse Vehicle)
listVehicles = ET.client listVehiclesAPI

type GetVehicleAPI =
  Transit ("vehicles" :> Capture "vehicleId" Text :> Scoped (Get '[JSON] Vehicle))

getVehicleAPI :: Proxy GetVehicleAPI
getVehicleAPI = Proxy

getVehicle :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient Vehicle
getVehicle = ET.client getVehicleAPI

type CreateVehicleAPI =
  Transit ("vehicles" :> ReqBody '[JSON] CreateVehicleReq :> Scoped (Post '[JSON] Vehicle))

createVehicleAPI :: Proxy CreateVehicleAPI
createVehicleAPI = Proxy

createVehicle :: Text -> CreateVehicleReq -> Maybe Text -> Maybe Text -> ET.EulerClient Vehicle
createVehicle = ET.client createVehicleAPI

type UpdateVehicleAPI =
  Transit ("vehicles" :> Capture "vehicleId" Text :> ReqBody '[JSON] UpdateVehicleReq :> Scoped (Patch '[JSON] Vehicle))

updateVehicleAPI :: Proxy UpdateVehicleAPI
updateVehicleAPI = Proxy

updateVehicle :: Text -> Text -> UpdateVehicleReq -> Maybe Text -> Maybe Text -> ET.EulerClient Vehicle
updateVehicle = ET.client updateVehicleAPI

type DeleteVehicleAPI =
  Transit ("vehicles" :> Capture "vehicleId" Text :> Scoped (Verb 'DELETE 204 '[JSON] NoContent))

deleteVehicleAPI :: Proxy DeleteVehicleAPI
deleteVehicleAPI = Proxy

deleteVehicle :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient NoContent
deleteVehicle = ET.client deleteVehicleAPI

type BulkVehiclesAPI =
  Transit ("vehicles" :> "bulk" :> ReqBody '[JSON] BulkVehiclesReq :> Scoped (Post '[JSON] BulkResult))

bulkVehiclesAPI :: Proxy BulkVehiclesAPI
bulkVehiclesAPI = Proxy

bulkVehicles :: Text -> BulkVehiclesReq -> Maybe Text -> Maybe Text -> ET.EulerClient BulkResult
bulkVehicles = ET.client bulkVehiclesAPI

type ListDevicesAPI =
  Transit ("devices" :> QueryParam "type" DeviceType :> QueryParam "search" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Scoped (Get '[JSON] (ListResponse Device)))

listDevicesAPI :: Proxy ListDevicesAPI
listDevicesAPI = Proxy

listDevices :: Text -> Maybe DeviceType -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> ET.EulerClient (ListResponse Device)
listDevices = ET.client listDevicesAPI

type BulkDevicesAPI =
  Transit ("devices" :> "bulk" :> ReqBody '[JSON] BulkDevicesReq :> Scoped (Post '[JSON] BulkResult))

bulkDevicesAPI :: Proxy BulkDevicesAPI
bulkDevicesAPI = Proxy

bulkDevices :: Text -> BulkDevicesReq -> Maybe Text -> Maybe Text -> ET.EulerClient BulkResult
bulkDevices = ET.client bulkDevicesAPI

type DeleteDeviceAPI =
  Transit ("devices" :> Capture "deviceId" Text :> QueryParam' '[Required, Strict] "type" DeviceType :> Scoped (Verb 'DELETE 204 '[JSON] NoContent))

deleteDeviceAPI :: Proxy DeleteDeviceAPI
deleteDeviceAPI = Proxy

deleteDevice :: Text -> Text -> DeviceType -> Maybe Text -> Maybe Text -> ET.EulerClient NoContent
deleteDevice = ET.client deleteDeviceAPI

type ListEmployeesAPI =
  Transit ("employees" :> QueryParam "role" Text :> QueryParam "depotId" Text :> QueryParam "search" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Scoped (Get '[JSON] (ListResponse Employee)))

listEmployeesAPI :: Proxy ListEmployeesAPI
listEmployeesAPI = Proxy

listEmployees :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> ET.EulerClient (ListResponse Employee)
listEmployees = ET.client listEmployeesAPI

type GetEmployeeAPI =
  Transit ("employees" :> Capture "employeeId" Text :> Scoped (Get '[JSON] Employee))

getEmployeeAPI :: Proxy GetEmployeeAPI
getEmployeeAPI = Proxy

getEmployee :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient Employee
getEmployee = ET.client getEmployeeAPI

type CreateEmployeeAPI =
  Transit ("employees" :> ReqBody '[JSON] CreateEmployeeReq :> Scoped (Post '[JSON] Employee))

createEmployeeAPI :: Proxy CreateEmployeeAPI
createEmployeeAPI = Proxy

createEmployee :: Text -> CreateEmployeeReq -> Maybe Text -> Maybe Text -> ET.EulerClient Employee
createEmployee = ET.client createEmployeeAPI

type UpdateEmployeeAPI =
  Transit ("employees" :> Capture "employeeId" Text :> ReqBody '[JSON] UpdateEmployeeReq :> Scoped (Patch '[JSON] Employee))

updateEmployeeAPI :: Proxy UpdateEmployeeAPI
updateEmployeeAPI = Proxy

updateEmployee :: Text -> Text -> UpdateEmployeeReq -> Maybe Text -> Maybe Text -> ET.EulerClient Employee
updateEmployee = ET.client updateEmployeeAPI

type DeleteEmployeeAPI =
  Transit ("employees" :> Capture "employeeId" Text :> Scoped (Verb 'DELETE 204 '[JSON] NoContent))

deleteEmployeeAPI :: Proxy DeleteEmployeeAPI
deleteEmployeeAPI = Proxy

deleteEmployee :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient NoContent
deleteEmployee = ET.client deleteEmployeeAPI

type BulkEmployeesAPI =
  Transit ("employees" :> "bulk" :> ReqBody '[JSON] BulkEmployeesReq :> Scoped (Post '[JSON] BulkResult))

bulkEmployeesAPI :: Proxy BulkEmployeesAPI
bulkEmployeesAPI = Proxy

bulkEmployees :: Text -> BulkEmployeesReq -> Maybe Text -> Maybe Text -> ET.EulerClient BulkResult
bulkEmployees = ET.client bulkEmployeesAPI

type EmployeeLoginAPI =
  Transit ("employees" :> "login" :> ReqBody '[JSON] EmployeeLoginReq :> Scoped (Post '[JSON] EmployeeLoginResp))

employeeLoginAPI :: Proxy EmployeeLoginAPI
employeeLoginAPI = Proxy

employeeLogin :: Text -> EmployeeLoginReq -> Maybe Text -> Maybe Text -> ET.EulerClient EmployeeLoginResp
employeeLogin = ET.client employeeLoginAPI

type EmployeeVerifyAPI =
  Transit ("employees" :> "verify" :> ReqBody '[JSON] EmployeeVerifyReq :> Scoped (Post '[JSON] EmployeeVerifyResp))

employeeVerifyAPI :: Proxy EmployeeVerifyAPI
employeeVerifyAPI = Proxy

employeeVerify :: Text -> EmployeeVerifyReq -> Maybe Text -> Maybe Text -> ET.EulerClient EmployeeVerifyResp
employeeVerify = ET.client employeeVerifyAPI

type ListDesignationsAPI =
  Transit ("designations" :> Scoped (Get '[JSON] [Designation]))

listDesignationsAPI :: Proxy ListDesignationsAPI
listDesignationsAPI = Proxy

listDesignations :: Text -> Maybe Text -> Maybe Text -> ET.EulerClient [Designation]
listDesignations = ET.client listDesignationsAPI

type ListCrewAPI =
  Transit ("crew" :> QueryParam "role" Text :> QueryParam "search" Text :> QueryParam "depotId" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Scoped (Get '[JSON] (ListResponse CrewMember)))

listCrewAPI :: Proxy ListCrewAPI
listCrewAPI = Proxy

listCrew :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> ET.EulerClient (ListResponse CrewMember)
listCrew = ET.client listCrewAPI

type GetLookupsAPI =
  Transit ("lookups" :> Scoped (Get '[JSON] Lookups))

getLookupsAPI :: Proxy GetLookupsAPI
getLookupsAPI = Proxy

getLookups :: Text -> Maybe Text -> Maybe Text -> ET.EulerClient Lookups
getLookups = ET.client getLookupsAPI

type ListServiceTypesAPI =
  Transit ("service-types" :> QueryParam "search" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Scoped (Get '[JSON] (ListResponse ServiceType)))

listServiceTypesAPI :: Proxy ListServiceTypesAPI
listServiceTypesAPI = Proxy

listServiceTypes :: Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> ET.EulerClient (ListResponse ServiceType)
listServiceTypes = ET.client listServiceTypesAPI

type GetServiceTypeAPI =
  Transit ("service-types" :> Capture "serviceTypeId" Text :> Scoped (Get '[JSON] ServiceType))

getServiceTypeAPI :: Proxy GetServiceTypeAPI
getServiceTypeAPI = Proxy

getServiceType :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient ServiceType
getServiceType = ET.client getServiceTypeAPI

type CreateServiceTypeAPI =
  Transit ("service-types" :> ReqBody '[JSON] CreateServiceTypeReq :> Scoped (Post '[JSON] ServiceType))

createServiceTypeAPI :: Proxy CreateServiceTypeAPI
createServiceTypeAPI = Proxy

createServiceType :: Text -> CreateServiceTypeReq -> Maybe Text -> Maybe Text -> ET.EulerClient ServiceType
createServiceType = ET.client createServiceTypeAPI

type UpdateServiceTypeAPI =
  Transit ("service-types" :> Capture "serviceTypeId" Text :> ReqBody '[JSON] UpdateServiceTypeReq :> Scoped (Patch '[JSON] ServiceType))

updateServiceTypeAPI :: Proxy UpdateServiceTypeAPI
updateServiceTypeAPI = Proxy

updateServiceType :: Text -> Text -> UpdateServiceTypeReq -> Maybe Text -> Maybe Text -> ET.EulerClient ServiceType
updateServiceType = ET.client updateServiceTypeAPI

type DeleteServiceTypeAPI =
  Transit ("service-types" :> Capture "serviceTypeId" Text :> Scoped (Verb 'DELETE 204 '[JSON] NoContent))

deleteServiceTypeAPI :: Proxy DeleteServiceTypeAPI
deleteServiceTypeAPI = Proxy

deleteServiceType :: Text -> Text -> Maybe Text -> Maybe Text -> ET.EulerClient NoContent
deleteServiceType = ET.client deleteServiceTypeAPI

type ListStationEtasAPI =
  Transit ("station-etas" :> QueryParam "sourceStationCode" Text :> QueryParam "destinationStationCode" Text :> QueryParam "limit" Int :> QueryParam "offset" Int :> Scoped (Get '[JSON] (ListResponse StationEta)))

listStationEtasAPI :: Proxy ListStationEtasAPI
listStationEtasAPI = Proxy

listStationEtas :: Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> ET.EulerClient (ListResponse StationEta)
listStationEtas = ET.client listStationEtasAPI

type PutStationEtasAPI =
  Transit ("station-etas" :> ReqBody '[JSON] PutStationEtasReq :> Scoped (Put '[JSON] StationEtaUpsertResult))

putStationEtasAPI :: Proxy PutStationEtasAPI
putStationEtasAPI = Proxy

putStationEtas :: Text -> PutStationEtasReq -> Maybe Text -> Maybe Text -> ET.EulerClient StationEtaUpsertResult
putStationEtas = ET.client putStationEtasAPI
