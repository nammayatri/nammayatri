{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters
  ( transitOperatorV2ListOrganizations,
    transitOperatorV2GetOrganization,
    transitOperatorV2CreateOrganization,
    transitOperatorV2UpdateOrganization,
    transitOperatorV2DeleteOrganization,
    transitOperatorV2ListDepots,
    transitOperatorV2GetDepot,
    transitOperatorV2CreateDepot,
    transitOperatorV2UpdateDepot,
    transitOperatorV2DeleteDepot,
    transitOperatorV2ListVehicles,
    transitOperatorV2GetVehicle,
    transitOperatorV2CreateVehicle,
    transitOperatorV2UpdateVehicle,
    transitOperatorV2DeleteVehicle,
    transitOperatorV2BulkVehicles,
    transitOperatorV2ListDevices,
    transitOperatorV2BulkDevices,
    transitOperatorV2DeleteDevice,
    transitOperatorV2ListEmployees,
    transitOperatorV2GetEmployee,
    transitOperatorV2CreateEmployee,
    transitOperatorV2UpdateEmployee,
    transitOperatorV2DeleteEmployee,
    transitOperatorV2BulkEmployees,
    transitOperatorV2EmployeeLogin,
    transitOperatorV2EmployeeVerify,
    transitOperatorV2ListDesignations,
    transitOperatorV2ListCrew,
    transitOperatorV2GetLookups,
    transitOperatorV2ListServiceTypes,
    transitOperatorV2GetServiceType,
    transitOperatorV2CreateServiceType,
    transitOperatorV2UpdateServiceType,
    transitOperatorV2DeleteServiceType,
    transitOperatorV2ListStationEtas,
    transitOperatorV2PutStationEtas,
    transitOperatorV2ListGtfsRoutes,
    transitOperatorV2ListGtfsRouteStops,
    transitOperatorV2SearchGtfsStops,
    transitOperatorV2AssignPersonOrganization,
    transitOperatorV2ListPersons,
  )
where

import qualified API.Types.Dashboard.AppManagement.TransitOperatorV2Masters as Types
import qualified "beckn-spec" BecknV2.OnDemand.Enums
import qualified Data.List as DL
import qualified Data.Text as T
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions (runInDashboardDb)
import Kernel.External.Encryption (decrypt, getDbHash)
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error (MerchantError (MerchantNotFound))
import Kernel.Types.Id (Id (..))
import qualified Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM)
import "bms-backend" Lib.BmsBackend.Transit.Common (TmsCaller (..), callTms, scoped)
import qualified "bms-backend" Lib.BmsBackend.Transit.Common
import qualified "bms-backend" Lib.BmsBackend.Transit.Masters.API as API
import qualified "bms-backend" Lib.BmsBackend.Transit.Masters.Types
import qualified SharedLogic.External.Nandi.Flow as Nandi
import qualified SharedLogic.External.Nandi.Types as NandiTypes
import SharedLogic.TransitOperatorV2
import qualified "lib-dashboard" Storage.Queries.Merchant as QDashboardMerchant
import qualified "lib-dashboard" Storage.Queries.Person as QDashboardPerson

transitOperatorV2ListOrganizations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.Masters.Types.OrganizationList)
transitOperatorV2ListOrganizations merchantShortId opCity search limit offset requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "listOrganizations" API.listOrganizationsAPI (API.listOrganizations search limit offset)

transitOperatorV2GetOrganization :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.Masters.Types.Organization)
transitOperatorV2GetOrganization merchantShortId opCity organizationId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "getOrganization" API.getOrganizationAPI (API.getOrganization organizationId)

transitOperatorV2CreateOrganization :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.CreateOrganizationReq -> Environment.Flow Lib.BmsBackend.Transit.Masters.Types.Organization)
transitOperatorV2CreateOrganization merchantShortId opCity requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "createOrganization" API.createOrganizationAPI (API.createOrganization req)

transitOperatorV2UpdateOrganization :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.UpdateOrganizationReq -> Environment.Flow Lib.BmsBackend.Transit.Masters.Types.Organization)
transitOperatorV2UpdateOrganization merchantShortId opCity organizationId requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "updateOrganization" API.updateOrganizationAPI (API.updateOrganization organizationId req)

transitOperatorV2DeleteOrganization :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
transitOperatorV2DeleteOrganization merchantShortId opCity organizationId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tmsUnit ctx "deleteOrganization" API.deleteOrganizationAPI (API.deleteOrganization organizationId)

transitOperatorV2ListDepots :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.Masters.Types.DepotList)
transitOperatorV2ListDepots merchantShortId opCity search organizationId limit offset requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "listDepots" API.listDepotsAPI (API.listDepots ctx.gtfsId search organizationId limit offset)

transitOperatorV2GetDepot :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.Masters.Types.Depot)
transitOperatorV2GetDepot merchantShortId opCity depotId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "getDepot" API.getDepotAPI (API.getDepot ctx.gtfsId depotId)

transitOperatorV2CreateDepot :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.CreateDepotReq -> Environment.Flow Lib.BmsBackend.Transit.Masters.Types.Depot)
transitOperatorV2CreateDepot merchantShortId opCity requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "createDepot" API.createDepotAPI (API.createDepot ctx.gtfsId req)

transitOperatorV2UpdateDepot :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.UpdateDepotReq -> Environment.Flow Lib.BmsBackend.Transit.Masters.Types.Depot)
transitOperatorV2UpdateDepot merchantShortId opCity depotId requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "updateDepot" API.updateDepotAPI (API.updateDepot ctx.gtfsId depotId req)

transitOperatorV2DeleteDepot :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
transitOperatorV2DeleteDepot merchantShortId opCity depotId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tmsUnit ctx "deleteDepot" API.deleteDepotAPI (API.deleteDepot ctx.gtfsId depotId)

transitOperatorV2ListVehicles :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.Masters.Types.VehicleList)
transitOperatorV2ListVehicles merchantShortId opCity search depotId serviceTypeId vehicleNo fleetNo tagNumber limit offset requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "listVehicles" API.listVehiclesAPI (API.listVehicles ctx.gtfsId search depotId serviceTypeId vehicleNo fleetNo tagNumber limit offset)

transitOperatorV2GetVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.Masters.Types.Vehicle)
transitOperatorV2GetVehicle merchantShortId opCity vehicleId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "getVehicle" API.getVehicleAPI (API.getVehicle ctx.gtfsId vehicleId)

transitOperatorV2CreateVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.CreateVehicleReq -> Environment.Flow Lib.BmsBackend.Transit.Masters.Types.Vehicle)
transitOperatorV2CreateVehicle merchantShortId opCity requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "createVehicle" API.createVehicleAPI (API.createVehicle ctx.gtfsId req)

transitOperatorV2UpdateVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.UpdateVehicleReq -> Environment.Flow Lib.BmsBackend.Transit.Masters.Types.Vehicle)
transitOperatorV2UpdateVehicle merchantShortId opCity vehicleId requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "updateVehicle" API.updateVehicleAPI (API.updateVehicle ctx.gtfsId vehicleId req)

transitOperatorV2DeleteVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
transitOperatorV2DeleteVehicle merchantShortId opCity vehicleId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tmsUnit ctx "deleteVehicle" API.deleteVehicleAPI (API.deleteVehicle ctx.gtfsId vehicleId)

transitOperatorV2BulkVehicles :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.BulkVehiclesReq -> Environment.Flow Lib.BmsBackend.Transit.Common.BulkResult)
transitOperatorV2BulkVehicles merchantShortId opCity requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "bulkVehicles" API.bulkVehiclesAPI (API.bulkVehicles ctx.gtfsId req)

transitOperatorV2ListDevices :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Lib.BmsBackend.Transit.Masters.Types.DeviceType) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.Masters.Types.DeviceList)
transitOperatorV2ListDevices merchantShortId opCity deviceType search limit offset requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "listDevices" API.listDevicesAPI (API.listDevices ctx.gtfsId deviceType search limit offset)

transitOperatorV2BulkDevices :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.BulkDevicesReq -> Environment.Flow Lib.BmsBackend.Transit.Common.BulkResult)
transitOperatorV2BulkDevices merchantShortId opCity requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "bulkDevices" API.bulkDevicesAPI (API.bulkDevices ctx.gtfsId req)

transitOperatorV2DeleteDevice :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.DeviceType -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
transitOperatorV2DeleteDevice merchantShortId opCity deviceId requestorId vehicleCategory deviceType = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tmsUnit ctx "deleteDevice" API.deleteDeviceAPI (API.deleteDevice ctx.gtfsId deviceId deviceType)

transitOperatorV2ListEmployees :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.Masters.Types.EmployeeList)
transitOperatorV2ListEmployees merchantShortId opCity role depotId search limit offset requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "listEmployees" API.listEmployeesAPI (API.listEmployees ctx.gtfsId role depotId search limit offset)

transitOperatorV2GetEmployee :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.Masters.Types.Employee)
transitOperatorV2GetEmployee merchantShortId opCity employeeId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "getEmployee" API.getEmployeeAPI (API.getEmployee ctx.gtfsId employeeId)

transitOperatorV2CreateEmployee :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.CreateEmployeeReq -> Environment.Flow Lib.BmsBackend.Transit.Masters.Types.Employee)
transitOperatorV2CreateEmployee merchantShortId opCity requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  hashedReq <- hashEmployeeSecrets req
  tms ctx "createEmployee" API.createEmployeeAPI (API.createEmployee ctx.gtfsId hashedReq)

transitOperatorV2UpdateEmployee :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.UpdateEmployeeReq -> Environment.Flow Lib.BmsBackend.Transit.Masters.Types.Employee)
transitOperatorV2UpdateEmployee merchantShortId opCity employeeId requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  hashedReq <- hashEmployeeSecrets req
  tms ctx "updateEmployee" API.updateEmployeeAPI (API.updateEmployee ctx.gtfsId employeeId hashedReq)

transitOperatorV2DeleteEmployee :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
transitOperatorV2DeleteEmployee merchantShortId opCity employeeId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tmsUnit ctx "deleteEmployee" API.deleteEmployeeAPI (API.deleteEmployee ctx.gtfsId employeeId)

transitOperatorV2BulkEmployees :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.BulkEmployeesReq -> Environment.Flow Lib.BmsBackend.Transit.Common.BulkResult)
transitOperatorV2BulkEmployees merchantShortId opCity requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  hashedEmployees <- traverse hashEmployeeSecrets req.employees
  tms ctx "bulkEmployees" API.bulkEmployeesAPI (API.bulkEmployees ctx.gtfsId (Kernel.Prelude.setField @"employees" req hashedEmployees))

transitOperatorV2EmployeeLogin :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.EmployeeLoginReq -> Environment.Flow Lib.BmsBackend.Transit.Masters.Types.EmployeeLoginResp)
transitOperatorV2EmployeeLogin merchantShortId opCity requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  hashedPassword <- hashSecret req.password_hash
  hashedEmail <- hashSecret req.email_hash
  tms ctx "employeeLogin" API.employeeLoginAPI (API.employeeLogin ctx.gtfsId (Kernel.Prelude.setField @"email_hash" (Kernel.Prelude.setField @"password_hash" req hashedPassword) hashedEmail))

transitOperatorV2EmployeeVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.EmployeeVerifyReq -> Environment.Flow Lib.BmsBackend.Transit.Masters.Types.EmployeeVerifyResp)
transitOperatorV2EmployeeVerify merchantShortId opCity requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "employeeVerify" API.employeeVerifyAPI (API.employeeVerify ctx.gtfsId req)

transitOperatorV2ListDesignations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [Lib.BmsBackend.Transit.Masters.Types.Designation])
transitOperatorV2ListDesignations merchantShortId opCity requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "listDesignations" API.listDesignationsAPI (API.listDesignations ctx.gtfsId)

transitOperatorV2ListCrew :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.Masters.Types.CrewMemberList)
transitOperatorV2ListCrew merchantShortId opCity role search depotId limit offset requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "listCrew" API.listCrewAPI (API.listCrew ctx.gtfsId role search depotId limit offset)

transitOperatorV2GetLookups :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.Masters.Types.Lookups)
transitOperatorV2GetLookups merchantShortId opCity requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "getLookups" API.getLookupsAPI (API.getLookups ctx.gtfsId)

transitOperatorV2ListServiceTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.Masters.Types.ServiceTypeList)
transitOperatorV2ListServiceTypes merchantShortId opCity search limit offset requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "listServiceTypes" API.listServiceTypesAPI (API.listServiceTypes ctx.gtfsId search limit offset)

transitOperatorV2GetServiceType :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.Masters.Types.ServiceType)
transitOperatorV2GetServiceType merchantShortId opCity serviceTypeId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "getServiceType" API.getServiceTypeAPI (API.getServiceType ctx.gtfsId serviceTypeId)

transitOperatorV2CreateServiceType :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.CreateServiceTypeReq -> Environment.Flow Lib.BmsBackend.Transit.Masters.Types.ServiceType)
transitOperatorV2CreateServiceType merchantShortId opCity requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "createServiceType" API.createServiceTypeAPI (API.createServiceType ctx.gtfsId req)

transitOperatorV2UpdateServiceType :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.UpdateServiceTypeReq -> Environment.Flow Lib.BmsBackend.Transit.Masters.Types.ServiceType)
transitOperatorV2UpdateServiceType merchantShortId opCity serviceTypeId requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "updateServiceType" API.updateServiceTypeAPI (API.updateServiceType ctx.gtfsId serviceTypeId req)

transitOperatorV2DeleteServiceType :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
transitOperatorV2DeleteServiceType merchantShortId opCity serviceTypeId requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tmsUnit ctx "deleteServiceType" API.deleteServiceTypeAPI (API.deleteServiceType ctx.gtfsId serviceTypeId)

transitOperatorV2ListStationEtas :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow Lib.BmsBackend.Transit.Masters.Types.StationEtaList)
transitOperatorV2ListStationEtas merchantShortId opCity sourceStationCode destinationStationCode limit offset requestorId vehicleCategory = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "listStationEtas" API.listStationEtasAPI (API.listStationEtas ctx.gtfsId sourceStationCode destinationStationCode limit offset)

transitOperatorV2PutStationEtas :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.PutStationEtasReq -> Environment.Flow Lib.BmsBackend.Transit.Masters.Types.StationEtaUpsertResult)
transitOperatorV2PutStationEtas merchantShortId opCity requestorId vehicleCategory req = do
  ctx <- transitCtx merchantShortId opCity vehicleCategory requestorId
  tms ctx "putStationEtas" API.putStationEtasAPI (API.putStationEtas ctx.gtfsId req)

transitOperatorV2ListGtfsRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [Types.GtfsRoute])
transitOperatorV2ListGtfsRoutes merchantShortId opCity requestorId vehicleCategory = do
  (baseUrl, gtfsId) <- transitGims merchantShortId opCity vehicleCategory requestorId
  DL.sortOn (\route -> (route.number, route.id)) . map toGtfsRoute <$> Nandi.getRoutesByGtfsId baseUrl gtfsId

transitOperatorV2ListGtfsRouteStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.Flow [Types.GtfsRouteStop])
transitOperatorV2ListGtfsRouteStops merchantShortId opCity routeId requestorId vehicleCategory = do
  (baseUrl, gtfsId) <- transitGims merchantShortId opCity vehicleCategory requestorId
  DL.sortOn (.sequenceNum) . map toGtfsRouteStop <$> Nandi.getRouteStopMappingByRouteCode baseUrl gtfsId routeId

transitOperatorV2SearchGtfsStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> BecknV2.OnDemand.Enums.VehicleCategory -> Kernel.Prelude.Text -> Environment.Flow [Types.GtfsStop])
transitOperatorV2SearchGtfsStops merchantShortId opCity requestorId vehicleCategory q = do
  (baseUrl, gtfsId) <- transitGims merchantShortId opCity vehicleCategory requestorId
  if T.null (T.strip q)
    then pure []
    else toGtfsStops <$> Nandi.getStationsByGtfsIdFuzzySearch baseUrl gtfsId (T.strip q)

toGtfsRoute :: NandiTypes.RouteInfoNandi -> Types.GtfsRoute
toGtfsRoute route =
  Types.GtfsRoute
    { id = route.id,
      number = route.shortName,
      name = route.longName,
      mode = Just (show route.mode),
      tripCount = route.tripCount,
      stopCount = route.stopCount
    }

toGtfsRouteStop :: NandiTypes.RouteStopMappingInMemoryServer -> Types.GtfsRouteStop
toGtfsRouteStop stop =
  Types.GtfsRouteStop
    { sequenceNum = stop.sequenceNum,
      stopCode = stop.stopCode,
      name = Just stop.stopName,
      lat = stop.stopPoint.lat,
      lon = stop.stopPoint.lon,
      stageNumber = stop.stageNumber,
      isStageStop = fromMaybe False stop.isStageStop
    }

toGtfsStops :: [NandiTypes.RouteStopMappingInMemoryServer] -> [Types.GtfsStop]
toGtfsStops rows =
  [ Types.GtfsStop
      { code = stop.stopCode,
        name = Just stop.stopName,
        lat = stop.stopPoint.lat,
        lon = stop.stopPoint.lon,
        routeCodes = DL.nub [row.routeCode | row <- rows, row.stopCode == stop.stopCode]
      }
    | stop <- take 50 (DL.nubBy (\a b -> a.stopCode == b.stopCode) rows)
  ]

transitOperatorV2AssignPersonOrganization :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Lib.BmsBackend.Transit.Masters.Types.AssignPersonOrganizationReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
transitOperatorV2AssignPersonOrganization merchantShortId opCity personId mbRequestorId req = do
  requestorId <- requireRequestorId mbRequestorId
  merchantOpCity <- findMerchantOpCity merchantShortId opCity
  void $ findDashboardPerson personId
  whenJust req.organizationId $ \organizationId -> do
    cfg <- tmsConfig merchantOpCity.merchantId merchantOpCity.id
    let caller = TmsCaller {orgId = "*", actorPersonId = requestorId}
    void $ callTms cfg "getOrganization" API.getOrganizationAPI (scoped caller (API.getOrganization organizationId))
  runInDashboardDb (QDashboardPerson.updatePersonOrganizationId (Id personId) req.organizationId)
  pure Success

hashEmployeeSecrets :: (Kernel.Prelude.HasField "passwordHash" r (Maybe Text), Kernel.Prelude.HasField "emailHash" r (Maybe Text)) => r -> Environment.Flow r
hashEmployeeSecrets req = do
  hashedPassword <- hashSecret req.passwordHash
  hashedEmail <- hashSecret req.emailHash
  pure $ Kernel.Prelude.setField @"emailHash" (Kernel.Prelude.setField @"passwordHash" req hashedPassword) hashedEmail

transitOperatorV2ListPersons :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow Types.TransitPersonList)
transitOperatorV2ListPersons merchantShortId opCity search organizationId limit offset mbRequestorId = do
  void $ requireRequestorId mbRequestorId
  let shortId = Kernel.Types.Id.getShortId merchantShortId
  merchant <- runInDashboardDb (QDashboardMerchant.findByShortId (Kernel.Types.Id.ShortId shortId)) >>= fromMaybeM (MerchantNotFound shortId)
  let searchText = search >>= nonBlank
  searchHash <- traverse getDbHash searchText
  (persons, total) <-
    runInDashboardDb $
      QDashboardPerson.findAllForTransitOrganization
        merchant.id
        opCity
        searchText
        searchHash
        (organizationId >>= nonBlank)
        (fromIntegral (clampLimit limit))
        (fromIntegral (max 0 (fromMaybe 0 offset)))
  items <- forM persons $ \person -> do
    p <- decrypt person
    pure
      Types.TransitPerson
        { id = Kernel.Types.Id.getId p.id,
          firstName = p.firstName,
          lastName = p.lastName,
          email = p.email,
          mobileNumber = p.mobileNumber,
          mobileCountryCode = p.mobileCountryCode,
          organizationId = p.organizationId
        }
  pure Types.TransitPersonList {items, total}
  where
    nonBlank t = let trimmed = T.strip t in if trimmed == "" then Nothing else Just trimmed
    clampLimit = max 1 . min 100 . fromMaybe 20
