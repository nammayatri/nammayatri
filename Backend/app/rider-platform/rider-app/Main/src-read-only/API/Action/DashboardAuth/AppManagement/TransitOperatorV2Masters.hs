{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.TransitOperatorV2Masters
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.TransitOperatorV2Masters
import qualified "beckn-spec" BecknV2.OnDemand.Enums
import qualified Control.Lens
import qualified Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified "bms-backend" Lib.BmsBackend.Transit.Common
import qualified "bms-backend" Lib.BmsBackend.Transit.Masters.Types
import Servant
import qualified Tools.ActorInfo
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("transitOperatorV2" :> (TransitOperatorV2ListOrganizations :<|> TransitOperatorV2GetOrganization :<|> TransitOperatorV2CreateOrganization :<|> TransitOperatorV2UpdateOrganization :<|> TransitOperatorV2DeleteOrganization :<|> TransitOperatorV2ListDepots :<|> TransitOperatorV2GetDepot :<|> TransitOperatorV2CreateDepot :<|> TransitOperatorV2UpdateDepot :<|> TransitOperatorV2DeleteDepot :<|> TransitOperatorV2ListVehicles :<|> TransitOperatorV2GetVehicle :<|> TransitOperatorV2CreateVehicle :<|> TransitOperatorV2UpdateVehicle :<|> TransitOperatorV2DeleteVehicle :<|> TransitOperatorV2BulkVehicles :<|> TransitOperatorV2ListDevices :<|> TransitOperatorV2BulkDevices :<|> TransitOperatorV2DeleteDevice :<|> TransitOperatorV2ListEmployees :<|> TransitOperatorV2GetEmployee :<|> TransitOperatorV2CreateEmployee :<|> TransitOperatorV2UpdateEmployee :<|> TransitOperatorV2DeleteEmployee :<|> TransitOperatorV2BulkEmployees :<|> TransitOperatorV2EmployeeLogin :<|> TransitOperatorV2EmployeeVerify :<|> TransitOperatorV2ListDesignations :<|> TransitOperatorV2ListCrew :<|> TransitOperatorV2GetLookups :<|> TransitOperatorV2ListServiceTypes :<|> TransitOperatorV2GetServiceType :<|> TransitOperatorV2CreateServiceType :<|> TransitOperatorV2UpdateServiceType :<|> TransitOperatorV2DeleteServiceType :<|> TransitOperatorV2ListStationEtas :<|> TransitOperatorV2PutStationEtas :<|> TransitOperatorV2ListGtfsRoutes :<|> TransitOperatorV2ListGtfsRouteStops :<|> TransitOperatorV2SearchGtfsStops :<|> TransitOperatorV2ListPersons :<|> TransitOperatorV2AssignPersonOrganization))

type TransitOperatorV2ListOrganizations =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_LIST_ORGANIZATIONS"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2ListOrganizations
  )

type TransitOperatorV2GetOrganization =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_GET_ORGANIZATION"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2GetOrganization
  )

type TransitOperatorV2CreateOrganization =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_CREATE_ORGANIZATION"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2CreateOrganization
  )

type TransitOperatorV2UpdateOrganization =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_UPDATE_ORGANIZATION"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2UpdateOrganization
  )

type TransitOperatorV2DeleteOrganization =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_DELETE_ORGANIZATION"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2DeleteOrganization
  )

type TransitOperatorV2ListDepots =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_LIST_DEPOTS"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2ListDepots
  )

type TransitOperatorV2GetDepot =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_GET_DEPOT"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2GetDepot
  )

type TransitOperatorV2CreateDepot =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_CREATE_DEPOT"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2CreateDepot
  )

type TransitOperatorV2UpdateDepot =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_UPDATE_DEPOT"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2UpdateDepot
  )

type TransitOperatorV2DeleteDepot =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_DELETE_DEPOT"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2DeleteDepot
  )

type TransitOperatorV2ListVehicles =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_LIST_VEHICLES"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2ListVehicles
  )

type TransitOperatorV2GetVehicle =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_GET_VEHICLE"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2GetVehicle
  )

type TransitOperatorV2CreateVehicle =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_CREATE_VEHICLE"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2CreateVehicle
  )

type TransitOperatorV2UpdateVehicle =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_UPDATE_VEHICLE"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2UpdateVehicle
  )

type TransitOperatorV2DeleteVehicle =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_DELETE_VEHICLE"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2DeleteVehicle
  )

type TransitOperatorV2BulkVehicles =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_BULK_VEHICLES"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2BulkVehicles
  )

type TransitOperatorV2ListDevices =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_LIST_DEVICES"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2ListDevices
  )

type TransitOperatorV2BulkDevices =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_BULK_DEVICES"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2BulkDevices
  )

type TransitOperatorV2DeleteDevice =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_DELETE_DEVICE"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2DeleteDevice
  )

type TransitOperatorV2ListEmployees =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_LIST_EMPLOYEES"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2ListEmployees
  )

type TransitOperatorV2GetEmployee =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_GET_EMPLOYEE"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2GetEmployee
  )

type TransitOperatorV2CreateEmployee =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_CREATE_EMPLOYEE"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2CreateEmployee
  )

type TransitOperatorV2UpdateEmployee =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_UPDATE_EMPLOYEE"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2UpdateEmployee
  )

type TransitOperatorV2DeleteEmployee =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_DELETE_EMPLOYEE"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2DeleteEmployee
  )

type TransitOperatorV2BulkEmployees =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_BULK_EMPLOYEES"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2BulkEmployees
  )

type TransitOperatorV2EmployeeLogin =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_EMPLOYEE_LOGIN"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2EmployeeLogin
  )

type TransitOperatorV2EmployeeVerify =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_EMPLOYEE_VERIFY"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2EmployeeVerify
  )

type TransitOperatorV2ListDesignations =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_LIST_DESIGNATIONS"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2ListDesignations
  )

type TransitOperatorV2ListCrew =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_LIST_CREW"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2ListCrew
  )

type TransitOperatorV2GetLookups =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_GET_LOOKUPS"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2GetLookups
  )

type TransitOperatorV2ListServiceTypes =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_LIST_SERVICE_TYPES"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2ListServiceTypes
  )

type TransitOperatorV2GetServiceType =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_GET_SERVICE_TYPE"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2GetServiceType
  )

type TransitOperatorV2CreateServiceType =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_CREATE_SERVICE_TYPE"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2CreateServiceType
  )

type TransitOperatorV2UpdateServiceType =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_UPDATE_SERVICE_TYPE"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2UpdateServiceType
  )

type TransitOperatorV2DeleteServiceType =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_DELETE_SERVICE_TYPE"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2DeleteServiceType
  )

type TransitOperatorV2ListStationEtas =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_LIST_STATION_ETAS"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2ListStationEtas
  )

type TransitOperatorV2PutStationEtas =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_PUT_STATION_ETAS"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2PutStationEtas
  )

type TransitOperatorV2ListGtfsRoutes =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_LIST_GTFS_ROUTES"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2ListGtfsRoutes
  )

type TransitOperatorV2ListGtfsRouteStops =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_LIST_GTFS_ROUTE_STOPS"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2ListGtfsRouteStops
  )

type TransitOperatorV2SearchGtfsStops =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_SEARCH_GTFS_STOPS"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2SearchGtfsStops
  )

type TransitOperatorV2ListPersons =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_LIST_PERSONS"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2ListPersons
  )

type TransitOperatorV2AssignPersonOrganization =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_ASSIGN_PERSON_ORGANIZATION"
      :> API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitOperatorV2AssignPersonOrganization
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = transitOperatorV2ListOrganizations merchantId city :<|> transitOperatorV2GetOrganization merchantId city :<|> transitOperatorV2CreateOrganization merchantId city :<|> transitOperatorV2UpdateOrganization merchantId city :<|> transitOperatorV2DeleteOrganization merchantId city :<|> transitOperatorV2ListDepots merchantId city :<|> transitOperatorV2GetDepot merchantId city :<|> transitOperatorV2CreateDepot merchantId city :<|> transitOperatorV2UpdateDepot merchantId city :<|> transitOperatorV2DeleteDepot merchantId city :<|> transitOperatorV2ListVehicles merchantId city :<|> transitOperatorV2GetVehicle merchantId city :<|> transitOperatorV2CreateVehicle merchantId city :<|> transitOperatorV2UpdateVehicle merchantId city :<|> transitOperatorV2DeleteVehicle merchantId city :<|> transitOperatorV2BulkVehicles merchantId city :<|> transitOperatorV2ListDevices merchantId city :<|> transitOperatorV2BulkDevices merchantId city :<|> transitOperatorV2DeleteDevice merchantId city :<|> transitOperatorV2ListEmployees merchantId city :<|> transitOperatorV2GetEmployee merchantId city :<|> transitOperatorV2CreateEmployee merchantId city :<|> transitOperatorV2UpdateEmployee merchantId city :<|> transitOperatorV2DeleteEmployee merchantId city :<|> transitOperatorV2BulkEmployees merchantId city :<|> transitOperatorV2EmployeeLogin merchantId city :<|> transitOperatorV2EmployeeVerify merchantId city :<|> transitOperatorV2ListDesignations merchantId city :<|> transitOperatorV2ListCrew merchantId city :<|> transitOperatorV2GetLookups merchantId city :<|> transitOperatorV2ListServiceTypes merchantId city :<|> transitOperatorV2GetServiceType merchantId city :<|> transitOperatorV2CreateServiceType merchantId city :<|> transitOperatorV2UpdateServiceType merchantId city :<|> transitOperatorV2DeleteServiceType merchantId city :<|> transitOperatorV2ListStationEtas merchantId city :<|> transitOperatorV2PutStationEtas merchantId city :<|> transitOperatorV2ListGtfsRoutes merchantId city :<|> transitOperatorV2ListGtfsRouteStops merchantId city :<|> transitOperatorV2SearchGtfsStops merchantId city :<|> transitOperatorV2ListPersons merchantId city :<|> transitOperatorV2AssignPersonOrganization merchantId city

transitOperatorV2ListOrganizations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.Masters.Types.OrganizationList)
transitOperatorV2ListOrganizations a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a5 a7 a6
        Tools.ActorInfo.withDashboardUserActorInfo a5 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2ListOrganizations a7 a6 a4 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a5)) a1
    )

transitOperatorV2GetOrganization :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.Masters.Types.Organization)
transitOperatorV2GetOrganization a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2GetOrganization a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitOperatorV2CreateOrganization :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.CreateOrganizationReq -> Environment.FlowHandler Lib.BmsBackend.Transit.Masters.Types.Organization)
transitOperatorV2CreateOrganization a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_CREATE_ORGANIZATION" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2CreateOrganization a5 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a2 a1
    )

transitOperatorV2UpdateOrganization :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.UpdateOrganizationReq -> Environment.FlowHandler Lib.BmsBackend.Transit.Masters.Types.Organization)
transitOperatorV2UpdateOrganization a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a4 a6 a5
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_UPDATE_ORGANIZATION" a4 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2UpdateOrganization a6 a5 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a2 a1
    )

transitOperatorV2DeleteOrganization :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
transitOperatorV2DeleteOrganization a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_DELETE_ORGANIZATION" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2DeleteOrganization a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitOperatorV2ListDepots :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.Masters.Types.DepotList)
transitOperatorV2ListDepots a8 a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a6 a8 a7
        Tools.ActorInfo.withDashboardUserActorInfo a6 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2ListDepots a8 a7 a5 a4 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a6)) a1
    )

transitOperatorV2GetDepot :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.Masters.Types.Depot)
transitOperatorV2GetDepot a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2GetDepot a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitOperatorV2CreateDepot :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.CreateDepotReq -> Environment.FlowHandler Lib.BmsBackend.Transit.Masters.Types.Depot)
transitOperatorV2CreateDepot a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_CREATE_DEPOT" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2CreateDepot a5 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a2 a1
    )

transitOperatorV2UpdateDepot :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.UpdateDepotReq -> Environment.FlowHandler Lib.BmsBackend.Transit.Masters.Types.Depot)
transitOperatorV2UpdateDepot a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a4 a6 a5
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_UPDATE_DEPOT" a4 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2UpdateDepot a6 a5 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a2 a1
    )

transitOperatorV2DeleteDepot :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
transitOperatorV2DeleteDepot a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_DELETE_DEPOT" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2DeleteDepot a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitOperatorV2ListVehicles :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.Masters.Types.VehicleList)
transitOperatorV2ListVehicles a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a10 a12 a11
        Tools.ActorInfo.withDashboardUserActorInfo a10 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2ListVehicles a12 a11 a9 a8 a7 a6 a5 a4 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a10)) a1
    )

transitOperatorV2GetVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.Masters.Types.Vehicle)
transitOperatorV2GetVehicle a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2GetVehicle a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitOperatorV2CreateVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.CreateVehicleReq -> Environment.FlowHandler Lib.BmsBackend.Transit.Masters.Types.Vehicle)
transitOperatorV2CreateVehicle a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_CREATE_VEHICLE" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2CreateVehicle a5 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a2 a1
    )

transitOperatorV2UpdateVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.UpdateVehicleReq -> Environment.FlowHandler Lib.BmsBackend.Transit.Masters.Types.Vehicle)
transitOperatorV2UpdateVehicle a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a4 a6 a5
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_UPDATE_VEHICLE" a4 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2UpdateVehicle a6 a5 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a2 a1
    )

transitOperatorV2DeleteVehicle :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
transitOperatorV2DeleteVehicle a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_DELETE_VEHICLE" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2DeleteVehicle a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitOperatorV2BulkVehicles :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.BulkVehiclesReq -> Environment.FlowHandler Lib.BmsBackend.Transit.Common.BulkResult)
transitOperatorV2BulkVehicles a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_BULK_VEHICLES" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2BulkVehicles a5 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a2 a1
    )

transitOperatorV2ListDevices :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Lib.BmsBackend.Transit.Masters.Types.DeviceType) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.Masters.Types.DeviceList)
transitOperatorV2ListDevices a8 a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a6 a8 a7
        Tools.ActorInfo.withDashboardUserActorInfo a6 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2ListDevices a8 a7 a5 a4 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a6)) a1
    )

transitOperatorV2BulkDevices :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.BulkDevicesReq -> Environment.FlowHandler Lib.BmsBackend.Transit.Common.BulkResult)
transitOperatorV2BulkDevices a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_BULK_DEVICES" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2BulkDevices a5 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a2 a1
    )

transitOperatorV2DeleteDevice :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.DeviceType -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
transitOperatorV2DeleteDevice a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a4 a6 a5
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_DELETE_DEVICE" a4 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2DeleteDevice a6 a5 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a2 a1
    )

transitOperatorV2ListEmployees :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.Masters.Types.EmployeeList)
transitOperatorV2ListEmployees a9 a8 a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a7 a9 a8
        Tools.ActorInfo.withDashboardUserActorInfo a7 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2ListEmployees a9 a8 a6 a5 a4 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a7)) a1
    )

transitOperatorV2GetEmployee :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.Masters.Types.Employee)
transitOperatorV2GetEmployee a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2GetEmployee a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitOperatorV2CreateEmployee :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.CreateEmployeeReq -> Environment.FlowHandler Lib.BmsBackend.Transit.Masters.Types.Employee)
transitOperatorV2CreateEmployee a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_CREATE_EMPLOYEE" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2CreateEmployee a5 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a2 a1
    )

transitOperatorV2UpdateEmployee :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.UpdateEmployeeReq -> Environment.FlowHandler Lib.BmsBackend.Transit.Masters.Types.Employee)
transitOperatorV2UpdateEmployee a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a4 a6 a5
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_UPDATE_EMPLOYEE" a4 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2UpdateEmployee a6 a5 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a2 a1
    )

transitOperatorV2DeleteEmployee :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
transitOperatorV2DeleteEmployee a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_DELETE_EMPLOYEE" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2DeleteEmployee a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitOperatorV2BulkEmployees :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.BulkEmployeesReq -> Environment.FlowHandler Lib.BmsBackend.Transit.Common.BulkResult)
transitOperatorV2BulkEmployees a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_BULK_EMPLOYEES" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2BulkEmployees a5 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a2 a1
    )

transitOperatorV2EmployeeLogin :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.EmployeeLoginReq -> Environment.FlowHandler Lib.BmsBackend.Transit.Masters.Types.EmployeeLoginResp)
transitOperatorV2EmployeeLogin a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_EMPLOYEE_LOGIN" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2EmployeeLogin a5 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a2 a1
    )

transitOperatorV2EmployeeVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.EmployeeVerifyReq -> Environment.FlowHandler Lib.BmsBackend.Transit.Masters.Types.EmployeeVerifyResp)
transitOperatorV2EmployeeVerify a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_EMPLOYEE_VERIFY" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2EmployeeVerify a5 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a2 a1
    )

transitOperatorV2ListDesignations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [Lib.BmsBackend.Transit.Masters.Types.Designation])
transitOperatorV2ListDesignations a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a2 a4 a3
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2ListDesignations a4 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2)) a1
    )

transitOperatorV2ListCrew :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.Masters.Types.CrewMemberList)
transitOperatorV2ListCrew a9 a8 a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a7 a9 a8
        Tools.ActorInfo.withDashboardUserActorInfo a7 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2ListCrew a9 a8 a6 a5 a4 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a7)) a1
    )

transitOperatorV2GetLookups :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.Masters.Types.Lookups)
transitOperatorV2GetLookups a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a2 a4 a3
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2GetLookups a4 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2)) a1
    )

transitOperatorV2ListServiceTypes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.Masters.Types.ServiceTypeList)
transitOperatorV2ListServiceTypes a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a5 a7 a6
        Tools.ActorInfo.withDashboardUserActorInfo a5 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2ListServiceTypes a7 a6 a4 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a5)) a1
    )

transitOperatorV2GetServiceType :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.Masters.Types.ServiceType)
transitOperatorV2GetServiceType a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2GetServiceType a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitOperatorV2CreateServiceType :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.CreateServiceTypeReq -> Environment.FlowHandler Lib.BmsBackend.Transit.Masters.Types.ServiceType)
transitOperatorV2CreateServiceType a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_CREATE_SERVICE_TYPE" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2CreateServiceType a5 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a2 a1
    )

transitOperatorV2UpdateServiceType :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.UpdateServiceTypeReq -> Environment.FlowHandler Lib.BmsBackend.Transit.Masters.Types.ServiceType)
transitOperatorV2UpdateServiceType a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a4 a6 a5
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_UPDATE_SERVICE_TYPE" a4 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2UpdateServiceType a6 a5 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)) a2 a1
    )

transitOperatorV2DeleteServiceType :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
transitOperatorV2DeleteServiceType a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_DELETE_SERVICE_TYPE" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2DeleteServiceType a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitOperatorV2ListStationEtas :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler Lib.BmsBackend.Transit.Masters.Types.StationEtaList)
transitOperatorV2ListStationEtas a8 a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a6 a8 a7
        Tools.ActorInfo.withDashboardUserActorInfo a6 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2ListStationEtas a8 a7 a5 a4 a3 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a6)) a1
    )

transitOperatorV2PutStationEtas :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Lib.BmsBackend.Transit.Masters.Types.PutStationEtasReq -> Environment.FlowHandler Lib.BmsBackend.Transit.Masters.Types.StationEtaUpsertResult)
transitOperatorV2PutStationEtas a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_PUT_STATION_ETAS" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2PutStationEtas a5 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a2 a1
    )

transitOperatorV2ListGtfsRoutes :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.GtfsRoute])
transitOperatorV2ListGtfsRoutes a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a2 a4 a3
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2ListGtfsRoutes a4 a3 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2)) a1
    )

transitOperatorV2ListGtfsRouteStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> BecknV2.OnDemand.Enums.VehicleCategory -> Environment.FlowHandler [API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.GtfsRouteStop])
transitOperatorV2ListGtfsRouteStops a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2ListGtfsRouteStops a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

transitOperatorV2SearchGtfsStops :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> BecknV2.OnDemand.Enums.VehicleCategory -> Kernel.Prelude.Text -> Environment.FlowHandler [API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.GtfsStop])
transitOperatorV2SearchGtfsStops a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2SearchGtfsStops a5 a4 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a2 a1
    )

transitOperatorV2ListPersons :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.Dashboard.AppManagement.TransitOperatorV2Masters.TransitPersonList)
transitOperatorV2ListPersons a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a5 a7 a6
        Tools.ActorInfo.withDashboardUserActorInfo a5 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2ListPersons a7 a6 a4 a3 a2 a1 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a5))
    )

transitOperatorV2AssignPersonOrganization :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Lib.BmsBackend.Transit.Masters.Types.AssignPersonOrganizationReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
transitOperatorV2AssignPersonOrganization a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR_V2_MASTERS/TRANSIT_OPERATOR_V2_ASSIGN_PERSON_ORGANIZATION" a3 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.TransitOperatorV2Masters.transitOperatorV2AssignPersonOrganization a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )
