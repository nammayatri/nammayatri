module App.Routes where

import App.Types
import Beckn.Types.API.Cancel
import Beckn.Types.API.Confirm
import Beckn.Types.API.Search
import Beckn.Types.API.Status
import Beckn.Types.API.Track
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Storage.Case
import Beckn.Types.Storage.Person as SP
import Beckn.Types.Storage.ProductInstance
import Beckn.Types.Storage.Vehicle
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import Product.BecknProvider.BP as BP
import qualified Product.Case.CRUD as Case
import qualified Product.Cron as Cron
import qualified Product.Location as Location
import qualified Product.Person as Person
import qualified Product.ProductInstance as ProductInstance
import qualified Product.Products as Product
import qualified Product.Registration as Registration
import qualified Product.Transporter as Transporter
import qualified Product.Vehicle as Vehicle
import Servant
import Types.API.Case
import Types.API.Cron
import Types.API.Location as Location
import Types.API.Person as PersonAPI
import Types.API.ProductInstance
import Types.API.Products
import Types.API.Registration
import Types.API.Transporter
import Types.API.Vehicle
import Utils.Auth (VerifyAPIKey)
import Utils.Common (AdminTokenAuth, DriverTokenAuth, OrgTokenAuth, TokenAuth)

type TransportAPI =
  "v1"
    :> ( Get '[JSON] Text
           :<|> RegistrationAPI
           :<|> PersonAPI
           :<|> OrganizationAPI --Transporter
           :<|> SearchAPI VerifyAPIKey
           :<|> ConfirmAPI
           :<|> CancelAPI
           :<|> StatusAPI
           :<|> TrackAPI
           :<|> CaseAPI
           :<|> CronAPI
           :<|> ProductInstanceAPI
           :<|> VehicleAPI
           :<|> LocationAPI
           :<|> ProductAPI
           :<|> RouteAPI
       )

---- Registration Flow ------
type RegistrationAPI =
  "token"
    :> ( ReqBody '[JSON] InitiateLoginReq
           :> Post '[JSON] InitiateLoginRes
           :<|> Capture "tokenId" Text
             :> "verify"
             :> ReqBody '[JSON] LoginReq
             :> Post '[JSON] LoginRes
           :<|> Capture "tokenId" Text
             :> "resend"
             :> ReqBody '[JSON] ReInitiateLoginReq
             :> Post '[JSON] InitiateLoginRes
       )

registrationFlow :: FlowServer RegistrationAPI
registrationFlow =
  Registration.initiateLogin
    :<|> Registration.login
    :<|> Registration.reInitiateLogin

-- Following is person flow
type PersonAPI =
  "person"
    :> ( AdminTokenAuth
           :> ReqBody '[JSON] CreatePersonReq
           :> Post '[JSON] UpdatePersonRes
           :<|> "list"
             :> AdminTokenAuth
             :> QueryParams "roles" SP.Role
             :> QueryParam "entityType" PersonAPI.EntityType
             :> QueryParam "limit" Integer
             :> QueryParam "offset" Integer
             :> Get '[JSON] ListPersonRes
           :<|> TokenAuth
             :> Capture "personId" Text
             :> "update"
             :> ReqBody '[JSON] UpdatePersonReq
             :> Post '[JSON] UpdatePersonRes
           :<|> TokenAuth
             :> QueryParam "personId" Text
             :> QueryParam "mobileNumber" Text
             :> QueryParam "mobileCountryCode" Text
             :> QueryParam "email" Text
             :> QueryParam "identifier" Text
             :> QueryParam "identifierType" SP.IdentifierType
             :> Get '[JSON] PersonEntityRes
           :<|> AdminTokenAuth
             :> Capture "personId" Text
             :> Delete '[JSON] DeletePersonRes
           :<|> AdminTokenAuth
             :> Capture "personId" Text
             :> "link"
             :> ReqBody '[JSON] LinkReq
             :> Post '[JSON] PersonEntityRes
       )

personFlow :: FlowServer PersonAPI
personFlow =
  Person.createPerson
    :<|> Person.listPerson
    :<|> Person.updatePerson
    :<|> Person.getPerson
    :<|> Person.deletePerson
    :<|> Person.linkEntity

-- Following is vehicle flow
type VehicleAPI =
  "vehicle"
    :> ( DriverTokenAuth
           :> ReqBody '[JSON] CreateVehicleReq
           :> Post '[JSON] CreateVehicleRes
           :<|> "list"
             :> DriverTokenAuth
             :> QueryParam "variant" Variant
             :> QueryParam "category" Category
             :> QueryParam "energyType" EnergyType
             :> QueryParam "limit" Int
             :> QueryParam "offset" Int
             :> Get '[JSON] ListVehicleRes
           :<|> DriverTokenAuth
             :> Capture "vehicleId" Text
             :> ReqBody '[JSON] UpdateVehicleReq
             :> Post '[JSON] UpdateVehicleRes
           :<|> DriverTokenAuth
             :> Capture "vehicleId" Text
             :> Delete '[JSON] DeleteVehicleRes
           :<|> TokenAuth
             :> QueryParam "registrationNo" Text
             :> QueryParam "vehicleId" Text
             :> Get '[JSON] CreateVehicleRes
       )

vehicleFlow :: FlowServer VehicleAPI
vehicleFlow =
  Vehicle.createVehicle
    :<|> Vehicle.listVehicles
    :<|> Vehicle.updateVehicle
    :<|> Vehicle.deleteVehicle
    :<|> Vehicle.getVehicle

-- Following is organization creation
type OrganizationAPI =
  "transporter"
    :> ( TokenAuth
           :> Get '[JSON] TransporterRec
           :<|> TokenAuth
           :> ReqBody '[JSON] TransporterReq
           :> Post '[JSON] TransporterRes
           :<|> "gateway"
           :> OrgTokenAuth
           :> ReqBody '[JSON] TransporterReq
           :> Post '[JSON] GatewayRes
           :<|> TokenAuth
           :> Capture "orgId" Text
           :> ReqBody '[JSON] UpdateTransporterReq
           :> Post '[JSON] TransporterRec
       )

organizationFlow :: FlowServer OrganizationAPI
organizationFlow =
  Transporter.getTransporter
    :<|> Transporter.createTransporter
    :<|> Transporter.createGateway
    :<|> Transporter.updateTransporter

-----------------------------
-------- Case Flow----------
type CaseAPI =
  "case"
    :> ( TokenAuth
           :> QueryParams "status" CaseStatus
           :> MandatoryQueryParam "type" CaseType
           :> QueryParam "limit" Int
           :> QueryParam "offset" Int
           :> QueryParam "ignoreOffered" Bool
           :> Get '[JSON] CaseListRes
           :<|> TokenAuth
           :> Capture "caseId" Text
           :> ReqBody '[JSON] UpdateCaseReq
           :> Post '[JSON] Case
       )

caseFlow =
  Case.list
    :<|> Case.update

-------- ProductInstance Flow----------
type ProductInstanceAPI =
  "productInstance"
    :> ( TokenAuth
           :> QueryParams "status" ProductInstanceStatus
           :> QueryParams "type" CaseType
           :> QueryParam "limit" Int
           :> QueryParam "offset" Int
           :> Get '[JSON] ProductInstanceList
           :<|> "person"
           :> TokenAuth
           :> Capture "personId" Text
           :> Get '[JSON] RideListRes
           :<|> "vehicle"
           :> TokenAuth
           :> Capture "vehicleId" Text
           :> Get '[JSON] RideListRes
           :<|> TokenAuth
           :> Capture "productInstanceId" Text
           :> "cases"
           :> QueryParam "type" CaseType
           :> Get '[JSON] CaseListRes
           :<|> TokenAuth
           :> Capture "productInstanceId" Text
           :> ReqBody '[JSON] ProdInstUpdateReq
           :> Post '[JSON] ProdInstInfo
       )

productInstanceFlow =
  ProductInstance.list
    :<|> ProductInstance.listDriverRides
    :<|> ProductInstance.listVehicleRides
    :<|> ProductInstance.listCasesByProductInstance
    :<|> ProductInstance.update

-------- Product Flow----------
type ProductAPI =
  "product"
    :> ( AdminTokenAuth
           :> ReqBody '[JSON] CreateProdReq
           :> Post '[JSON] ProdRes
       )

productFlow =
  Product.createProduct

-- Location update and get for tracking is as follows
type LocationAPI =
  "location"
    :> ( Capture "caseId" Text
           :> Get '[JSON] GetLocationRes
           :<|> TokenAuth
             :> Capture "caseId" Text
             :> ReqBody '[JSON] UpdateLocationReq
             :> Post '[JSON] UpdateLocationRes
       )

locationFlow =
  Location.getLocation
    :<|> Location.updateLocation

-- location flow over

transporterAPI :: Proxy TransportAPI
transporterAPI = Proxy

transporterServer :: V.Key (HashMap Text Text) -> FlowServer TransportAPI
transporterServer key =
  pure "App is UP"
    :<|> registrationFlow
    :<|> personFlow
    :<|> organizationFlow
    :<|> searchApiFlow
    :<|> confirmApiFlow
    :<|> cancelApiFlow
    :<|> statusApiFlow
    :<|> trackApiFlow
    :<|> caseFlow
    :<|> cronFlow
    :<|> productInstanceFlow
    :<|> vehicleFlow
    :<|> locationFlow
    :<|> productFlow
    :<|> routeApiFlow

searchApiFlow :: FlowServer (SearchAPI VerifyAPIKey)
searchApiFlow = BP.search

type ConfirmAPI =
  "confirm"
    :> ( ReqBody '[JSON] ConfirmReq
           :> Post '[JSON] AckResponse
       )

confirmApiFlow :: FlowServer ConfirmAPI
confirmApiFlow = BP.confirm

type CancelAPI =
  "cancel"
    :> ( ReqBody '[JSON] CancelReq
           :> Post '[JSON] AckResponse
       )

cancelApiFlow :: FlowServer CancelAPI
cancelApiFlow = BP.cancel

type CronAPI =
  "cron"
    :> "expire_cases"
    :> Header "Authorization" CronAuthKey
    :> ReqBody '[JSON] ExpireCaseReq
    :> Post '[JSON] ExpireCaseRes

cronFlow :: FlowServer CronAPI
cronFlow =
  Cron.expire

type StatusAPI =
  "status"
    :> "services"
    :> ( ReqBody '[JSON] StatusReq
           :> Post '[JSON] AckResponse
       )

statusApiFlow :: FlowServer StatusAPI
statusApiFlow = BP.serviceStatus

type TrackAPI =
  "track"
    :> ( ReqBody '[JSON] TrackTripReq
           :> Post '[JSON] TrackTripRes
       )

trackApiFlow :: FlowServer TrackAPI
trackApiFlow = BP.trackTrip

type RouteAPI =
  "route"
    :> TokenAuth
    :> ReqBody '[JSON] Location.Request
    :> Post '[JSON] Location.Response

routeApiFlow :: FlowServer RouteAPI
routeApiFlow = Location.getRoute
