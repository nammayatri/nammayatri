module App.Routes where

import App.Routes.FarePolicy
import App.Types
import qualified Beckn.Types.APIResult as APIResult
import Beckn.Types.App
import qualified Beckn.Types.Core.API.Call as Call
import qualified Beckn.Types.Core.API.Cancel as API
import qualified Beckn.Types.Core.API.Confirm as API
import qualified Beckn.Types.Core.API.Feedback as API
import qualified Beckn.Types.Core.API.Search as API
import qualified Beckn.Types.Core.API.Status as API
import qualified Beckn.Types.Core.API.Track as API
import Beckn.Types.Storage.Case
import Beckn.Types.Storage.Person as SP
import Beckn.Types.Storage.ProductInstance
import Beckn.Types.Storage.RegistrationToken
import Beckn.Types.Storage.Vehicle
import Beckn.Utils.Servant.SignatureAuth
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import Data.Time (UTCTime)
import EulerHS.Prelude
import Product.BecknProvider.BP as BP
import Product.BecknProvider.Feedback as BP
import qualified Product.Call as Call
import qualified Product.Case.CRUD as Case
import qualified Product.Cron as Cron
import qualified Product.DriverInformation as DriverInformation
import qualified Product.HealthCheck as HealthCheck
import qualified Product.Location as Location
import qualified Product.Person as Person
import qualified Product.ProductInstance as ProductInstance
import qualified Product.Products as Product
import qualified Product.Registration as Registration
import qualified Product.Ride as Ride
import qualified Product.Transporter as Transporter
import qualified Product.Vehicle as Vehicle
import Servant
import Types.API.Case
import Types.API.Cron
import qualified Types.API.DriverInformation as DriverInformationAPI
import Types.API.Location as Location
import Types.API.Person
import Types.API.ProductInstance
import Types.API.Products
import Types.API.Registration
import qualified Types.API.Ride as RideAPI
import Types.API.Transporter
import Types.API.Vehicle
import Utils.Auth (lookup)
import Utils.Common (AdminTokenAuth, DriverTokenAuth, OrgTokenAuth, TokenAuth)

type TransportAPI =
  "v1"
    :> ( HealthCheckAPI
           :<|> RegistrationAPI
           :<|> PersonAPI
           :<|> OrganizationAPI --Transporter
           :<|> OrgBecknAPI
           :<|> CaseAPI
           :<|> CronAPI
           :<|> ProductInstanceAPI
           :<|> VehicleAPI
           :<|> LocationAPI
           :<|> ProductAPI
           :<|> CallAPIs
           :<|> RouteAPI
           :<|> DriverInformationAPI
           :<|> FarePolicyAPI
           :<|> RideAPI
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
           :> Get '[JSON] CaseListRes
           :<|> TokenAuth
           :> Capture "caseId" Text
           :> ReqBody '[JSON] UpdateCaseReq
           :> Post '[JSON] Case
       )

caseFlow ::
  (RegistrationToken -> [CaseStatus] -> CaseType -> Maybe Int -> Maybe Int -> FlowHandler CaseListRes)
    :<|> (RegistrationToken -> Text -> UpdateCaseReq -> FlowHandler Case)
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
           :> Capture "productInstanceId" ProductInstanceId
           :> ReqBody '[JSON] ProdInstUpdateReq
           :> Post '[JSON] ProdInstInfo
       )

productInstanceFlow ::
  ( RegistrationToken ->
    [ProductInstanceStatus] ->
    [CaseType] ->
    Maybe Int ->
    Maybe Int ->
    FlowHandler ProductInstanceList
  )
    :<|> ( (RegistrationToken -> Text -> FlowHandler RideListRes)
             :<|> ( (RegistrationToken -> Text -> FlowHandler RideListRes)
                      :<|> ( ( RegistrationToken ->
                               Text ->
                               Maybe CaseType ->
                               FlowHandler CaseListRes
                             )
                               :<|> ( RegistrationToken ->
                                      ProductInstanceId ->
                                      ProdInstUpdateReq ->
                                      FlowHandler ProdInstInfo
                                    )
                           )
                  )
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

productFlow :: Text -> CreateProdReq -> FlowHandler ProdRes
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

locationFlow ::
  (Text -> FlowHandler GetLocationRes)
    :<|> (RegistrationToken -> Text -> UpdateLocationReq -> FlowHandler UpdateLocationRes)
locationFlow =
  Location.getLocation
    :<|> Location.updateLocation

-- location flow over

transporterAPI :: Proxy TransportAPI
transporterAPI = Proxy

transporterServer :: FlowServer TransportAPI
transporterServer =
  healthCheckFlow
    :<|> registrationFlow
    :<|> personFlow
    :<|> organizationFlow
    :<|> orgBecknApiFlow
    :<|> caseFlow
    :<|> cronFlow
    :<|> productInstanceFlow
    :<|> vehicleFlow
    :<|> locationFlow
    :<|> productFlow
    :<|> callFlow
    :<|> routeApiFlow
    :<|> driverInformationFlow
    :<|> farePolicyFlow
    :<|> rideFlow

type OrgBecknAPI =
  Capture "orgId" OrganizationId
    :> SignatureAuth "Authorization"
    :> SignatureAuth "Proxy-Authorization"
    :> API.SearchAPI
    :<|> Capture "orgId" OrganizationId
    :> SignatureAuth "Authorization"
    :> API.ConfirmAPI
    :<|> Capture "orgId" OrganizationId
    :> SignatureAuth "Authorization"
    :> API.CancelAPI
    :<|> Capture "orgId" OrganizationId
    :> SignatureAuth "Authorization"
    :> API.StatusAPI
    :<|> Capture "orgId" OrganizationId
    :> SignatureAuth "Authorization"
    :> API.TrackAPI
    :<|> Capture "orgId" OrganizationId
    :> SignatureAuth "Authorization"
    :> API.FeedbackAPI

orgBecknApiFlow :: FlowServer OrgBecknAPI
orgBecknApiFlow =
  (\orgId -> HttpSig.withBecknAuthProxy (BP.search orgId) lookup)
    :<|> (\orgId -> HttpSig.withBecknAuth (BP.confirm orgId) lookup)
    :<|> (\orgId -> HttpSig.withBecknAuth (BP.cancel orgId) lookup)
    :<|> (\orgId -> HttpSig.withBecknAuth (BP.serviceStatus orgId) lookup)
    :<|> (\orgId -> HttpSig.withBecknAuth (BP.trackTrip orgId) lookup)
    :<|> (\orgId -> HttpSig.withBecknAuth (BP.feedback orgId) lookup)

type CronAPI =
  "cron"
    :> "expire_cases"
    :> Header "Authorization" CronAuthKey
    :> ReqBody '[JSON] ExpireCaseReq
    :> Post '[JSON] ExpireRes
    :<|> "expire_product_instances"
    :> Header "Authorization" CronAuthKey
    :> Post '[JSON] ExpireRes

cronFlow :: FlowServer CronAPI
cronFlow =
  Cron.expireCases
    :<|> Cron.expireProductInstances

-------- Initiate a call (Exotel) APIs --------
type CallAPIs =
  "call"
    :> "to_customer"
    :> TokenAuth
    :> ReqBody '[JSON] Call.CallReq
    :> Post '[JSON] Call.CallRes

callFlow :: FlowServer CallAPIs
callFlow =
  Call.initiateCall

type RouteAPI =
  "route"
    :> TokenAuth
    :> ReqBody '[JSON] Location.Request
    :> Post '[JSON] Location.Response

routeApiFlow :: FlowServer RouteAPI
routeApiFlow = Location.getRoute

type DriverInformationAPI =
  "active_drivers"
    :> TokenAuth
    :> Capture "time" UTCTime
    :> Capture "quantity" Integer
    :> Get '[JSON] DriverInformationAPI.ActiveDriversResponse
    :<|> "update_drivers_stats"
    :> AdminTokenAuth
    :> Capture "quantity" Integer
    :> Post '[JSON] APIResult.APIResult
    :<|> "driver"
    :> ( TokenAuth
           :> Get '[JSON] DriverInformationAPI.DriverInformationResponse
           :<|> "setActivity"
           :> TokenAuth
           :> MandatoryQueryParam "active" Bool
           :> Post '[JSON] APIResult.APIResult
           :<|> "notification"
           :> TokenAuth
           :> QueryParam "productInstanceId" ProductInstanceId
           :> Get '[JSON] DriverInformationAPI.GetRideInfoRes
       )

driverInformationFlow :: FlowServer DriverInformationAPI
driverInformationFlow =
  DriverInformation.getAvailableDriversInfo
    :<|> DriverInformation.updateDriversStats
    :<|> DriverInformation.getInformation
    :<|> DriverInformation.setActivity
    :<|> DriverInformation.getRideInfo

type RideAPI =
  "ride" :> "respond"
    :> TokenAuth
    :> ReqBody '[JSON] RideAPI.SetDriverAcceptanceReq
    :> Post '[JSON] RideAPI.SetDriverAcceptanceRes

rideFlow :: FlowServer RideAPI
rideFlow = Ride.setDriverAcceptance

type HealthCheckAPI = Get '[JSON] Text

healthCheckFlow :: FlowServer HealthCheckAPI
healthCheckFlow = HealthCheck.healthCheck
