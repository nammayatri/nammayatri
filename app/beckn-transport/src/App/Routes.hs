module App.Routes where

import App.Routes.FarePolicy
import App.Types
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Types.APISuccess (APISuccess)
import Beckn.Types.App
import qualified Beckn.Types.Core.API.Call as Call
import qualified Beckn.Types.Core.API.Cancel as API
import qualified Beckn.Types.Core.API.Confirm as API
import qualified Beckn.Types.Core.API.Feedback as API
import qualified Beckn.Types.Core.API.Search as API
import qualified Beckn.Types.Core.API.Status as API
import qualified Beckn.Types.Core.API.Track as API
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth
import EulerHS.Prelude
import Product.BecknProvider.BP as BP
import Product.BecknProvider.Confirm as BP
import Product.BecknProvider.Feedback as BP
import Product.BecknProvider.Search as BP
import qualified Product.Call as Call
import qualified Product.CancellationReason as CancellationReason
import qualified Product.Case as Case
import qualified Product.Cron as Cron
import qualified Product.DriverInformation as DriverInformation
import qualified Product.Location as Location
import qualified Product.Person as Person
import qualified Product.ProductInstance as ProductInstance
import qualified Product.Products as Product
import qualified Product.Registration as Registration
import qualified Product.Ride as Ride
import qualified Product.RideAPI.CancelRide as RideAPI.CancelRide
import qualified Product.RideAPI.EndRide as RideAPI.EndRide
import qualified Product.RideAPI.StartRide as RideAPI.StartRide
import qualified Product.Services.GoogleMaps as GoogleMapsFlow
import qualified Product.Transporter as Transporter
import qualified Product.Vehicle as Vehicle
import Servant
import qualified Types.API.CancellationReason as CancellationReasonAPI
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
import Types.App (Ride)
import Types.Storage.Case
import Types.Storage.Organization (Organization)
import Types.Storage.Person as SP
import Types.Storage.ProductInstance
import Types.Storage.Vehicle
import Utils.Auth (AdminTokenAuth, DriverTokenAuth, LookupRegistryOrg, TokenAuth)

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
           :<|> CancellationReasonAPI
           :<|> GoogleMapsProxyAPI
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
           :<|> "logout"
             :> TokenAuth
             :> Post '[JSON] APISuccess
       )

registrationFlow :: FlowServer RegistrationAPI
registrationFlow =
  Registration.initiateLogin
    :<|> Registration.login
    :<|> Registration.reInitiateLogin
    :<|> Registration.logout

-- Following is person flow
type PersonAPI =
  "person"
    :> ( TokenAuth
           :> Get '[JSON] GetPersonDetailsRes
           :<|> "list"
             :> AdminTokenAuth
             :> QueryParams "roles" SP.Role
             :> QueryParam "limit" Integer
             :> QueryParam "offset" Integer
             :> Get '[JSON] ListPersonRes
           :<|> TokenAuth
             :> "update"
             :> ReqBody '[JSON] UpdatePersonReq
             :> Post '[JSON] UpdatePersonRes
           :<|> AdminTokenAuth
             :> Capture "personId" (Id Person)
             :> Delete '[JSON] DeletePersonRes
       )

personFlow :: FlowServer PersonAPI
personFlow =
  Person.getPersonDetails
    :<|> Person.listPerson
    :<|> Person.updatePerson
    :<|> Person.deletePerson

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
             :> QueryParam "registrationNo" Text
             :> QueryParam "limit" Int
             :> QueryParam "offset" Int
             :> Get '[JSON] ListVehicleRes
           :<|> DriverTokenAuth
             :> Capture "vehicleId" (Id Vehicle)
             :> ReqBody '[JSON] UpdateVehicleReq
             :> Post '[JSON] UpdateVehicleRes
           :<|> DriverTokenAuth
             :> Capture "vehicleId" (Id Vehicle)
             :> Delete '[JSON] DeleteVehicleRes
           :<|> TokenAuth
             :> QueryParam "registrationNo" Text
             :> QueryParam "vehicleId" (Id Vehicle)
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
           :<|> TokenAuth
           :> Capture "orgId" (Id Organization)
           :> ReqBody '[JSON] UpdateTransporterReq
           :> Post '[JSON] TransporterRec
       )

organizationFlow :: FlowServer OrganizationAPI
organizationFlow =
  Transporter.getTransporter
    :<|> Transporter.createTransporter
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
       )

caseFlow :: FlowServer CaseAPI
caseFlow = Case.list

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
           :> Capture "personId" (Id Person)
           :> QueryParam "limit" Integer
           :> QueryParam "offset" Integer
           :> Get '[JSON] RideListRes
           :<|> "vehicle"
           :> TokenAuth
           :> Capture "vehicleId" (Id Vehicle)
           :> Get '[JSON] RideListRes
           :<|> TokenAuth
           :> Capture "productInstanceId" (Id ProductInstance)
           :> "cases"
           :> QueryParam "type" CaseType
           :> Get '[JSON] CaseListRes
       )

productInstanceFlow :: FlowServer ProductInstanceAPI
productInstanceFlow =
  ProductInstance.list
    :<|> ProductInstance.listDriverRides
    :<|> ProductInstance.listVehicleRides
    :<|> ProductInstance.listCasesByProductInstance

-------- Product Flow----------
type ProductAPI =
  "product"
    :> ( AdminTokenAuth
           :> ReqBody '[JSON] CreateProdReq
           :> Post '[JSON] ProdRes
       )

productFlow :: FlowServer ProductAPI
productFlow =
  Product.createProduct

-- Location update and get for tracking is as follows
type LocationAPI =
  "location"
    :> ( Capture "productInstanceId" (Id ProductInstance) -- TODO: add auth
           :> Get '[JSON] GetLocationRes
           :<|> TokenAuth
           :> ReqBody '[JSON] UpdateLocationReq
           :> Post '[JSON] UpdateLocationRes
       )

locationFlow :: FlowServer LocationAPI
locationFlow =
  Location.getLocation
    :<|> Location.updateLocation

-- location flow over

transporterAPI :: Proxy TransportAPI
transporterAPI = Proxy

transporterServer :: FlowServer TransportAPI
transporterServer =
  pure "App is UP"
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
    :<|> cancellationReasonFlow
    :<|> googleMapsProxyFlow

type OrgBecknAPI =
  Capture "orgId" (Id Organization)
    :> SignatureAuth "Authorization" LookupRegistryOrg
    :> SignatureAuth "Proxy-Authorization" LookupRegistryOrg
    :> API.SearchAPI
    :<|> Capture "orgId" (Id Organization)
    :> SignatureAuth "Authorization" LookupRegistryOrg
    :> API.ConfirmAPI
    :<|> Capture "orgId" (Id Organization)
    :> SignatureAuth "Authorization" LookupRegistryOrg
    :> API.CancelAPI
    :<|> Capture "orgId" (Id Organization)
    :> SignatureAuth "Authorization" LookupRegistryOrg
    :> API.StatusAPI
    :<|> Capture "orgId" (Id Organization)
    :> SignatureAuth "Authorization" LookupRegistryOrg
    :> API.TrackAPI
    :<|> Capture "orgId" (Id Organization)
    :> SignatureAuth "Authorization" LookupRegistryOrg
    :> API.FeedbackAPI

orgBecknApiFlow :: FlowServer OrgBecknAPI
orgBecknApiFlow =
  BP.search
    :<|> BP.confirm
    :<|> BP.cancel
    :<|> BP.serviceStatus
    :<|> BP.trackTrip
    :<|> BP.feedback

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
routeApiFlow = Location.getRoutes

type DriverInformationAPI =
  "driver"
    :> ( AdminTokenAuth
           :> ReqBody '[JSON] DriverInformationAPI.CreateDriverReq
           :> Post '[JSON] DriverInformationAPI.CreateDriverRes
           :<|> TokenAuth
             :> Get '[JSON] DriverInformationAPI.DriverInformationResponse
           :<|> "setOnline"
             :> TokenAuth
             :> MandatoryQueryParam "active" Bool
             :> Post '[JSON] APISuccess
           :<|> "notification"
             :> TokenAuth
             :> QueryParam "productInstanceId" (Id Ride)
             :> Get '[JSON] DriverInformationAPI.GetRideInfoRes
           :<|> "list"
             :> AdminTokenAuth
             :> QueryParam "searchString" Text
             :> QueryParam "limit" Integer
             :> QueryParam "offset" Integer
             :> Get '[JSON] DriverInformationAPI.ListDriverRes
           :<|> AdminTokenAuth
             :> "linkVehicle"
             :> Capture "personId" (Id Person)
             :> ReqBody '[JSON] DriverInformationAPI.LinkVehicleReq
             :> Post '[JSON] DriverInformationAPI.LinkVehicleRes
           :<|> "activate"
             :> AdminTokenAuth
             :> Capture "personId" (Id Person)
             :> Post '[JSON] APISuccess
           :<|> "deactivate"
             :> AdminTokenAuth
             :> Capture "personId" (Id Person)
             :> Post '[JSON] APISuccess
       )

driverInformationFlow :: FlowServer DriverInformationAPI
driverInformationFlow =
  DriverInformation.createDriver
    :<|> DriverInformation.getInformation
    :<|> DriverInformation.setOnline
    :<|> DriverInformation.getRideInfo
    :<|> DriverInformation.listDriver
    :<|> DriverInformation.linkVehicle
    :<|> DriverInformation.activateDriver
    :<|> DriverInformation.deactivateDriver

type RideAPI =
  "ride"
    :> ( "respond"
           :> TokenAuth
           :> ReqBody '[JSON] RideAPI.SetDriverAcceptanceReq
           :> Post '[JSON] RideAPI.SetDriverAcceptanceRes
           :<|> TokenAuth
             :> Capture "rideId" (Id ProductInstance)
             :> "start"
             :> ReqBody '[JSON] RideAPI.StartRideReq
             :> Post '[JSON] APISuccess
           :<|> TokenAuth
             :> Capture "rideId" (Id ProductInstance)
             :> "end"
             :> Post '[JSON] APISuccess
           :<|> TokenAuth
             :> Capture "rideId" (Id ProductInstance)
             :> "cancel"
             :> ReqBody '[JSON] RideAPI.CancelRideReq
             :> Post '[JSON] APISuccess
       )

rideFlow :: FlowServer RideAPI
rideFlow =
  Ride.setDriverAcceptance
    :<|> RideAPI.StartRide.startRide
    :<|> RideAPI.EndRide.endRide
    :<|> RideAPI.CancelRide.cancelRide

type CancellationReasonAPI =
  "cancellationReason"
    :> ( "list"
           :> TokenAuth
           :> Get '[JSON] CancellationReasonAPI.ListRes
       )

cancellationReasonFlow :: FlowServer CancellationReasonAPI
cancellationReasonFlow = CancellationReason.list

type HealthCheckAPI = Get '[JSON] Text

type GoogleMapsProxyAPI =
  "googleMaps"
    :> ( "autoComplete"
           :> TokenAuth
           :> MandatoryQueryParam "input" Text
           :> MandatoryQueryParam "location" Text -- Passing it as <latitude>,<longitude>
           :> MandatoryQueryParam "radius" Integer
           :> MandatoryQueryParam "language" Text
           :> Get '[JSON] GoogleMaps.SearchLocationResp
           :<|> "placeDetails"
             :> TokenAuth
             :> MandatoryQueryParam "place_id" Text
             :> Get '[JSON] GoogleMaps.PlaceDetailsResp
           :<|> "getPlaceName"
             :> TokenAuth
             :> MandatoryQueryParam "latlng" Text -- Passing it as <latitude>,<longitude>
             :> Get '[JSON] GoogleMaps.GetPlaceNameResp
       )

googleMapsProxyFlow :: FlowServer GoogleMapsProxyAPI
googleMapsProxyFlow =
  GoogleMapsFlow.autoComplete
    :<|> GoogleMapsFlow.placeDetails
    :<|> GoogleMapsFlow.getPlaceName
