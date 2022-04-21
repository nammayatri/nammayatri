module App.Routes where

import App.Routes.FarePolicy
import App.SchedulerExample (createBananasCountingJob, createFakeJob, createIncorrectDataJob, createTimePrinterJob)
import App.Types
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Types.APISuccess
import Beckn.Types.App
import qualified Beckn.Types.Core.Taxi.API.Cancel as API
import qualified Beckn.Types.Core.Taxi.API.Confirm as API
import qualified Beckn.Types.Core.Taxi.API.Rating as API
import qualified Beckn.Types.Core.Taxi.API.Search as API
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth
import Data.OpenApi
import qualified Domain.Types.CallStatus as SCS
import Domain.Types.Organization (Organization)
import Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SRT
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideBooking as SRB
import Domain.Types.Vehicle
import EulerHS.Prelude
import Product.BecknProvider.Cancel as BP
import Product.BecknProvider.Confirm as BP
import Product.BecknProvider.Rating as BP
import Product.BecknProvider.Search as BP
import qualified Product.Call as Call
import qualified Product.CancellationReason as CancellationReason
import qualified Product.Driver as Driver
import qualified Product.Location as Location
import qualified Product.OrgAdmin as OrgAdmin
import qualified Product.Registration as Registration
import qualified Product.Ride as Ride
import qualified Product.RideAPI.CancelRide as RideAPI.CancelRide
import qualified Product.RideAPI.EndRide as RideAPI.EndRide
import qualified Product.RideAPI.StartRide as RideAPI.StartRide
import qualified Product.RideBooking as RideBooking
import qualified Product.Services.GoogleMaps as GoogleMapsFlow
import qualified Product.Transporter as Transporter
import qualified Product.Vehicle as Vehicle
import Servant
import Servant.OpenApi
import qualified Types.API.Call as API
import qualified Types.API.CancellationReason as CancellationReasonAPI
import qualified Types.API.Driver as DriverAPI
import Types.API.Location as Location
import qualified Types.API.OrgAdmin as OrgAdminAPI
import Types.API.Registration
import qualified Types.API.Ride as RideAPI
import qualified Types.API.RideBooking as RideBookingAPI
import Types.API.Transporter
import Types.API.Vehicle
import Utils.Auth (AdminTokenAuth, TokenAuth)

type TransportAPI =
  MainAPI
    :<|> SwaggerAPI

type MainAPI =
  "v2" :> UIAPI
    :<|> "v1"
    :> OrgBecknAPI
    :<|> "v2"
    :> "test"
    :> ReqBody '[JSON] Text
    :> Get '[JSON] Text

type UIAPI =
  HealthCheckAPI
    :<|> RegistrationAPI
    :<|> OrgAdminAPI
    :<|> DriverAPI
    :<|> VehicleAPI
    :<|> OrganizationAPI --Transporter
    :<|> RideBookingAPI
    :<|> FarePolicyAPI
    :<|> LocationAPI
    :<|> CallAPIs
    :<|> RouteAPI
    :<|> RideAPI
    :<|> CancellationReasonAPI
    :<|> GoogleMapsProxyAPI

transporterAPI :: Proxy TransportAPI
transporterAPI = Proxy

uiServer :: FlowServer UIAPI
uiServer =
  pure "App is UP"
    :<|> registrationFlow
    :<|> orgAdminFlow
    :<|> driverFlow
    :<|> vehicleFlow
    :<|> organizationFlow
    :<|> rideBookingFlow
    :<|> farePolicyFlow
    :<|> locationFlow
    :<|> callFlow
    :<|> routeApiFlow
    :<|> rideFlow
    :<|> cancellationReasonFlow
    :<|> googleMapsProxyFlow

mainServer :: FlowServer MainAPI
mainServer =
  uiServer
    :<|> orgBecknApiFlow
    :<|> testServer

-- TODO: remove this
testServer :: Text -> FlowHandler Text
testServer jobType = withFlowHandlerAPI $ do
  case jobType of
    "bananas" -> void $ createBananasCountingJob 7
    "failing_time" -> void $ createTimePrinterJob 7
    "incorrect_data" -> void $ createIncorrectDataJob 7
    "fake_job" -> void $ createFakeJob 7
    _ -> logWarning "unknown job type"
  pure "OK"

transporterServer :: FlowServer TransportAPI
transporterServer =
  mainServer
    :<|> writeSwaggerJSONFlow

---- Registration Flow ------
type RegistrationAPI =
  "auth"
    :> ( ReqBody '[JSON] AuthReq
           :> Post '[JSON] AuthRes
           :<|> Capture "authId" (Id SRT.RegistrationToken)
             :> "verify"
             :> ReqBody '[JSON] AuthVerifyReq
             :> Post '[JSON] AuthVerifyRes
           :<|> "otp"
             :> Capture "authId" (Id SRT.RegistrationToken)
             :> "resend"
             :> Post '[JSON] ResendAuthRes
           :<|> "logout"
             :> TokenAuth
             :> Post '[JSON] APISuccess
       )

registrationFlow :: FlowServer RegistrationAPI
registrationFlow =
  Registration.auth
    :<|> Registration.verify
    :<|> Registration.resend
    :<|> Registration.logout

type OrgAdminAPI =
  "orgAdmin" :> "profile"
    :> ( AdminTokenAuth
           :> Get '[JSON] OrgAdminAPI.OrgAdminProfileRes
           :<|> AdminTokenAuth
             :> ReqBody '[JSON] OrgAdminAPI.UpdateOrgAdminProfileReq
             :> Post '[JSON] OrgAdminAPI.UpdateOrgAdminProfileRes
       )

orgAdminFlow :: FlowServer OrgAdminAPI
orgAdminFlow =
  OrgAdmin.getProfile
    :<|> OrgAdmin.updateProfile

type DriverAPI =
  "org" :> "driver"
    :> ( AdminTokenAuth
           :> ReqBody '[JSON] DriverAPI.OnboardDriverReq
           :> Post '[JSON] DriverAPI.OnboardDriverRes
           :<|> "list"
             :> AdminTokenAuth
             :> QueryParam "searchString" Text
             :> QueryParam "limit" Integer
             :> QueryParam "offset" Integer
             :> Get '[JSON] DriverAPI.ListDriverRes
           :<|> AdminTokenAuth
             :> Capture "driverId" (Id Person)
             :> "vehicle"
             :> Capture "vehicleId" (Id Vehicle)
             :> "link"
             :> Post '[JSON] DriverAPI.LinkVehicleRes
           :<|> AdminTokenAuth
             :> Capture "driverId" (Id Person)
             :> MandatoryQueryParam "enabled" Bool
             :> Post '[JSON] APISuccess
           :<|> AdminTokenAuth
             :> Capture "driverId" (Id Person)
             :> Delete '[JSON] APISuccess
       )
    :<|> "driver"
      :> ( "setActivity"
             :> TokenAuth
             :> MandatoryQueryParam "active" Bool
             :> Post '[JSON] APISuccess
             :<|> "profile"
               :> ( TokenAuth
                      :> Get '[JSON] DriverAPI.DriverInformationRes
                      :<|> TokenAuth
                        :> ReqBody '[JSON] DriverAPI.UpdateDriverReq
                        :> Post '[JSON] DriverAPI.UpdateDriverRes
                  )
         )

driverFlow :: FlowServer DriverAPI
driverFlow =
  ( Driver.createDriver
      :<|> Driver.listDriver
      :<|> Driver.linkVehicle
      :<|> Driver.changeDriverEnableState
      :<|> Driver.deleteDriver
  )
    :<|> ( Driver.setActivity
             :<|> ( Driver.getInformation
                      :<|> Driver.updateDriver
                  )
         )

-- Following is vehicle flow
type VehicleAPI =
  "org" :> "vehicle"
    :> ( AdminTokenAuth
           :> ReqBody '[JSON] CreateVehicleReq
           :> Post '[JSON] CreateVehicleRes
           :<|> "list"
             :> AdminTokenAuth
             :> QueryParam "variant" Variant
             :> QueryParam "registrationNo" Text
             :> QueryParam "limit" Int
             :> QueryParam "offset" Int
             :> Get '[JSON] ListVehicleRes
           :<|> AdminTokenAuth
             :> Capture "vehicleId" (Id Vehicle)
             :> ReqBody '[JSON] UpdateVehicleReq
             :> Post '[JSON] UpdateVehicleRes
           :<|> AdminTokenAuth
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
           :<|> AdminTokenAuth
           :> Capture "orgId" (Id Organization)
           :> ReqBody '[JSON] UpdateTransporterReq
           :> Post '[JSON] UpdateTransporterRes
       )

organizationFlow :: FlowServer OrganizationAPI
organizationFlow =
  Transporter.getTransporter
    :<|> Transporter.updateTransporter

type RideBookingAPI =
  "org" :> "rideBooking"
    :> ( Capture "bookingId" (Id SRB.RideBooking)
           :> TokenAuth
           :> Post '[JSON] RideBookingAPI.RideBookingStatusRes
           :<|> "list"
             :> AdminTokenAuth
             :> QueryParam "limit" Integer
             :> QueryParam "offset" Integer
             :> QueryParam "onlyActive" Bool
             :> Get '[JSON] RideBookingAPI.RideBookingListRes
           :<|> Capture "bookingId" (Id SRB.RideBooking)
             :> "cancel"
             :> AdminTokenAuth
             :> Get '[JSON] APISuccess
       )
    :<|> "driver"
    :> "rideBooking"
    :> Capture "bookingId" (Id SRB.RideBooking)
    :> "notification"
    :> ( "respond"
           :> TokenAuth
           :> ReqBody '[JSON] RideBookingAPI.SetDriverAcceptanceReq
           :> Post '[JSON] RideBookingAPI.SetDriverAcceptanceRes
           :<|> TokenAuth
           :> Get '[JSON] RideBookingAPI.GetRideInfoRes
       )

rideBookingFlow :: FlowServer RideBookingAPI
rideBookingFlow =
  ( RideBooking.rideBookingStatus
      :<|> RideBooking.rideBookingList
      :<|> RideBooking.rideBookingCancel
  )
    :<|> ( \rideBookingId ->
             RideBooking.setDriverAcceptance rideBookingId
               :<|> RideBooking.getRideInfo rideBookingId
         )

-- Location update and get for tracking is as follows
type LocationAPI =
  "driver" :> "location"
    :> ( Capture "rideId" (Id SRide.Ride) -- TODO: add auth
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
type OrgBecknAPI =
  Capture "orgId" (Id Organization)
    :> SignatureAuth "Authorization"
    :> SignatureAuth "X-Gateway-Authorization"
    :> API.SearchAPI
    :<|> Capture "orgId" (Id Organization)
    :> SignatureAuth "Authorization"
    :> API.ConfirmAPI
    :<|> Capture "orgId" (Id Organization)
    :> SignatureAuth "Authorization"
    :> API.CancelAPI
    :<|> Capture "orgId" (Id Organization)
    :> SignatureAuth "Authorization"
    :> API.RatingAPI

orgBecknApiFlow :: FlowServer OrgBecknAPI
orgBecknApiFlow =
  BP.search
    :<|> BP.confirm
    :<|> BP.cancel
    :<|> BP.ratingImpl

-------- Initiate a call (Exotel) APIs --------
type CallAPIs =
  "driver" :> "ride"
    :> Capture "rideId" (Id SRide.Ride)
    :> "call"
    :> ( "rider"
           :> TokenAuth
           :> Post '[JSON] API.CallRes
           :<|> "statusCallback"
           :> ReqBody '[JSON] API.CallCallbackReq
           :> Post '[JSON] API.CallCallbackRes
           :<|> Capture "callId" (Id SCS.CallStatus)
           :> "status"
           :> TokenAuth
           :> Get '[JSON] API.GetCallStatusRes
       )

callFlow :: FlowServer CallAPIs
callFlow rideId =
  Call.initiateCallToCustomer rideId
    :<|> Call.callStatusCallback rideId
    :<|> Call.getCallStatus rideId

type RouteAPI =
  "route"
    :> TokenAuth
    :> ReqBody '[JSON] Location.Request
    :> Post '[JSON] Location.Response

routeApiFlow :: FlowServer RouteAPI
routeApiFlow = Location.getRoutes

type RideAPI =
  "driver" :> "ride"
    :> ( "list"
           :> TokenAuth
           :> QueryParam "limit" Integer
           :> QueryParam "offset" Integer
           :> QueryParam "onlyActive" Bool
           :> Get '[JSON] RideAPI.DriverRideListRes
           :<|> TokenAuth
             :> Capture "rideId" (Id SRide.Ride)
             :> "start"
             :> ReqBody '[JSON] RideAPI.StartRideReq
             :> Post '[JSON] APISuccess
           :<|> TokenAuth
             :> Capture "rideId" (Id SRide.Ride)
             :> "end"
             :> Post '[JSON] APISuccess
           :<|> TokenAuth
             :> Capture "rideId" (Id SRide.Ride)
             :> "cancel"
             :> ReqBody '[JSON] RideAPI.CancelRideReq
             :> Post '[JSON] APISuccess
       )

rideFlow :: FlowServer RideAPI
rideFlow =
  Ride.listDriverRides
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

type SwaggerAPI = "swagger" :> Get '[JSON] OpenApi

swagger :: OpenApi
swagger = do
  let openApi = toOpenApi (Proxy :: Proxy MainAPI)
  openApi
    { _openApiInfo =
        (_openApiInfo openApi)
          { _infoTitle = "Yatri Partner",
            _infoVersion = "1.0"
          }
    }

writeSwaggerJSONFlow :: FlowServer SwaggerAPI
writeSwaggerJSONFlow = return swagger
