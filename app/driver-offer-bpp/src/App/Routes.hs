module App.Routes where

import qualified API.Beckn as Beckn
import qualified API.UI.CancellationReason as CancellationReason
import qualified API.UI.Driver as Driver
import qualified API.UI.Registration as Registration
import qualified API.UI.Ride as Ride
import App.Routes.FarePolicy
import Beckn.Types.Id
import Beckn.Utils.Common
import Data.OpenApi
import Domain.Types.Organization (Organization)
import Domain.Types.Person as SP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.Vehicle.Variant as Variant
import Environment
import EulerHS.Prelude
import qualified Product.Call as Call
import qualified Product.DriverOnboarding.DriverLicense as DriverOnboarding
import qualified Product.DriverOnboarding.Idfy as Idfy
import qualified Product.DriverOnboarding.Status as DriverOnboarding
import qualified Product.DriverOnboarding.VehicleRegistrationCertificate as DriverOnboarding
import qualified Product.Location as Location
import qualified Product.OrgAdmin as OrgAdmin
import qualified Product.Transporter as Transporter
import qualified Product.Vehicle as Vehicle
import Servant
import Servant.OpenApi
import qualified Types.API.Call as CallAPI
import qualified Types.API.DriverOnboarding.DriverLicense as DriverOnboarding
import qualified Types.API.DriverOnboarding.Status as DriverOnboarding
import qualified Types.API.DriverOnboarding.VehicleRegistrationCertificate as DriverOnboarding
import Types.API.Idfy
import Types.API.Location as Location
import qualified Types.API.OrgAdmin as OrgAdminAPI
import Types.API.Transporter
import Types.API.Vehicle
import Utils.Auth (AdminTokenAuth, TokenAuth)

type DriverOfferAPI =
  MainAPI
    :<|> SwaggerAPI

type MainAPI =
  UIAPI
    :<|> Beckn.API

type UIAPI =
  "ui"
    :> ( HealthCheckAPI
          :<|> Registration.API
          :<|> DriverOnboardingAPI
          :<|> OrgAdminAPI
          :<|> Driver.API
          :<|> VehicleAPI
          :<|> OrganizationAPI
          :<|> FarePolicyAPI
          :<|> LocationAPI
          :<|> RouteAPI
          :<|> Ride.API
          :<|> CallAPIs
          :<|> IdfyHandlerAPI
          :<|> CancellationReason.API
       )

driverOfferAPI :: Proxy DriverOfferAPI
driverOfferAPI = Proxy

uiServer :: FlowServer UIAPI
uiServer =
  pure "App is UP"
    :<|> Registration.handler
    :<|> driverOnboardingFlow
    :<|> orgAdminFlow
    :<|> Driver.handler
    :<|> vehicleFlow
    :<|> organizationFlow
    :<|> farePolicyFlow
    :<|> locationFlow
    :<|> routeFlow
    :<|> Ride.handler
    :<|> callFlow
    :<|> idfyHandlerFlow
    :<|> CancellationReason.handler

mainServer :: FlowServer MainAPI
mainServer =
  uiServer
    :<|> Beckn.handler

driverOfferServer :: FlowServer DriverOfferAPI
driverOfferServer =
  mainServer
    :<|> writeSwaggerJSONFlow

type DriverOnboardingAPI =
  "driver" :> "register"
    :> ( "dl"
           :> ( TokenAuth
                  :> ReqBody '[JSON] DriverOnboarding.DriverDLReq
                  :> Post '[JSON] DriverOnboarding.DriverDLRes
                  :<|> "image"
                    :> TokenAuth
                    :> ReqBody '[JSON] DriverOnboarding.DriverDLImageReq
                    :> Post '[JSON] DriverOnboarding.DriverDLRes
              )
           :<|> "rc"
             :> ( TokenAuth
                    :> ReqBody '[JSON] DriverOnboarding.DriverRCReq
                    :> Post '[JSON] DriverOnboarding.DriverRCRes
                    :<|> "image"
                      :> TokenAuth
                      :> ReqBody '[JSON] DriverOnboarding.DriverRCImageReq
                      :> Post '[JSON] DriverOnboarding.DriverRCRes
                )
           :<|> "status"
             :> TokenAuth
             :> Get '[JSON] DriverOnboarding.StatusRes
       )

driverOnboardingFlow :: FlowServer DriverOnboardingAPI
driverOnboardingFlow =
  ( DriverOnboarding.verifyDL
      :<|> DriverOnboarding.validateDLImage
  )
    :<|> ( DriverOnboarding.verifyRC
             :<|> DriverOnboarding.validateRCImage
         )
    :<|> DriverOnboarding.statusHandler
    
type OrgAdminAPI =
  "orgAdmin" :> "profile"
    :> AdminTokenAuth
    :> Get '[JSON] OrgAdminAPI.OrgAdminProfileRes
    :<|> AdminTokenAuth
      :> ReqBody '[JSON] OrgAdminAPI.UpdateOrgAdminProfileReq
      :> Post '[JSON] OrgAdminAPI.UpdateOrgAdminProfileRes

orgAdminFlow :: FlowServer OrgAdminAPI
orgAdminFlow =
  OrgAdmin.getProfile
    :<|> OrgAdmin.updateProfile

-- Following is vehicle flow
type VehicleAPI =
  "org" :> "vehicle"
    :> ( "list"
           :> AdminTokenAuth
           :> QueryParam "variant" Variant.Variant
           :> QueryParam "registrationNo" Text
           :> QueryParam "limit" Int
           :> QueryParam "offset" Int
           :> Get '[JSON] ListVehicleRes
           :<|> AdminTokenAuth
             :> Capture "driverId" (Id Person)
             :> ReqBody '[JSON] UpdateVehicleReq
             :> Post '[JSON] UpdateVehicleRes
           :<|> TokenAuth
             :> QueryParam "registrationNo" Text
             :> QueryParam "driverId" (Id Person)
             :> Get '[JSON] GetVehicleRes
       )

vehicleFlow :: FlowServer VehicleAPI
vehicleFlow =
  Vehicle.listVehicles
    :<|> Vehicle.updateVehicle
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

-- Location update and get for tracking is as follows
type LocationAPI =
  "driver" :> "location"
    :> ( Capture "rideId" (Id DRide.Ride) -- TODO: add auth
           :> Get '[JSON] GetLocationRes
           :<|> TokenAuth
             :> ReqBody '[JSON] UpdateLocationReq
             :> Post '[JSON] UpdateLocationRes
       )

locationFlow :: FlowServer LocationAPI
locationFlow =
  Location.getLocation
    :<|> Location.updateLocation

type RouteAPI =
  "route"
    :> TokenAuth
    :> ReqBody '[JSON] Location.Request
    :> Post '[JSON] Location.Response

routeFlow :: FlowServer RouteAPI
routeFlow = Location.getRoute

type IdfyHandlerAPI =
  "ext" :> "idfy"
    :> ( "drivingLicense"
           :> ReqBody '[JSON] IdfyDLReq
           :> Post '[JSON] AckResponse
           :<|> "vehicleRegistrationCert"
             :> ReqBody '[JSON] IdfyRCReq
             :> Post '[JSON] AckResponse
       )

idfyHandlerFlow :: FlowServer IdfyHandlerAPI
idfyHandlerFlow =
  Idfy.idfyDrivingLicense --update handler
    :<|> Idfy.idfyRCLicense --update handler

-------- Direct call (Exotel) APIs
type CallAPIs =
  "exotel"
    :> "call"
    :> ( "customer"
           :> "number"
           :> MandatoryQueryParam "CallSid" Text
           :> MandatoryQueryParam "CallFrom" Text
           :> MandatoryQueryParam "CallTo" Text
           :> MandatoryQueryParam "CallStatus" Text
           :> Get '[JSON] CallAPI.MobileNumberResp
           :<|> "statusCallback"
           :> MandatoryQueryParam "CallSid" Text
           :> MandatoryQueryParam "DialCallStatus" Text
           :> MandatoryQueryParam "RecordingUrl" Text
           :> QueryParam "Legs[0][OnCallDuration]" Int
           :> Get '[JSON] CallAPI.CallCallbackRes
       )

-- :<|> "ride"
--   :> Capture "rideId" (Id DRide.Ride)
--   :> "call"
--   :> "status"
--   :> TokenAuth
--   :> Get '[JSON] CallAPI.GetCallStatusRes

callFlow :: FlowServer CallAPIs
callFlow =
  Call.getCustomerMobileNumber
    :<|> Call.directCallStatusCallback

-- :<|> Call.getCallStatus

type HealthCheckAPI = Get '[JSON] Text

type SwaggerAPI = "swagger" :> Get '[JSON] OpenApi

swagger :: OpenApi
swagger = do
  let openApi = toOpenApi (Proxy :: Proxy MainAPI)
  openApi
    { _openApiInfo =
        (_openApiInfo openApi)
          { _infoTitle = "Namma Yatri Partner",
            _infoVersion = "1.0"
          }
    }

writeSwaggerJSONFlow :: FlowServer SwaggerAPI
writeSwaggerJSONFlow = return swagger
