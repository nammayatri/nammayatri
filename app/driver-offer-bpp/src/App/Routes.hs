module App.Routes where

import qualified API.Beckn as Beckn
import qualified API.UI.CancellationReason as CancellationReason
import qualified API.UI.Driver as Driver
import qualified API.UI.Location as Location
import qualified API.UI.OrgAdmin as OrgAdmin
import qualified API.UI.Registration as Registration
import qualified API.UI.Ride as Ride
import qualified API.UI.Route as Route
import qualified API.UI.Transporter as Transporter
import qualified API.UI.Vehicle as Vehicle
import App.Routes.FarePolicy
import Beckn.Utils.Common
import Data.OpenApi
import Environment
import EulerHS.Prelude
import qualified Product.Call as Call
import qualified Product.DriverOnboarding.DriverLicense as DriverOnboarding
import qualified Product.DriverOnboarding.Idfy as Idfy
import qualified Product.DriverOnboarding.Status as DriverOnboarding
import qualified Product.DriverOnboarding.VehicleRegistrationCertificate as DriverOnboarding
import Servant
import Servant.OpenApi
import qualified Types.API.Call as CallAPI
import qualified Types.API.DriverOnboarding.DriverLicense as DriverOnboarding
import qualified Types.API.DriverOnboarding.Status as DriverOnboarding
import qualified Types.API.DriverOnboarding.VehicleRegistrationCertificate as DriverOnboarding
import Types.API.Idfy
import Utils.Auth (TokenAuth)

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
          :<|> OrgAdmin.API
          :<|> Driver.API
          :<|> Vehicle.API
          :<|> Transporter.API
          :<|> FarePolicyAPI
          :<|> Location.API
          :<|> Route.API
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
    :<|> OrgAdmin.handler
    :<|> Driver.handler
    :<|> Vehicle.handler
    :<|> Transporter.handler
    :<|> farePolicyFlow
    :<|> Location.handler
    :<|> Route.handler
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
