module App.Routes where

import App.Routes.FarePolicy
import Beckn.Types.APISuccess
import Beckn.Types.App
import Beckn.Types.Core.Ack (AckResponse)
import qualified Beckn.Types.Core.Taxi.API.Cancel as API
import qualified Beckn.Types.Core.Taxi.API.Init as API
import qualified Beckn.Types.Core.Taxi.API.Search as API
import qualified Beckn.Types.Core.Taxi.API.Select as API
import qualified Beckn.Types.Core.Taxi.API.Track as API
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth
import Data.OpenApi
import Domain.Types.Organization (Organization)
import Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SRT
import Domain.Types.Vehicle
import qualified Domain.Types.Vehicle.Variant as Variant
import Environment
import EulerHS.Prelude
import qualified Product.BecknProvider.Cancel as BP
import Product.BecknProvider.Init as BP
import Product.BecknProvider.Search as BP
import Product.BecknProvider.Select as BP
import qualified Product.BecknProvider.Track as BP
import qualified Product.Driver as Driver
import Product.DriveronBoarding.DriverOnBoarding as DO
import qualified Product.DriveronBoarding.Idfy as Idfy
import Product.DriveronBoarding.Status as Status
import qualified Product.Location as Location
import qualified Product.OrgAdmin as OrgAdmin
import qualified Product.Registration as Registration
import qualified Product.Transporter as Transporter
import qualified Product.Vehicle as Vehicle
import Servant
import Servant.OpenApi
import qualified Types.API.Driver as DriverAPI
import Types.API.Driveronboarding.DriverOnBoarding
import Types.API.Driveronboarding.Status
import Types.API.Idfy
import Types.API.Location as Location
import qualified Types.API.OrgAdmin as OrgAdminAPI
import Types.API.Registration
import Types.API.Transporter
import Types.API.Vehicle
import Utils.Auth (AdminTokenAuth, TokenAuth)

type DriverOfferAPI =
  MainAPI
    :<|> SwaggerAPI

type MainAPI =
  "v2" :> UIAPI
    :<|> OrgBecknAPI

type UIAPI =
  HealthCheckAPI
    :<|> RegistrationAPI
    :<|> OrgAdminAPI
    :<|> DriverAPI
    :<|> VehicleAPI
    :<|> OrganizationAPI
    :<|> FarePolicyAPI
    :<|> LocationAPI
    :<|> IdfyHandlerAPI
    :<|> OnBoardingAPI

driverOfferAPI :: Proxy DriverOfferAPI
driverOfferAPI = Proxy

uiServer :: FlowServer UIAPI
uiServer =
  pure "App is UP"
    :<|> registrationFlow
    :<|> orgAdminFlow
    :<|> driverFlow
    :<|> vehicleFlow
    :<|> organizationFlow
    :<|> farePolicyFlow
    :<|> locationFlow
    :<|> idfyHandlerFlow
    :<|> onBoardingAPIFlow

mainServer :: FlowServer MainAPI
mainServer =
  uiServer
    :<|> orgBecknApiFlow

driverOfferServer :: FlowServer DriverOfferAPI
driverOfferServer =
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
    :> AdminTokenAuth
    :> Get '[JSON] OrgAdminAPI.OrgAdminProfileRes
    :<|> AdminTokenAuth
      :> ReqBody '[JSON] OrgAdminAPI.UpdateOrgAdminProfileReq
      :> Post '[JSON] OrgAdminAPI.UpdateOrgAdminProfileRes

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
             :<|> "nearbyRideRequest"
               :> ( TokenAuth
                      :> Get '[JSON] DriverAPI.GetNearbySearchRequestsRes
                  )
             :<|> "searchRequest"
               :> ( TokenAuth
                      :> "quote"
                      :> "offer"
                      :> ReqBody '[JSON] DriverAPI.DriverOfferReq
                      :> Post '[JSON] APISuccess
                  )
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
             :<|> Driver.getNearbySearchRequests
             :<|> Driver.offerQuote
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
             :> QueryParam "variant" Variant.Variant
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

-- Location update and get for tracking is as follows
type LocationAPI =
  "driver" :> "location"
    :> ( TokenAuth
           :> ReqBody '[JSON] UpdateLocationReq
           :> Post '[JSON] UpdateLocationRes
       )

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

locationFlow :: FlowServer LocationAPI
locationFlow =
  --  Location.getLocation
  Location.updateLocation

type OrgBecknAPI =
  Capture "orgId" (Id Organization)
    :> SignatureAuth "Authorization"
    :> SignatureAuth "X-Gateway-Authorization"
    :> API.SearchAPI
    :<|> Capture "orgId" (Id Organization)
    :> SignatureAuth "Authorization"
    :> API.SelectAPI
    :<|> Capture "orgId" (Id Organization)
      :> SignatureAuth "Authorization"
      :> API.InitAPI
    :<|> Capture "orgId" (Id Organization)
    :> SignatureAuth "Authorization"
    :> API.TrackAPI
    :<|> Capture "orgId" (Id Organization)
    :> SignatureAuth "Authorization"
    :> API.CancelAPI

orgBecknApiFlow :: FlowServer OrgBecknAPI
orgBecknApiFlow =
  BP.search
    :<|> BP.select
    :<|> BP.init
    :<|> BP.track
    :<|> BP.cancel

type OnBoardingAPI =
  "driver"
    :> ( "register"
           :> TokenAuth
           :> ReqBody '[JSON] DriverOnBoardingReq
           :> Post '[JSON] DriverOnBoardingRes
           :<|> "register"
           :> "status"
           :> TokenAuth
           :> Get '[JSON] StatusRes
       )

onBoardingAPIFlow :: FlowServer OnBoardingAPI
onBoardingAPIFlow =
  DO.registrationHandler
    :<|> Status.statusHandler

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
