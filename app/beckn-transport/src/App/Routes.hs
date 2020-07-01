module App.Routes where

-- import           Beckn.Types.API.Search
-- import           Beckn.Types.API.Confirm
-- import           Beckn.Types.Common

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
import Beckn.Types.Storage.Products
import Data.Aeson
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import Network.Wai.Parse
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
import Servant.Multipart
import Types.API.Case
import Types.API.Cron
import Types.API.Location
import Types.API.Person
import Types.API.ProductInstance
import Types.API.Products
import Types.API.Registration
import Types.API.Registration
import Types.API.Transporter
import Types.API.Vehicle

type TransporterAPIs =
  "v1"
    :> ( Get '[JSON] Text
           :<|> RegistrationAPIs
           :<|> PersonAPIs
           :<|> OrganizationAPIs --Transporter
           :<|> SearchAPIs
           :<|> ConfirmAPIs
           :<|> CancelAPIs
           :<|> StatusAPIs
           :<|> TrackApis
           :<|> CaseAPIs
           :<|> CronAPIs
           :<|> ProductInstanceAPIs
           :<|> VehicleAPIs
           :<|> LocationAPIs
           :<|> ProductAPIs
       )

---- Registration Flow ------
type RegistrationAPIs =
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

registrationFlow :: FlowServer RegistrationAPIs
registrationFlow =
  Registration.initiateLogin
    :<|> Registration.login
    :<|> Registration.reInitiateLogin

-- Following is person flow
type PersonAPIs =
  "person"
    :> ( AuthHeader
           :> ReqBody '[JSON] CreatePersonReq
           :> Post '[JSON] UpdatePersonRes
           :<|> "list"
             :> AuthHeader
             :> QueryParams "roles" SP.Role
             :> QueryParam "limit" Integer
             :> QueryParam "offset" Integer
             :> Get '[JSON] ListPersonRes
           :<|> Capture "personId" Text
             :> AuthHeader
             :> "update"
             :> ReqBody '[JSON] UpdatePersonReq
             :> Post '[JSON] UpdatePersonRes
           :<|> AuthHeader
             :> QueryParam "personId" Text
             :> QueryParam "mobileNumber" Text
             :> QueryParam "email" Text
             :> QueryParam "identifier" Text
             :> Get '[JSON] PersonRes
           :<|> Capture "personId" Text
             :> AuthHeader
             :> Delete '[JSON] DeletePersonRes
       )

personFlow :: FlowServer PersonAPIs
personFlow =
  Person.createPerson
    :<|> Person.listPerson
    :<|> Person.updatePerson
    :<|> Person.getPerson
    :<|> Person.deletePerson

-- Following is vehicle flow
type VehicleAPIs =
  "vehicle"
    :> ( AuthHeader
           :> ReqBody '[JSON] CreateVehicleReq
           :> Post '[JSON] CreateVehicleRes
           :<|> "list"
             :> AuthHeader
             :> QueryParam "limit" Integer
             :> QueryParam "offset" Integer
             :> Get '[JSON] ListVehicleRes
           :<|> Capture "vehicleId" Text
             :> AuthHeader
             :> ReqBody '[JSON] UpdateVehicleReq
             :> Post '[JSON] UpdateVehicleRes
           :<|> Capture "vehicleId" Text
             :> AuthHeader
             :> Delete '[JSON] DeleteVehicleRes
           :<|> AuthHeader
             :> QueryParam "registrationNo" Text
             :> QueryParam "vehicleId" Text
             :> Get '[JSON] CreateVehicleRes
       )

vehicleFlow :: FlowServer VehicleAPIs
vehicleFlow =
  Vehicle.createVehicle
    :<|> Vehicle.listVehicles
    :<|> Vehicle.updateVehicle
    :<|> Vehicle.deleteVehicle
    :<|> Vehicle.getVehicle

-- Following is organization creation
type OrganizationAPIs =
  "transporter"
    :> ( AuthHeader
           :> Get '[JSON] TransporterRec
           :<|> AuthHeader
           :> ReqBody '[JSON] TransporterReq
           :> Post '[JSON] TransporterRes
           :<|> "gateway"
           :> AuthHeader
           :> ReqBody '[JSON] TransporterReq
           :> Post '[JSON] GatewayRes
           :<|> Capture "orgId" Text
           :> AuthHeader
           :> ReqBody '[JSON] UpdateTransporterReq
           :> Post '[JSON] TransporterRec
       )

organizationFlow :: FlowServer OrganizationAPIs
organizationFlow =
  Transporter.getTransporter
    :<|> Transporter.createTransporter
    :<|> Transporter.createGateway
    :<|> Transporter.updateTransporter

-----------------------------
-------- Case Flow----------
type CaseAPIs =
  "case"
    :> ( AuthHeader
           :> QueryParams "status" CaseStatus
           :> MandatoryQueryParam "type" CaseType
           :> QueryParam "limit" Int
           :> QueryParam "offset" Int
           :> QueryParam "ignoreOffered" Bool
           :> Get '[JSON] CaseListRes
           :<|> AuthHeader
           :> Capture "caseId" Text
           :> ReqBody '[JSON] UpdateCaseReq
           :> Post '[JSON] Case
       )

caseFlow =
  Case.list
    :<|> Case.update

-------- ProductInstance Flow----------
type ProductInstanceAPIs =
  "productInstance"
    :> ( AuthHeader
           :> QueryParams "status" ProductInstanceStatus
           :> QueryParam "limit" Int
           :> QueryParam "offset" Int
           :> Get '[JSON] ProductInstanceList
       )

productInstanceFlow =
  ProductInstance.list

-------- Product Flow----------
type ProductAPIs =
  "product"
    :> ( AuthHeader
           :> QueryParam "vehicleId" Text
           :> Get '[JSON] ProdListRes
           :<|> AuthHeader
             :> Capture "productId" Text
             :> ReqBody '[JSON] ProdReq
             :> Post '[JSON] ProdInfoRes
           :<|> AuthHeader
             :> Capture "productId" Text
             :> "cases"
             :> QueryParam "type" CaseType
             :> Get '[JSON] CaseListRes
       )

productFlow =
  Product.listRides
    :<|> Product.update
    :<|> Product.listCasesByProd

-- Location update and get for tracking is as follows
type LocationAPIs =
  "location"
    :> ( Capture "caseId" Text
           :> Get '[JSON] GetLocationRes
           :<|> Capture "caseId" Text
             :> AuthHeader
             :> ReqBody '[JSON] UpdateLocationReq
             :> Post '[JSON] UpdateLocationRes
       )

locationFlow =
  Location.getLocation
    :<|> Location.updateLocation

-- location flow over

transporterAPIs :: Proxy TransporterAPIs
transporterAPIs = Proxy

transporterServer' :: V.Key (HashMap Text Text) -> FlowServer TransporterAPIs
transporterServer' key =
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

type SearchAPIs =
  "search"
    :> "services"
    :> ( ReqBody '[JSON] SearchReq
           :> Post '[JSON] AckResponse
       )

searchApiFlow :: FlowServer SearchAPIs
searchApiFlow = BP.search

type ConfirmAPIs =
  "confirm"
    :> "services"
    :> ( ReqBody '[JSON] ConfirmReq
           :> Post '[JSON] AckResponse
       )

confirmApiFlow :: FlowServer ConfirmAPIs
confirmApiFlow = BP.confirm

type CancelAPIs =
  "cancel"
    :> "services"
    :> ( ReqBody '[JSON] CancelReq
           :> Post '[JSON] AckResponse
       )

cancelApiFlow :: FlowServer CancelAPIs
cancelApiFlow = BP.cancel

type CronAPIs =
  "cron"
    :> "expire_cases"
    :> Header "Authorization" CronAuthKey
    :> ReqBody '[JSON] ExpireCaseReq
    :> Post '[JSON] ExpireCaseRes

cronFlow :: FlowServer CronAPIs
cronFlow =
  Cron.expire

type StatusAPIs =
  "status"
    :> "services"
    :> ( ReqBody '[JSON] StatusReq
           :> Post '[JSON] AckResponse
       )

statusApiFlow :: FlowServer StatusAPIs
statusApiFlow = BP.serviceStatus

type TrackApis =
  "track"
    :> "trip"
    :> ( ReqBody '[JSON] TrackTripReq
           :> Post '[JSON] TrackTripRes
       )

trackApiFlow :: FlowServer TrackApis
trackApiFlow = BP.trackTrip
