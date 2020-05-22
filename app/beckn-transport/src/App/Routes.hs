module App.Routes where

-- import           Beckn.Types.API.Search
-- import           Beckn.Types.API.Confirm
-- import           Beckn.Types.Common

import Beckn.Types.API.Confirm
import Beckn.Types.API.Search
import Beckn.Types.API.Status
import Beckn.Types.API.Track
import Beckn.Types.App
import Beckn.Types.Common
import Beckn.Types.Storage.Case
import Beckn.Types.Storage.Products
import Data.Aeson
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import Network.Wai.Parse
import Product.BecknProvider.BP as BP
import qualified Product.Case.CRUD as Case
import qualified Product.CaseProduct as CaseProduct
import qualified Product.Location as Location
import qualified Product.Person as Person
import qualified Product.Products as Product
import qualified Product.Registration as Registration
import qualified Product.Transporter as Transporter
import qualified Product.Vehicle as Vehicle
import Servant
import Servant.Multipart
import Types.API.Case
import Types.API.CaseProduct
import Types.API.Location
import Types.API.Person
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
           :<|> StatusAPIs
           :<|> TrackApis
           :<|> CaseAPIs
           :<|> CaseProductAPIs
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
    :> ( Header "authorization" Text
           :> ReqBody '[JSON] CreatePersonReq
           :> Post '[JSON] UpdatePersonRes
           :<|> "list"
             :> Header "authorization" Text
             :> ReqBody '[JSON] ListPersonReq
             :> Post '[JSON] ListPersonRes
           :<|> Capture "personId" Text
             :> Header "authorization" Text
             :> "update"
             :> ReqBody '[JSON] UpdatePersonReq
             :> Post '[JSON] UpdatePersonRes
       )

personFlow :: FlowServer PersonAPIs
personFlow =
  Person.createPerson
    :<|> Person.listPerson
    :<|> Person.updatePerson

-- Following is vehicle flow
type VehicleAPIs =
  "vehicle"
    :> ( Header "authorization" Text
           :> ReqBody '[JSON] CreateVehicleReq
           :> Post '[JSON] CreateVehicleRes
           :<|> "list"
             :> Header "authorization" Text
             :> ReqBody '[JSON] ListVehicleReq
             :> Post '[JSON] ListVehicleRes
       )

vehicleFlow :: FlowServer VehicleAPIs
vehicleFlow =
  Vehicle.createVehicle
    :<|> Vehicle.listVehicles

-- Following is organization creation
type OrganizationAPIs =
  "transporter"
    :> ( Header "authorization" Text
           :> ReqBody '[JSON] TransporterReq
           :> Post '[JSON] TransporterRes
           :<|> "gateway"
             :> Header "authorization" Text
             :> ReqBody '[JSON] TransporterReq
             :> Post '[JSON] GatewayRes
       )

organizationFlow :: FlowServer OrganizationAPIs
organizationFlow =
  Transporter.createTransporter
    :<|> Transporter.createGateway

-----------------------------
-------- Case Flow----------
type CaseAPIs =
  "case"
    :> ( Header "authorization" Text
           :> QueryParam "status" CaseStatus
           :> QueryParam "type" CaseType
           :> QueryParam "limit" Int
           :> QueryParam "offset" Int
           :> Get '[JSON] CaseListRes
           :<|> Header "authorization" Text
             :> Capture "caseId" Text
             :> ReqBody '[JSON] UpdateCaseReq
             :> Post '[JSON] Case
       )

caseFlow =
  Case.list
    :<|> Case.update

-------- CaseProduct Flow----------
type CaseProductAPIs =
  "caseProduct"
    :> ( Header "authorization" Text
           :> QueryParam "status" ProductsStatus
           :> QueryParam "limit" Int
           :> QueryParam "offset" Int
           :> Get '[JSON] CaseProductList
       )

caseProductFlow =
  CaseProduct.list

-------- Product Flow----------
type ProductAPIs =
  "product"
    :> ( Header "authorization" Text
           :> Get '[JSON] ProdListRes
           :<|> Header "authorization" Text
             :> Capture "productId" Text
             :> ReqBody '[JSON] ProdReq
             :> Post '[JSON] ProdInfoRes
           :<|> Header "authorization" Text
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
             :> Header "authorization" Text
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
    :<|> statusApiFlow
    :<|> trackApiFlow
    :<|> caseFlow
    :<|> caseProductFlow
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
