{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module App.Routes where

import App.Types
import qualified Beckn.Types.API.Call as Call
import qualified Beckn.Types.API.Cancel as Cancel (OnCancelReq (..), OnCancelRes)
import qualified Beckn.Types.API.Confirm as Confirm
import qualified Beckn.Types.API.Search as Search
import qualified Beckn.Types.API.Status as Status
import Beckn.Types.API.Track
import qualified Beckn.Types.API.Update as Update
import Beckn.Types.App
import Beckn.Types.Common (AckResponse (..))
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import Beckn.Types.Storage.ProductInstance
import EulerHS.Prelude
import qualified Product.Call as Call
import qualified Product.Cancel as Cancel
import qualified Product.Case as Case
import qualified Product.Confirm as Confirm
import qualified Product.Cron as Cron
import qualified Product.Info as Info
import qualified Product.Location as Location
import qualified Product.ProductInstance as ProductInstance
import qualified Product.Registration as Registration
import qualified Product.Search as Search
import qualified Product.Status as Status
import qualified Product.TrackTrip as TrackTrip
import qualified Product.Update as Update
import Servant
import qualified Types.API.Cancel as Cancel
import qualified Types.API.Case as Case
import qualified Types.API.Confirm as ConfirmAPI
import qualified Types.API.Cron as Cron
import qualified Types.API.Location as Location
import Types.API.Product
import qualified Types.API.ProductInstance as ProductInstance
import Types.API.Registration
import qualified Types.API.Search as Search'
import Types.API.Status
import Utils.Auth (VerifyAPIKey)
import Utils.Common (TokenAuth)

type AppAPI =
  "v1"
    :> ( Get '[JSON] Text
           :<|> RegistrationAPI
           :<|> SearchAPI
           :<|> ConfirmAPI
           :<|> CaseAPI
           :<|> InfoAPI
           :<|> TrackTripAPI
           :<|> UpdateAPI
           :<|> ProductInstanceAPI
           :<|> CancelAPI
           :<|> CronAPI
           :<|> CallAPIs
           :<|> RouteAPI
           :<|> StatusAPI
       )

appAPI :: Proxy AppAPI
appAPI = Proxy

appServer :: FlowServer AppAPI
appServer =
  pure "App is UP"
    :<|> registrationFlow
    :<|> searchFlow
    :<|> confirmFlow
    :<|> caseFlow
    :<|> infoFlow
    :<|> trackTripFlow
    :<|> updateFlow
    :<|> productInstanceFlow
    :<|> cancelFlow
    :<|> cronFlow
    :<|> callFlow
    :<|> routeApiFlow
    :<|> statusFlow

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

-------- Search Flow --------
type SearchAPI =
  "search"
    :> TokenAuth
    :> ReqBody '[JSON] Search.SearchReq
    :> Post '[JSON] Search'.AckResponse
    :<|> Search.OnSearchAPI VerifyAPIKey

searchFlow :: FlowServer SearchAPI
searchFlow =
  Search.search
    :<|> Search.searchCb

-------- Confirm Flow --------
type ConfirmAPI =
  ( "confirm"
      :> TokenAuth
      :> ReqBody '[JSON] ConfirmAPI.ConfirmReq
      :> Post '[JSON] AckResponse
      :<|> "on_confirm"
      :> ReqBody '[JSON] Confirm.OnConfirmReq
      :> Post '[JSON] Confirm.OnConfirmRes
  )

confirmFlow :: FlowServer ConfirmAPI
confirmFlow =
  Confirm.confirm
    :<|> Confirm.onConfirm

------- Case Flow -------
type CaseAPI =
  "case"
    :> TokenAuth
    :> ( "list"
           :> MandatoryQueryParam "type" Case.CaseType
           :> QueryParams "status" Case.CaseStatus
           :> QueryParam "limit" Integer
           :> QueryParam "offset" Integer
           :> Get '[JSON] Case.CaseListRes
           :<|> Capture "caseId" CaseId
           :> Get '[JSON] Case.StatusRes
       )

caseFlow :: FlowServer CaseAPI
caseFlow regToken =
  Case.list regToken
    :<|> Case.status regToken

-------- Info Flow ------
type InfoAPI =
  TokenAuth
    :> ( "product"
           :> Capture "id" Text
           :> Get '[JSON] GetProductInfoRes
           :<|> "location"
           :> Capture "caseId" Text
           :> Get '[JSON] Location.GetLocationRes
       )

infoFlow :: FlowServer InfoAPI
infoFlow regToken =
  Info.getProductInfo regToken
    :<|> Info.getLocation regToken

------- Track trip Flow -------
type TrackTripAPI =
  "track"
    :> TokenAuth
    :> ReqBody '[JSON] TrackTripReq
    :> Post '[JSON] TrackTripRes
    :<|> "on_track"
    :> ReqBody '[JSON] OnTrackTripReq
    :> Post '[JSON] OnTrackTripRes

trackTripFlow :: FlowServer TrackTripAPI
trackTripFlow =
  TrackTrip.track
    :<|> TrackTrip.trackCb

------- Update Flow -------
type UpdateAPI =
  "on_update"
    :> ReqBody '[JSON] Update.OnUpdateReq
    :> Post '[JSON] Update.OnUpdateRes

updateFlow :: FlowServer UpdateAPI
updateFlow =
  Update.onUpdate

-------- ProductInstance Flow----------
type ProductInstanceAPI =
  "productInstance"
    :> ( TokenAuth
           :> QueryParams "status" ProductInstanceStatus
           :> QueryParams "type" Case.CaseType
           :> QueryParam "limit" Int
           :> QueryParam "offset" Int
           :> Get '[JSON] ProductInstance.ProductInstanceList
       )

productInstanceFlow ::
  Person.Person ->
  [ProductInstanceStatus] ->
  [Case.CaseType] ->
  Maybe Int ->
  Maybe Int ->
  FlowHandler ProductInstance.ProductInstanceList
productInstanceFlow =
  ProductInstance.list

-------- Cancel Flow----------
type CancelAPI =
  "cancel"
    :> TokenAuth
    :> ReqBody '[JSON] Cancel.CancelReq
    :> Post '[JSON] Cancel.CancelRes
    -- on cancel
    :<|> "on_cancel"
    :> ReqBody '[JSON] Cancel.OnCancelReq
    :> Post '[JSON] Cancel.OnCancelRes

cancelFlow ::
  ( Person.Person ->
    Cancel.CancelReq ->
    FlowHandler Cancel.CancelRes
  )
    :<|> (Cancel.OnCancelReq -> FlowHandler Cancel.OnCancelRes)
cancelFlow =
  Cancel.cancel
    :<|> Cancel.onCancel

-------- Cron API --------
type CronAPI =
  "cron"
    :> "expire_cases"
    :> Header "Authorization" CronAuthKey
    :> ReqBody '[JSON] Cron.ExpireCaseReq
    :> Post '[JSON] Cron.ExpireCaseRes

cronFlow :: FlowServer CronAPI
cronFlow =
  Cron.updateCases

-------- Initiate a call (Exotel) APIs --------
type CallAPIs =
  "call"
    :> ( "to_provider"
           :> TokenAuth
           :> ReqBody '[JSON] Call.CallReq
           :> Post '[JSON] Call.CallRes
           :<|> "to_customer"
           :> ReqBody '[JSON] Call.CallReq
           :> Post '[JSON] Call.CallRes
       )

callFlow :: FlowServer CallAPIs
callFlow =
  Call.initiateCallToProvider
    :<|> Call.initiateCallToCustomer

type RouteAPI =
  "route"
    :> TokenAuth
    :> ReqBody '[JSON] Location.Request
    :> Post '[JSON] Location.Response

routeApiFlow :: FlowServer RouteAPI
routeApiFlow = Location.getRoute

-------- Status Flow----------
type StatusAPI =
  "status"
    :> TokenAuth
    :> ReqBody '[JSON] StatusReq
    :> Post '[JSON] StatusRes
    :<|> "on_status"
    :> ReqBody '[JSON] Status.OnStatusReq
    :> Post '[JSON] Status.OnStatusRes

statusFlow ::
  ( Person.Person ->
    StatusReq ->
    FlowHandler StatusRes
  )
    :<|> (Status.OnStatusReq -> FlowHandler Status.OnStatusRes)
statusFlow =
  Status.status
    :<|> Status.onStatus
