{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module App.Routes where

import App.Types
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import qualified Beckn.Types.APISuccess as APISuccess
import Beckn.Types.App
import qualified Beckn.Types.Core.API.Call as Call
import qualified Beckn.Types.Core.API.Cancel as Cancel (OnCancelReq, OnCancelRes)
import qualified Beckn.Types.Core.API.Confirm as Confirm
import qualified Beckn.Types.Core.API.Search as Search
import qualified Beckn.Types.Core.API.Status as Status
import Beckn.Types.Core.API.Track
import qualified Beckn.Types.Core.API.Update as Update
import Beckn.Types.Core.Ack (AckResponse (..))
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Person as Person
import Beckn.Types.Storage.ProductInstance
import Beckn.Utils.Servant.SignatureAuth
import qualified Beckn.Utils.Servant.SignatureAuth as HttpSig
import EulerHS.Prelude
import qualified Product.Call as Call
import qualified Product.Cancel as Cancel
import qualified Product.Case as Case
import qualified Product.Confirm as Confirm
import qualified Product.Cron as Cron
import qualified Product.CustomerSupport as CS
import qualified Product.Feedback as Feedback
import qualified Product.Info as Info
import qualified Product.Location as Location
import qualified Product.Person as Person
import qualified Product.ProductInstance as ProductInstance
import qualified Product.Registration as Registration
import qualified Product.Search as Search
import qualified Product.Serviceability as Serviceability
import qualified Product.Services.GoogleMaps as GoogleMapsFlow
import qualified Product.Status as Status
import qualified Product.Support as Support
import qualified Product.TrackTrip as TrackTrip
import qualified Product.Update as Update
import Servant
import qualified Types.API.Cancel as Cancel
import qualified Types.API.Case as Case
import qualified Types.API.Confirm as ConfirmAPI
import qualified Types.API.Cron as Cron
import qualified Types.API.CustomerSupport as CustomerSupport
import qualified Types.API.Feedback as Feedback
import qualified Types.API.Location as Location
import qualified Types.API.Person as Person
import Types.API.Product
import qualified Types.API.ProductInstance as ProductInstance
import Types.API.Registration
import qualified Types.API.Search as Search'
import qualified Types.API.Serviceability as Serviceability
import Types.API.Status
import qualified Types.API.Support as Support
import Types.Geofencing
import Utils.Auth
  ( lookup,
  )
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
           :<|> SupportAPI
           :<|> ServiceabilityAPI
           :<|> FeedbackAPI
           :<|> CustomerSupportAPI
           :<|> GoogleMapsProxyAPI
           :<|> PersonAPI
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
    :<|> supportFlow
    :<|> serviceabilityFlow
    :<|> feedbackFlow
    :<|> customerSupportFlow
    :<|> googleMapsProxyFlow
    :<|> personFlow

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
    :> ReqBody '[JSON] Search'.SearchReq
    :> Post '[JSON] Search'.AckResponse
    :<|> SignatureAuth "Authorization"
    :> SignatureAuth "Proxy-Authorization"
    :> Search.OnSearchAPI

searchFlow :: FlowServer SearchAPI
searchFlow =
  Search.search
    :<|> HttpSig.withBecknAuthProxy Search.searchCb lookup

-------- Confirm Flow --------
type ConfirmAPI =
  ( "confirm"
      :> TokenAuth
      :> ReqBody '[JSON] ConfirmAPI.ConfirmReq
      :> Post '[JSON] AckResponse
      :<|> SignatureAuth "Authorization"
      :> "on_confirm"
      :> ReqBody '[JSON] Confirm.OnConfirmReq
      :> Post '[JSON] Confirm.OnConfirmRes
  )

confirmFlow :: FlowServer ConfirmAPI
confirmFlow =
  Confirm.confirm
    :<|> HttpSig.withBecknAuth Confirm.onConfirm lookup

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
           :<|> Capture "caseId" (Id Case.Case)
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
    :<|> SignatureAuth "Authorization"
    :> "on_track"
    :> ReqBody '[JSON] OnTrackTripReq
    :> Post '[JSON] OnTrackTripRes

trackTripFlow :: FlowServer TrackTripAPI
trackTripFlow =
  TrackTrip.track
    :<|> HttpSig.withBecknAuth TrackTrip.trackCb lookup

------- Update Flow -------
type UpdateAPI =
  SignatureAuth "Authorization"
    :> "on_update"
    :> ReqBody '[JSON] Update.OnUpdateReq
    :> Post '[JSON] Update.OnUpdateRes

updateFlow :: FlowServer UpdateAPI
updateFlow =
  HttpSig.withBecknAuth Update.onUpdate lookup

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
    :<|> SignatureAuth "Authorization"
    :> "on_cancel"
    :> ReqBody '[JSON] Cancel.OnCancelReq
    :> Post '[JSON] Cancel.OnCancelRes

cancelFlow :: FlowServer CancelAPI
cancelFlow =
  Cancel.cancel
    :<|> HttpSig.withBecknAuth Cancel.onCancel lookup

-------- Cron API --------
type CronAPI =
  "cron"
    :> "expire_cases"
    :> Header "Authorization" CronAuthKey
    :> ReqBody '[JSON] Cron.ExpireCaseReq
    :> Post '[JSON] Cron.ExpireRes
    :<|> "expire_product_instances"
    :> Header "Authorization" CronAuthKey
    :> Post '[JSON] Cron.ExpireRes

cronFlow :: FlowServer CronAPI
cronFlow =
  Cron.updateCases
    :<|> Cron.expireProductInstances

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
    :<|> SignatureAuth "Authorization"
    :> "on_status"
    :> ReqBody '[JSON] Status.OnStatusReq
    :> Post '[JSON] Status.OnStatusRes

statusFlow :: FlowServer StatusAPI
statusFlow =
  Status.status
    :<|> HttpSig.withBecknAuth Status.onStatus lookup

-------- Support Flow----------
type SupportAPI =
  "support"
    :> ( "sendIssue"
           :> TokenAuth
           :> ReqBody '[JSON] Support.SendIssueReq
           :> Post '[JSON] Support.SendIssueRes
       )

supportFlow :: FlowServer SupportAPI
supportFlow = Support.sendIssue

-------- Serviceability----------
type ServiceabilityAPI =
  "serviceability"
    :> TokenAuth
    :> ( "origin"
           :> ReqBody '[JSON] Serviceability.ServiceabilityReq
           :> Post '[JSON] Serviceability.ServiceabilityRes
           :<|> "destination"
             :> ReqBody '[JSON] Serviceability.ServiceabilityReq
             :> Post '[JSON] Serviceability.ServiceabilityRes
           :<|> "ride"
             :> ReqBody '[JSON] Serviceability.RideServiceabilityReq
             :> Post '[JSON] Serviceability.RideServiceabilityRes
       )

serviceabilityFlow :: FlowServer ServiceabilityAPI
serviceabilityFlow regToken =
  Serviceability.checkServiceability origin regToken
    :<|> Serviceability.checkServiceability destination regToken
    :<|> Serviceability.checkRideServiceability regToken

-------- Feedback Flow ----------
type FeedbackAPI =
  "feedback"
    :> ( "rateRide"
           :> TokenAuth
           :> ReqBody '[JSON] Feedback.FeedbackReq
           :> Post '[JSON] Feedback.FeedbackRes
       )

feedbackFlow :: FlowServer FeedbackAPI
feedbackFlow = Feedback.feedback

-- Customer Support Flow --

type CustomerSupportAPI =
  "customerSupport"
    :> ( "login"
           :> ReqBody '[JSON] CustomerSupport.LoginReq
           :> Post '[JSON] CustomerSupport.LoginRes
           :<|> "logout"
             :> TokenAuth
             :> Post '[JSON] CustomerSupport.LogoutRes
           :<|> "orders"
             :> TokenAuth
             :> QueryParam "id" Text
             :> QueryParam "phone" Text
             :> QueryParam "limit" Integer
             :> QueryParam "offset" Integer
             :> Get '[JSON] [CustomerSupport.OrderResp]
       )

customerSupportFlow :: FlowServer CustomerSupportAPI
customerSupportFlow = CS.login :<|> CS.logout :<|> CS.listOrder

type GoogleMapsProxyAPI =
  "googleMaps"
    :> ( "autoComplete"
           :> TokenAuth
           :> MandatoryQueryParam "input" Text
           :> MandatoryQueryParam "location" Text -- Passing it as <latitude>,<longitude>
           :> MandatoryQueryParam "radius" Integer
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

type PersonAPI =
  "person"
    :> ( TokenAuth
           :> Get '[JSON] Person.GetPersonDetailsRes
           :<|> "update"
             :> TokenAuth
             :> ReqBody '[JSON] Person.UpdateReq
             :> Post '[JSON] APISuccess.APISuccess
       )

personFlow :: FlowServer PersonAPI
personFlow =
  Person.getPersonDetails
    :<|> Person.updatePerson
