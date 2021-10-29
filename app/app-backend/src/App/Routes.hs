{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module App.Routes where

import App.Types
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Types.APISuccess
import Beckn.Types.App
import qualified Beckn.Types.Core.API.Call as API
import qualified Beckn.Types.Core.API.Cancel as API
import qualified Beckn.Types.Core.API.Confirm as API
import qualified Beckn.Types.Core.API.Search as API
import qualified Beckn.Types.Core.API.Status as API
import qualified Beckn.Types.Core.API.Update as API
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth
import EulerHS.Prelude
import qualified Product.Call as Call
import qualified Product.Cancel as Cancel
import qualified Product.CancellationReason as CancellationReason
import qualified Product.Case as Case
import qualified Product.Confirm as Confirm
import qualified Product.CustomerSupport as CS
import qualified Product.Feedback as Feedback
import qualified Product.Info as Info
import qualified Product.Location as Location
import qualified Product.MetroOffer as Metro
import qualified Product.Person as Person
import qualified Product.ProductInstance as ProductInstance
import qualified Product.Registration as Registration
import qualified Product.Search as Search
import qualified Product.Serviceability as Serviceability
import qualified Product.Services.GoogleMaps as GoogleMapsFlow
import qualified Product.Status as Status
import qualified Product.Support as Support
import qualified Product.Update as Update
import Servant hiding (throwError)
import qualified Types.API.Cancel as Cancel
import qualified Types.API.CancellationReason as CancellationReasonAPI
import qualified Types.API.Case as Case
import qualified Types.API.Confirm as ConfirmAPI
import qualified Types.API.CustomerSupport as CustomerSupport
import qualified Types.API.Feedback as Feedback
import qualified Types.API.Location as Location
import qualified Types.API.Person as Person
import Types.API.Product
import qualified Types.API.ProductInstance as ProductInstance
import Types.API.Registration
import qualified Types.API.Search as Search
import qualified Types.API.Serviceability as Serviceability
import qualified Types.API.Support as Support
import Types.Geofencing
import qualified Types.Storage.Case as Case hiding (status)
import Types.Storage.ProductInstance
import Utils.Auth (LookupRegistryOrg, TokenAuth)

type AppAPI =
  UIAPI
    :<|> "cab" :> "v1" :> BecknCabAPI
    :<|> "metro" :> "v1" :> BecknMetroAPI

type BecknMetroAPI =
  SignatureAuth "Authorization" LookupRegistryOrg
    :> SignatureAuth "Proxy-Authorization" LookupRegistryOrg
    :> Metro.OnSearch

type UIAPI =
  "v1"
    :> ( Get '[JSON] Text
           :<|> RegistrationAPI
           :<|> SearchAPI
           :<|> ConfirmAPI
           :<|> CaseAPI
           :<|> InfoAPI
           :<|> ProductInstanceAPI
           :<|> CancelAPI
           :<|> CallAPIs
           :<|> RouteAPI
           :<|> SupportAPI
           :<|> ServiceabilityAPI
           :<|> FeedbackAPI
           :<|> CustomerSupportAPI
           :<|> GoogleMapsProxyAPI
           :<|> PersonAPI
           :<|> CancellationReasonAPI
       )

appAPI :: Proxy AppAPI
appAPI = Proxy

appServer :: FlowServer AppAPI
appServer =
  uiAPI
    :<|> becknCabApi
    :<|> becknMetroAPI

uiAPI :: FlowServer UIAPI
uiAPI =
  pure "App is UP"
    :<|> registrationFlow
    :<|> searchFlow
    :<|> confirmFlow
    :<|> caseFlow
    :<|> infoFlow
    :<|> productInstanceFlow
    :<|> cancelFlow
    :<|> callFlow
    :<|> routeApiFlow
    :<|> supportFlow
    :<|> serviceabilityFlow
    :<|> feedbackFlow
    :<|> customerSupportFlow
    :<|> googleMapsProxyFlow
    :<|> personFlow
    :<|> cancellationReasonFlow

becknMetroAPI :: FlowServer BecknMetroAPI
becknMetroAPI =
  Metro.searchCbMetro

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

-------- Search Flow --------
type SearchAPI =
  "search"
    :> TokenAuth
    :> ReqBody '[JSON] Search.SearchReq
    :> Post '[JSON] Search.SearchRes

type BecknCabAPI =
  SignatureAuth "Authorization" LookupRegistryOrg
    :> SignatureAuth "Proxy-Authorization" LookupRegistryOrg
    :> API.OnSearchAPI
    :<|> SignatureAuth "Authorization" LookupRegistryOrg
    :> "on_confirm"
    :> ReqBody '[JSON] API.OnConfirmReq
    :> Post '[JSON] API.OnConfirmRes
    :<|> SignatureAuth "Authorization" LookupRegistryOrg
    :> "on_update"
    :> ReqBody '[JSON] API.OnUpdateReq
    :> Post '[JSON] API.OnUpdateRes
    :<|> SignatureAuth "Authorization" LookupRegistryOrg
    :> "on_cancel"
    :> ReqBody '[JSON] API.OnCancelReq
    :> Post '[JSON] API.OnCancelRes
    :<|> SignatureAuth "Authorization" LookupRegistryOrg
    :> "on_status"
    :> ReqBody '[JSON] API.OnStatusReq
    :> Post '[JSON] API.OnStatusRes

becknCabApi :: FlowServer BecknCabAPI
becknCabApi =
  Search.searchCb
    :<|> Confirm.onConfirm
    :<|> Update.onUpdate
    :<|> Cancel.onCancel
    :<|> Status.onStatus

searchFlow :: FlowServer SearchAPI
searchFlow =
  Search.search

-------- Confirm Flow --------
type ConfirmAPI =
  ( "confirm"
      :> TokenAuth
      :> ReqBody '[JSON] ConfirmAPI.ConfirmReq
      :> Post '[JSON] ConfirmAPI.ConfirmRes
  )

confirmFlow :: FlowServer ConfirmAPI
confirmFlow =
  Confirm.confirm

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
           :> Get '[JSON] Case.GetStatusRes
       )

caseFlow :: FlowServer CaseAPI
caseFlow regToken =
  Case.list regToken
    :<|> Case.getStatus regToken

-------- Info Flow ------
type InfoAPI =
  TokenAuth
    :> ( "product"
           :> Capture "id" (Id ProductInstance)
           :> Get '[JSON] GetProductInfoRes
           :<|> "location"
           :> Capture "caseId" (Id Case.Case)
           :> Get '[JSON] Location.GetLocationRes
       )

infoFlow :: FlowServer InfoAPI
infoFlow regToken =
  Info.getProductInfo regToken
    :<|> Info.getLocation regToken

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

productInstanceFlow :: FlowServer ProductInstanceAPI
productInstanceFlow =
  ProductInstance.list

-------- Cancel Flow----------
type CancelAPI =
  "cancel"
    :> TokenAuth
    :> ReqBody '[JSON] Cancel.CancelReq
    :> Post '[JSON] Cancel.CancelRes

cancelFlow :: FlowServer CancelAPI
cancelFlow =
  Cancel.cancel

-------- Initiate a call (Exotel) APIs --------
type CallAPIs =
  "call"
    :> ( "to_provider"
           :> TokenAuth
           :> ReqBody '[JSON] API.CallReq
           :> Post '[JSON] API.CallRes
           :<|> "to_customer"
           :> ReqBody '[JSON] API.CallReq
           :> Post '[JSON] API.CallRes
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
customerSupportFlow =
  CS.login
    :<|> CS.logout
    :<|> CS.listOrder

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

type PersonAPI =
  "person"
    :> ( TokenAuth
           :> Get '[JSON] Person.GetPersonDetailsRes
           :<|> "update"
             :> TokenAuth
             :> ReqBody '[JSON] Person.UpdateReq
             :> Post '[JSON] APISuccess
       )

personFlow :: FlowServer PersonAPI
personFlow =
  Person.getPersonDetails
    :<|> Person.updatePerson

type CancellationReasonAPI =
  "cancellationReason"
    :> ( "list"
           :> TokenAuth
           :> Get '[JSON] CancellationReasonAPI.ListRes
       )

cancellationReasonFlow :: FlowServer CancellationReasonAPI
cancellationReasonFlow = CancellationReason.list
