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
import Data.OpenApi (Info (..), OpenApi (..))
import EulerHS.Prelude
import qualified Product.Call as Call
import qualified Product.Cancel as Cancel
import qualified Product.CancellationReason as CancellationReason
import qualified Product.Confirm as Confirm
import qualified Product.CustomerSupport as CS
import qualified Product.Feedback as Feedback
import qualified Product.Location as Location
import qualified Product.MetroOffer as Metro
import qualified Product.Profile as Profile
import qualified Product.Quote as Quote
import qualified Product.Registration as Registration
import qualified Product.Ride as Ride
import qualified Product.RideBooking as RideBooking
import qualified Product.Search as Search
import qualified Product.Serviceability as Serviceability
import qualified Product.Services.GoogleMaps as GoogleMapsFlow
import qualified Product.Status as Status
import qualified Product.Support as Support
import qualified Product.Update as Update
import Servant hiding (throwError)
import Servant.OpenApi
import qualified Types.API.Cancel as Cancel
import qualified Types.API.CancellationReason as CancellationReasonAPI
import qualified Types.API.Confirm as ConfirmAPI
import qualified Types.API.CustomerSupport as CustomerSupport
import qualified Types.API.Feedback as Feedback
import qualified Types.API.Location as Location
import qualified Types.API.Profile as Profile
import qualified Types.API.Quote as QuoteAPI
import Types.API.Registration
import qualified Types.API.Ride as RideAPI
import qualified Types.API.RideBooking as RideBookingAPI
import qualified Types.API.Search as Search
import qualified Types.API.Serviceability as Serviceability
import qualified Types.API.Support as Support
import Types.Geofencing
import qualified Types.Storage.Quote as Quote
import qualified Types.Storage.RegistrationToken as SRT
import qualified Types.Storage.Ride as SRide
import qualified Types.Storage.RideBooking as SRB
import qualified Types.Storage.SearchRequest as SSR
import Utils.Auth (TokenAuth)

type AppAPI =
  MainAPI
    :<|> SwaggerAPI

type MainAPI =
  "v2" :> UIAPI
    :<|> "cab" :> "v1" :> BecknCabAPI
    :<|> "metro" :> "v1" :> BecknMetroAPI

type BecknMetroAPI =
  SignatureAuth "Authorization"
    :> SignatureAuth "Proxy-Authorization"
    :> Metro.OnSearch

type UIAPI =
  Get '[JSON] Text
    :<|> RegistrationAPI
    :<|> ProfileAPI
    :<|> SearchAPI
    :<|> QuoteAPI
    :<|> ConfirmAPI
    :<|> RideBookingAPI
    :<|> CancelAPI
    :<|> RideAPI
    :<|> CallAPIs
    :<|> SupportAPI
    :<|> RouteAPI
    :<|> ServiceabilityAPI
    :<|> FeedbackAPI
    :<|> CustomerSupportAPI
    :<|> GoogleMapsProxyAPI
    :<|> CancellationReasonAPI

appAPI :: Proxy AppAPI
appAPI = Proxy

appServer :: FlowServer AppAPI
appServer =
  mainServer
    :<|> writeSwaggerJSONFlow

mainServer :: FlowServer MainAPI
mainServer =
  uiAPI
    :<|> becknCabApi
    :<|> becknMetroAPI

uiAPI :: FlowServer UIAPI
uiAPI =
  pure "App is UP"
    :<|> registrationFlow
    :<|> profileFlow
    :<|> searchFlow
    :<|> quoteFlow
    :<|> confirmFlow
    :<|> rideBookingFlow
    :<|> cancelFlow
    :<|> rideFlow
    :<|> callFlow
    :<|> supportFlow
    :<|> routeApiFlow
    :<|> serviceabilityFlow
    :<|> feedbackFlow
    :<|> customerSupportFlow
    :<|> googleMapsProxyFlow
    :<|> cancellationReasonFlow

becknMetroAPI :: FlowServer BecknMetroAPI
becknMetroAPI =
  Metro.searchCbMetro

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

type ProfileAPI =
  "profile"
    :> ( TokenAuth
           :> Get '[JSON] Profile.ProfileRes
           :<|> TokenAuth
             :> ReqBody '[JSON] Profile.UpdateProfileReq
             :> Post '[JSON] APISuccess
       )

profileFlow :: FlowServer ProfileAPI
profileFlow =
  Profile.getPersonDetails
    :<|> Profile.updatePerson

-------- Search Flow --------

type BecknCabAPI =
  SignatureAuth "Authorization"
    :> SignatureAuth "Proxy-Authorization"
    :> API.OnSearchAPI
    :<|> SignatureAuth "Authorization"
    :> "on_confirm"
    :> ReqBody '[JSON] API.OnConfirmReq
    :> Post '[JSON] API.OnConfirmRes
    :<|> SignatureAuth "Authorization"
    :> "on_update"
    :> ReqBody '[JSON] API.OnUpdateReq
    :> Post '[JSON] API.OnUpdateRes
    :<|> SignatureAuth "Authorization"
    :> "on_cancel"
    :> ReqBody '[JSON] API.OnCancelReq
    :> Post '[JSON] API.OnCancelRes
    :<|> SignatureAuth "Authorization"
    :> "on_status"
    :> ReqBody '[JSON] API.OnStatusReq
    :> Post '[JSON] API.OnStatusRes

type SearchAPI =
  "rideSearch"
    :> TokenAuth
    :> ReqBody '[JSON] Search.SearchReq
    :> Post '[JSON] Search.SearchRes

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

type QuoteAPI =
  "rideSearch"
    :> Capture "searchId" (Id SSR.SearchRequest)
    :> TokenAuth
    :> "quotes"
    :> Get '[JSON] QuoteAPI.GetQuotesRes

quoteFlow :: FlowServer QuoteAPI
quoteFlow =
  Quote.getQuotes

-------- Confirm Flow --------
type ConfirmAPI =
  "rideSearch"
    :> TokenAuth
    :> Capture "searchId" (Id SSR.SearchRequest)
    :> "quotes"
    :> Capture "quoteId" (Id Quote.Quote)
    :> "confirm"
    :> Post '[JSON] ConfirmAPI.ConfirmRes

confirmFlow :: FlowServer ConfirmAPI
confirmFlow =
  Confirm.confirm

type RideBookingAPI =
  "rideBooking"
    :> ( Capture "bookingId" (Id SRB.RideBooking)
           :> TokenAuth
           :> Post '[JSON] RideBookingAPI.RideBookingStatusRes
           :<|> "list"
             :> TokenAuth
             :> QueryParam "limit" Integer
             :> QueryParam "offset" Integer
             :> QueryParam "onlyActive" Bool
             :> Get '[JSON] RideBookingAPI.RideBookingListRes
       )

rideBookingFlow :: FlowServer RideBookingAPI
rideBookingFlow =
  RideBooking.rideBookingStatus
    :<|> RideBooking.rideBookingList

-------- Cancel Flow----------

type CancelAPI =
  "rideBooking"
    :> Capture "bookingId" (Id SRB.RideBooking)
    :> "cancel"
    :> TokenAuth
    :> ReqBody '[JSON] Cancel.CancelReq
    :> Post '[JSON] Cancel.CancelRes

cancelFlow :: FlowServer CancelAPI
cancelFlow =
  Cancel.cancel

type RideAPI =
  "ride"
    :> Capture "rideId" (Id SRide.Ride)
    :> "driver"
    :> "location"
    :> TokenAuth
    :> Post '[JSON] RideAPI.GetDriverLocRes

rideFlow :: FlowServer RideAPI
rideFlow =
  Ride.getDriverLoc

-------- Initiate a call (Exotel) APIs --------
type CallAPIs =
  "ride"
    :> Capture "rideId" (Id SRide.Ride)
    :> "call"
    :> ( "driver"
           :> TokenAuth
           :> Post '[JSON] API.CallRes
           :<|> "rider"
           :> Post '[JSON] API.CallRes
       )

callFlow :: FlowServer CallAPIs
callFlow rideId =
  Call.initiateCallToDriver rideId
    :<|> Call.initiateCallToCustomer rideId

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

------- Update Flow -------
type UpdateAPI =
  SignatureAuth "Authorization"
    :> "on_update"
    :> ReqBody '[JSON] API.OnUpdateReq
    :> Post '[JSON] API.OnUpdateRes

updateFlow :: FlowServer UpdateAPI
updateFlow =
  Update.onUpdate

type RouteAPI =
  "route"
    :> TokenAuth
    :> ReqBody '[JSON] Location.Request
    :> Post '[JSON] Location.Response

routeApiFlow :: FlowServer RouteAPI
routeApiFlow = Location.getRoute

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
       )

serviceabilityFlow :: FlowServer ServiceabilityAPI
serviceabilityFlow regToken =
  Serviceability.checkServiceability origin regToken
    :<|> Serviceability.checkServiceability destination regToken

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

type CancellationReasonAPI =
  "cancellationReason"
    :> ( "list"
           :> TokenAuth
           :> Get '[JSON] CancellationReasonAPI.ListRes
       )

cancellationReasonFlow :: FlowServer CancellationReasonAPI
cancellationReasonFlow = CancellationReason.list

type SwaggerAPI = "swagger" :> Get '[JSON] OpenApi

swagger :: OpenApi
swagger = do
  let openApi = toOpenApi (Proxy :: Proxy MainAPI)
  openApi
    { _openApiInfo =
        (_openApiInfo openApi)
          { _infoTitle = "Yatri",
            _infoVersion = "1.0"
          }
    }

writeSwaggerJSONFlow :: FlowServer SwaggerAPI
writeSwaggerJSONFlow = return swagger
