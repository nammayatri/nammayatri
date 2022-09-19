{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module App.Routes where

import qualified API.Beckn as Beckn
import qualified API.MetroBeckn as MetroBeckn
import qualified API.UI.Booking as Booking
import qualified API.UI.Confirm as Confirm
import qualified API.UI.Profile as Profile
import qualified API.UI.Quote as Quote
import qualified API.UI.Registration as Registration
import qualified API.UI.Search as Search
import qualified API.UI.Select as Select
import qualified App.Routes.Dashboard as Dashboard
import App.Types
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.InternalAPI.Auth.API as Auth
import Beckn.Types.APISuccess
import Beckn.Types.App
import Beckn.Types.Geofencing
import Beckn.Types.Id
import Data.OpenApi (Info (..), OpenApi (..))
import qualified Domain.Types.CallStatus as SCS
import qualified Domain.Types.CancellationReason as SCancellationReason
import qualified Domain.Types.Ride as SRide
import EulerHS.Prelude
import Product.Auth (authAPI)
import qualified Product.Call as Call
import qualified Product.Cancel as Cancel
import qualified Product.CancellationReason as CancellationReason
import qualified Product.CustomerSupport as CS
import qualified Product.Feedback as Feedback
import qualified Product.Location as Location
import qualified Product.Ride as Ride
import qualified Product.SavedLocations as SavedLocations
import qualified Product.Serviceability as Serviceability
import qualified Product.Services.GoogleMaps as GoogleMapsFlow
import qualified Product.Support as Support
import Servant hiding (throwError)
import Servant.OpenApi
import qualified Types.API.Call as API
import qualified Types.API.CancellationReason as CancellationReasonAPI
import qualified Types.API.CustomerSupport as CustomerSupport
import qualified Types.API.Feedback as Feedback
import qualified Types.API.Location as Location
import qualified Types.API.Ride as RideAPI
import qualified Types.API.SavedLocations as SavedLocationsAPI
import qualified Types.API.Serviceability as Serviceability
import qualified Types.API.Support as Support
import Utils.Auth (TokenAuth)

type AppAPI =
  MainAPI
    :<|> SwaggerAPI

type MainAPI =
  UIAPI
    :<|> Beckn.API
    :<|> MetroBeckn.API
    :<|> Auth.API
    :<|> Dashboard.API

type UIAPI =
  "v2"
    :> ( Get '[JSON] Text
           :<|> Registration.API
           :<|> Profile.API
           :<|> Search.API
           :<|> Select.API
           :<|> Quote.API
           :<|> Confirm.API
           :<|> Booking.API
           :<|> Cancel.CancelAPI
           :<|> RideAPI
           :<|> DeprecatedCallAPIs
           :<|> CallAPIs
           :<|> SupportAPI
           :<|> RouteAPI
           :<|> ServiceabilityAPI
           :<|> FeedbackAPI
           :<|> CustomerSupportAPI
           :<|> GoogleMapsProxyAPI
           :<|> CancellationReasonAPI
           :<|> SavedLocationAPI
       )

appAPI :: Proxy AppAPI
appAPI = Proxy

appServer :: FlowServer AppAPI
appServer =
  mainServer
    :<|> writeSwaggerJSONFlow

mainServer :: FlowServer MainAPI
mainServer =
  uiAPI
    :<|> Beckn.handler
    :<|> MetroBeckn.handler
    :<|> authAPI
    :<|> Dashboard.handler

uiAPI :: FlowServer UIAPI
uiAPI =
  pure "App is UP"
    :<|> Registration.handler
    :<|> Profile.handler
    :<|> Search.handler
    :<|> Select.handler
    :<|> Quote.handler
    :<|> Confirm.handler
    :<|> Booking.handler
    :<|> cancelFlow
    :<|> rideFlow
    :<|> deprecatedCallFlow
    :<|> callFlow
    :<|> supportFlow
    :<|> routeApiFlow
    :<|> serviceabilityFlow
    :<|> feedbackFlow
    :<|> customerSupportFlow
    :<|> googleMapsProxyFlow
    :<|> cancellationReasonFlow
    :<|> savedLocationFlow

-------- Cancel Flow----------

cancelFlow :: FlowServer Cancel.CancelAPI
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
type DeprecatedCallAPIs =
  "ride"
    :> Capture "rideId" (Id SRide.Ride)
    :> "call"
    :> ( "driver"
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

deprecatedCallFlow :: FlowServer DeprecatedCallAPIs
deprecatedCallFlow rideId =
  Call.initiateCallToDriver rideId
    :<|> Call.callStatusCallback rideId
    :<|> Call.getCallStatus rideId

-------- Direct call (Exotel) APIs
type CallAPIs =
  "exotel"
    :> "call"
    :> ( "driver"
           :> "number"
           :> MandatoryQueryParam "CallSid" Text
           :> MandatoryQueryParam "CallFrom" Text
           :> MandatoryQueryParam "CallTo" Text
           :> MandatoryQueryParam "CallStatus" Text
           :> Get '[JSON] API.MobileNumberResp
           :<|> "statusCallback"
           :> MandatoryQueryParam "CallSid" Text
           :> MandatoryQueryParam "DialCallStatus" Text
           :> MandatoryQueryParam "RecordingUrl" Text
           :> QueryParam "Legs[0][OnCallDuration]" Int
           :> Get '[JSON] API.CallCallbackRes
       )

callFlow :: FlowServer CallAPIs
callFlow =
  Call.getDriverMobileNumber
    :<|> Call.directCallStatusCallback

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
             :> QueryParam "language" Text
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
           :> MandatoryQueryParam "cancellationStage" SCancellationReason.CancellationStage
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

type SavedLocationAPI =
  "savedLocation"
    :> ( TokenAuth
           :> ReqBody '[JSON] SavedLocationsAPI.SavedReqLocationAPIEntity
           :> Post '[JSON] APISuccess
           :<|> TokenAuth
             :> "list"
             :> Get '[JSON] SavedLocationsAPI.SavedLocationsListRes
           :<|> TokenAuth
             :> Capture "tag" Text
             :> Delete '[JSON] APISuccess
       )

savedLocationFlow :: FlowServer SavedLocationAPI
savedLocationFlow =
  SavedLocations.createSavedLocationEntity
    :<|> SavedLocations.getSavedLocations
    :<|> SavedLocations.deleteSavedLocation
