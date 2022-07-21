{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module App.Routes where

import App.Types
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.InternalAPI.Auth.API as Auth
import Beckn.Types.APISuccess
import Beckn.Types.App
import qualified Beckn.Types.Core.Metro.API.OnSearch as Metro
import qualified Beckn.Types.Core.Taxi.API.OnConfirm as API
import qualified Beckn.Types.Core.Taxi.API.OnInit as API
import qualified Beckn.Types.Core.Taxi.API.OnSearch as API
import qualified Beckn.Types.Core.Taxi.API.OnSelect as API
import qualified Beckn.Types.Core.Taxi.API.OnTrack as API
import qualified Beckn.Types.Core.Taxi.API.OnUpdate as API
import Beckn.Types.Geofencing
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth
import Data.OpenApi (Info (..), OpenApi (..))
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.CallStatus as SCS
import qualified Domain.Types.CancellationReason as SCancellationReason
import qualified Domain.Types.Quote as Quote
import qualified Domain.Types.RegistrationToken as SRT
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.SearchRequest as SSR
import EulerHS.Prelude
import Product.Auth (authAPI)
import qualified Product.Booking as Booking
import qualified Product.Call as Call
import qualified Product.Cancel as Cancel
import qualified Product.CancellationReason as CancellationReason
import qualified Product.Confirm as Confirm
import qualified Product.CustomerSupport as CS
import qualified Product.Feedback as Feedback
import qualified Product.Init as Init
import qualified Product.Location as Location
import qualified Product.MetroOffer as Metro
import qualified Product.Profile as Profile
import qualified Product.Quote as Quote
import qualified Product.Registration as Registration
import qualified Product.Ride as Ride
import qualified Product.SavedLocations as SavedLocations
import qualified Product.Search as Search
import qualified Product.Select as Select
import qualified Product.Serviceability as Serviceability
import qualified Product.Services.GoogleMaps as GoogleMapsFlow
import qualified Product.Support as Support
import qualified Product.Track as Track
import qualified Product.Update as Update
import Servant hiding (throwError)
import Servant.OpenApi
import qualified Types.API.Booking as BookingAPI
import qualified Types.API.Call as API
import qualified Types.API.CancellationReason as CancellationReasonAPI
import qualified Types.API.CustomerSupport as CustomerSupport
import qualified Types.API.Feedback as Feedback
import qualified Types.API.Location as Location
import qualified Types.API.Profile as Profile
import qualified Types.API.Quote as QuoteAPI
import Types.API.Registration
import qualified Types.API.Ride as RideAPI
import qualified Types.API.SavedLocations as SavedLocationsAPI
import qualified Types.API.Search as Search
import Types.API.Select
import qualified Types.API.Serviceability as Serviceability
import qualified Types.API.Support as Support
import Utils.Auth (TokenAuth)

type AppAPI =
  MainAPI
    :<|> SwaggerAPI

type MainAPI =
  "v2" :> UIAPI
    :<|> "cab" :> "v1" :> BecknCabAPI
    :<|> "metro" :> "v1" :> BecknMetroAPI
    :<|> Auth.API

type BecknMetroAPI =
  SignatureAuth "Authorization"
    :> SignatureAuth "X-Gateway-Authorization"
    :> Metro.OnSearchAPI

type UIAPI =
  Get '[JSON] Text
    :<|> RegistrationAPI
    :<|> ProfileAPI
    :<|> SearchAPI
    :<|> SelectAPI
    :<|> Init.InitAPI
    :<|> QuoteAPI
    :<|> Confirm.ConfirmAPI
    :<|> BookingAPI
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
    :<|> authAPI

uiAPI :: FlowServer UIAPI
uiAPI =
  pure "App is UP"
    :<|> registrationFlow
    :<|> profileFlow
    :<|> searchFlow
    :<|> selectFlow
    :<|> Init.init
    :<|> quoteFlow
    :<|> confirmFlow
    :<|> bookingFlow
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
    :> SignatureAuth "X-Gateway-Authorization"
    :> API.OnSearchAPI
    :<|> SignatureAuth "Authorization"
    :> API.OnSelectAPI
    :<|> SignatureAuth "Authorization"
    :> API.OnInitAPI
    :<|> SignatureAuth "Authorization"
    :> API.OnConfirmAPI
    :<|> SignatureAuth "Authorization"
    :> API.OnUpdateAPI
    :<|> SignatureAuth "Authorization"
    :> API.OnTrackAPI

type SearchAPI =
  "rideSearch"
    :> TokenAuth
    :> ReqBody '[JSON] Search.SearchReq
    :> Post '[JSON] Search.SearchRes

becknCabApi :: FlowServer BecknCabAPI
becknCabApi =
  Search.searchCb
    :<|> Select.onSelect
    :<|> Init.onInit
    :<|> Confirm.onConfirm
    :<|> Update.onUpdate
    :<|> Track.onTrack

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

-------- Select Flow --------
type SelectAPI =
  "rideSearch"
    :> ( TokenAuth
           :> "quotes"
           :> Capture "quoteId" (Id Quote.Quote)
           :> "select"
           :> Post '[JSON] APISuccess
           :<|> TokenAuth
             :> "quotes"
             :> Capture "quoteId" (Id Quote.Quote)
             :> "select"
             :> "list"
             :> Get '[JSON] SelectListRes
       )

selectFlow :: FlowServer SelectAPI
selectFlow =
  Select.select
    :<|> Select.selectList

-------- Confirm Flow --------

confirmFlow :: FlowServer Confirm.ConfirmAPI
confirmFlow =
  Confirm.confirm

type BookingAPI =
  "rideBooking"
    :> ( Capture "rideBookingId" (Id SRB.Booking)
           :> TokenAuth
           :> Post '[JSON] BookingAPI.BookingStatusRes
           :<|> "list"
             :> TokenAuth
             :> QueryParam "limit" Integer
             :> QueryParam "offset" Integer
             :> QueryParam "onlyActive" Bool
             :> Get '[JSON] BookingAPI.BookingListRes
       )

bookingFlow :: FlowServer BookingAPI
bookingFlow =
  Booking.bookingStatus
    :<|> Booking.bookingList

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
    :> Post '[JSON] GoogleMaps.DirectionsResp

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
