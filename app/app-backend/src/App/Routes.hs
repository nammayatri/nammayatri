{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module App.Routes where

import qualified API.Auth as Auth
import qualified API.Beckn as Beckn
import qualified API.MetroBeckn as MetroBeckn
import qualified API.UI.Booking as Booking
import qualified API.UI.Call as Call
import qualified API.UI.Cancel as Cancel
import qualified API.UI.CancellationReason as CancellationReason
import qualified API.UI.Confirm as Confirm
import qualified API.UI.CustomerSupport as CustomerSupport
import qualified API.UI.Feedback as Feedback
import qualified API.UI.GoogleMaps as GoogleMapsProxy
import qualified API.UI.Profile as Profile
import qualified API.UI.Quote as Quote
import qualified API.UI.Registration as Registration
import qualified API.UI.Route as Route
import qualified API.UI.SavedReqLocation as SavedReqLocation
import qualified API.UI.Search as Search
import qualified API.UI.Select as Select
import qualified API.UI.Serviceability as Serviceability
import qualified API.UI.Support as Support
import qualified App.Routes.Dashboard as Dashboard
import App.Types
import Beckn.Types.Id
import Data.OpenApi (Info (..), OpenApi (..))
import qualified Domain.Types.Ride as SRide
import EulerHS.Prelude
import qualified Product.Ride as Ride
import Servant hiding (throwError)
import Servant.OpenApi
import qualified Types.API.Ride as RideAPI
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
           :<|> Cancel.API
           :<|> RideAPI
           :<|> Call.API
           :<|> Support.API
           :<|> Route.API
           :<|> Serviceability.API
           :<|> Feedback.API
           :<|> CustomerSupport.API
           :<|> GoogleMapsProxy.API
           :<|> CancellationReason.API
           :<|> SavedReqLocation.API
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
    :<|> Auth.handler
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
    :<|> Cancel.handler
    :<|> rideFlow
    :<|> Call.handler
    :<|> Support.handler
    :<|> Route.handler
    :<|> Serviceability.handler
    :<|> Feedback.handler
    :<|> CustomerSupport.handler
    :<|> GoogleMapsProxy.handler
    :<|> CancellationReason.handler
    :<|> SavedReqLocation.handler

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
