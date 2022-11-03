module API.UI.GoogleMaps
  ( API,
    handler,
  )
where

import qualified Beckn.External.GoogleMaps.Client as ClientGoogleMaps
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Types.App
import Beckn.Types.Id
import Beckn.Utils.Common (withFlowHandlerAPI)
import Beckn.Utils.Logging
import qualified Domain.Types.Person as Person
import Environment (FlowHandler, FlowServer)
import EulerHS.Prelude
import Servant
import Tools.Auth

type API =
  "googleMaps"
    :> ( "autoComplete"
           :> TokenAuth
           :> Header "sessiontoken" Text
           :> MandatoryQueryParam "input" Text
           :> MandatoryQueryParam "location" Text -- Passing it as <latitude>,<longitude>
           :> MandatoryQueryParam "radius" Integer
           :> MandatoryQueryParam "language" Text
           :> Get '[JSON] GoogleMaps.SearchLocationResp
           :<|> "placeDetails"
             :> TokenAuth
             :> Header "sessiontoken" Text
             :> MandatoryQueryParam "place_id" Text
             :> Get '[JSON] GoogleMaps.PlaceDetailsResp
           :<|> "getPlaceName"
             :> TokenAuth
             :> Header "sessiontoken" Text
             :> MandatoryQueryParam "latlng" Text -- Passing it as <latitude>,<longitude>
             :> QueryParam "language" Text
             :> Get '[JSON] GoogleMaps.GetPlaceNameResp
           :<|> "getCoordinates"
             :> TokenAuth
             :> Header "sessiontoken" Text
             :> MandatoryQueryParam "place_id" Text
             :> QueryParam "language" Text
             :> Get '[JSON] GoogleMaps.GetPlaceNameResp
       )

handler :: FlowServer API
handler =
  autoComplete
    :<|> placeDetails
    :<|> getPlaceName
    :<|> getCoordinates

autoComplete :: Id Person.Person -> Maybe Text -> Text -> Text -> Integer -> Text -> FlowHandler GoogleMaps.SearchLocationResp
autoComplete personId sessiontoken input location radius lang = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  url <- asks (.googleMapsUrl)
  apiKey <- asks (.googleMapsKey)
  let components = "country:in"
  ClientGoogleMaps.autoComplete url apiKey input sessiontoken location radius components lang

placeDetails :: Id Person.Person -> Maybe Text -> Text -> FlowHandler GoogleMaps.PlaceDetailsResp
placeDetails personId sessiontoken placeId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  url <- asks (.googleMapsUrl)
  apiKey <- asks (.googleMapsKey)
  let fields = "geometry"
  ClientGoogleMaps.placeDetails url sessiontoken apiKey placeId fields

getPlaceName :: Id Person.Person -> Maybe Text -> Text -> Maybe Text -> FlowHandler GoogleMaps.GetPlaceNameResp
getPlaceName personId sessiontoken latLng lang = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  url <- asks (.googleMapsUrl)
  apiKey <- asks (.googleMapsKey)
  ClientGoogleMaps.getPlaceName url sessiontoken latLng apiKey $ GoogleMaps.toMbLanguage lang

getCoordinates :: Id Person.Person -> Maybe Text -> Text -> Maybe Text -> FlowHandler GoogleMaps.GetPlaceNameResp
getCoordinates personId sessiontoken placeId lang = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  url <- asks (.googleMapsUrl)
  apiKey <- asks (.googleMapsKey)
  ClientGoogleMaps.getCoordinates url sessiontoken placeId apiKey $ GoogleMaps.toMbLanguage lang
