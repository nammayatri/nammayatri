module API.UI.GoogleMaps
  ( API,
    handler,
  )
where

import qualified Beckn.External.GoogleMaps.Client as ClientGoogleMaps
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Types.App
import Beckn.Types.Id
import Beckn.Utils.Logging
import qualified Domain.Types.Person as Person
import Environment (FlowHandler, FlowServer)
import EulerHS.Prelude
import Servant
import Utils.Auth
import Utils.Common (withFlowHandlerAPI)

type API =
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

handler :: FlowServer API
handler =
  autoComplete
    :<|> placeDetails
    :<|> getPlaceName

autoComplete :: Id Person.Person -> Text -> Text -> Integer -> Text -> FlowHandler GoogleMaps.SearchLocationResp
autoComplete personId input location radius lang = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  url <- asks (.googleMapsUrl)
  apiKey <- asks (.googleMapsKey)
  let components = "country:in"
  ClientGoogleMaps.autoComplete url apiKey input location radius components lang

placeDetails :: Id Person.Person -> Text -> FlowHandler GoogleMaps.PlaceDetailsResp
placeDetails personId placeId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  url <- asks (.googleMapsUrl)
  apiKey <- asks (.googleMapsKey)
  let fields = "geometry"
  ClientGoogleMaps.placeDetails url apiKey placeId fields

getPlaceName :: Id Person.Person -> Text -> Maybe Text -> FlowHandler GoogleMaps.GetPlaceNameResp
getPlaceName personId latLng lang = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  url <- asks (.googleMapsUrl)
  apiKey <- asks (.googleMapsKey)
  ClientGoogleMaps.getPlaceName url latLng apiKey $ GoogleMaps.toMbLanguage lang
