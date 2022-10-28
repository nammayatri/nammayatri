module API.UI.GoogleMaps
  ( API,
    handler,
  )
where

import qualified Beckn.External.Maps.Google as ClientGoogleMaps
import qualified Beckn.External.Maps.Google as GoogleMaps
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
           :> MandatoryQueryParam "language" GoogleMaps.Language
           :> Get '[JSON] GoogleMaps.SearchLocationResp
           :<|> "placeDetails"
             :> TokenAuth
             :> Header "sessiontoken" Text
             :> MandatoryQueryParam "place_id" Text
             :> Get '[JSON] GoogleMaps.PlaceDetailsResp
           :<|> "getPlaceName"
             :> TokenAuth
             :> Header "sessiontoken" Text
             :> MandatoryQueryParam "latlng" ClientGoogleMaps.LatLong -- Passing it as <latitude>,<longitude>
             :> QueryParam "language" GoogleMaps.Language
             :> Get '[JSON] GoogleMaps.GetPlaceNameResp
           :<|> "getCoordinates"
             :> TokenAuth
             :> Header "sessiontoken" Text
             :> MandatoryQueryParam "place_id" Text
             :> QueryParam "language" GoogleMaps.Language
             :> Get '[JSON] GoogleMaps.GetPlaceNameResp
       )

handler :: FlowServer API
handler =
  autoComplete
    :<|> placeDetails
    :<|> getPlaceName
    :<|> getCoordinates

autoComplete :: Id Person.Person -> Maybe Text -> Text -> Text -> Integer -> GoogleMaps.Language -> FlowHandler GoogleMaps.SearchLocationResp
autoComplete personId sessiontoken input location radius lang = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  let components = "country:in"
  ClientGoogleMaps.autoComplete input sessiontoken location radius components lang

placeDetails :: Id Person.Person -> Maybe Text -> Text -> FlowHandler GoogleMaps.PlaceDetailsResp
placeDetails personId sessiontoken placeId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  let fields = "geometry"
  ClientGoogleMaps.placeDetails sessiontoken placeId fields

getPlaceName :: Id Person.Person -> Maybe Text -> ClientGoogleMaps.LatLong -> Maybe GoogleMaps.Language -> FlowHandler GoogleMaps.GetPlaceNameResp
getPlaceName personId sessiontoken latLng lang = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  ClientGoogleMaps.getPlaceName sessiontoken (ClientGoogleMaps.ByLatLong latLng) lang

getCoordinates :: Id Person.Person -> Maybe Text -> Text -> Maybe GoogleMaps.Language -> FlowHandler GoogleMaps.GetPlaceNameResp
getCoordinates personId sessiontoken placeId lang = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  ClientGoogleMaps.getPlaceName sessiontoken (ClientGoogleMaps.ByPlaceId placeId) lang
