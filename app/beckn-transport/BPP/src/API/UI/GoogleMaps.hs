module API.UI.GoogleMaps (module Reexport, API, handler) where

import Beckn.External.Maps.Google as Reexport (GetPlaceNameResp (..), PlaceDetailsResp (..), SearchLocationResp (..))
import qualified Beckn.External.Maps.Google as DGoogleMaps
import qualified Beckn.External.Maps.Google as GoogleMaps
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Person as SP
import Environment
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
             :> MandatoryQueryParam "latlng" GoogleMaps.LatLong -- Passing it as <latitude>,<longitude>
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

autoComplete :: Id SP.Person -> Maybe Text -> Text -> Text -> Integer -> GoogleMaps.Language -> FlowHandler GoogleMaps.SearchLocationResp
autoComplete _ sessiontoken input location radius = withFlowHandlerAPI . DGoogleMaps.autoComplete input sessiontoken location radius components
  where
    components = "country:in"

placeDetails :: Id SP.Person -> Maybe Text -> Text -> FlowHandler GoogleMaps.PlaceDetailsResp
placeDetails _ sessiontoken placeId = withFlowHandlerAPI $ DGoogleMaps.placeDetails sessiontoken placeId fields
  where
    fields = "geometry"

getPlaceName :: Id SP.Person -> Maybe Text -> GoogleMaps.LatLong -> Maybe GoogleMaps.Language -> FlowHandler GoogleMaps.GetPlaceNameResp
getPlaceName _ sessiontoken latlng lang = withFlowHandlerAPI $ DGoogleMaps.getPlaceName sessiontoken (GoogleMaps.ByLatLong latlng) lang

getCoordinates :: Id SP.Person -> Maybe Text -> Text -> Maybe GoogleMaps.Language -> FlowHandler GoogleMaps.GetPlaceNameResp
getCoordinates _ sessiontoken placeId lang = withFlowHandlerAPI $ DGoogleMaps.getPlaceName sessiontoken (GoogleMaps.ByPlaceId placeId) lang
