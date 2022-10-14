module API.UI.GoogleMaps (module Reexport, API, handler) where

import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Action.UI.GoogleMaps as Reexport
  ( GetPlaceNameResp (..),
    PlaceDetailsResp (..),
    SearchLocationResp (..),
  )
import qualified Domain.Action.UI.GoogleMaps as DGoogleMaps
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

autoComplete :: Id SP.Person -> Maybe Text -> Text -> Text -> Integer -> Text -> FlowHandler SearchLocationResp
autoComplete _ sessiontoken input location radius = withFlowHandlerAPI . DGoogleMaps.autoComplete sessiontoken input location radius

placeDetails :: Id SP.Person -> Maybe Text -> Text -> FlowHandler PlaceDetailsResp
placeDetails _ sessiontoken = withFlowHandlerAPI . DGoogleMaps.placeDetails sessiontoken

getPlaceName :: Id SP.Person -> Maybe Text -> Text -> Maybe Text -> FlowHandler GetPlaceNameResp
getPlaceName _ sessiontoken latlng lang = withFlowHandlerAPI $ DGoogleMaps.getPlaceName sessiontoken latlng lang

getCoordinates :: Id SP.Person -> Maybe Text -> Text -> Maybe Text -> FlowHandler GetPlaceNameResp
getCoordinates _ sessiontoken placeId lang = withFlowHandlerAPI $ DGoogleMaps.getCoordinates sessiontoken placeId lang
