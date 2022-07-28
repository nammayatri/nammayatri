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

handler :: FlowServer API
handler =
  autoComplete
    :<|> placeDetails
    :<|> getPlaceName

autoComplete :: Id SP.Person -> Text -> Text -> Integer -> Text -> FlowHandler SearchLocationResp
autoComplete _ input location radius = withFlowHandlerAPI . DGoogleMaps.autoComplete input location radius

placeDetails :: Id SP.Person -> Text -> FlowHandler PlaceDetailsResp
placeDetails _ = withFlowHandlerAPI . DGoogleMaps.placeDetails

getPlaceName :: Id SP.Person -> Text -> FlowHandler GetPlaceNameResp
getPlaceName _ = withFlowHandlerAPI . DGoogleMaps.getPlaceName
