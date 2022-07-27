module API.UI.Route
  ( RouteRequest,
    RouteResponse,
    API,
    handler,
  )
where

import App.Types
import qualified Beckn.External.GoogleMaps.Types as GoogleMaps
import Beckn.Prelude
import qualified Beckn.Product.MapSearch.GoogleMaps as GoogleMaps
import Beckn.Types.Id
import qualified Beckn.Types.MapSearch as MapSearch
import Beckn.Utils.Logging
import qualified Domain.Types.Person as Person
import Servant
import Utils.Auth
import Utils.Common hiding (id)

type API =
  "route"
    :> TokenAuth
    :> ReqBody '[JSON] RouteRequest
    :> Post '[JSON] RouteResponse

handler :: FlowServer API
handler = getRoute

type RouteRequest = MapSearch.Request

type RouteResponse = GoogleMaps.DirectionsResp

getRoute :: Id Person.Person -> RouteRequest -> FlowHandler RouteResponse
getRoute personId = withFlowHandlerAPI . withPersonIdLogTag personId . GoogleMaps.getRoutes
