module API.UI.Route
  ( RouteRequest,
    RouteResponse,
    API,
    handler,
  )
where

import Beckn.Prelude
import qualified Beckn.Product.MapSearch.GoogleMaps as GoogleMaps
import Beckn.Types.Id
import qualified Beckn.Types.MapSearch as MapSearch
import Beckn.Utils.Common hiding (id)
import qualified Domain.Types.Person as Person
import Environment
import Servant
import Tools.Auth

type API =
  "route"
    :> TokenAuth
    :> ReqBody '[JSON] RouteRequest
    :> Post '[JSON] RouteResponse

handler :: FlowServer API
handler = getRoute

type RouteRequest = MapSearch.Request

type RouteResponse = MapSearch.Response

getRoute :: Id Person.Person -> RouteRequest -> FlowHandler RouteResponse
getRoute personId = withFlowHandlerAPI . withPersonIdLogTag personId . GoogleMaps.getRoutes
