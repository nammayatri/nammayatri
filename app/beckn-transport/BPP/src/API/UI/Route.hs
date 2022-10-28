module API.UI.Route
  ( RouteRequest,
    RouteResponse,
    API,
    handler,
  )
where

import qualified Beckn.External.Maps.Google as GoogleMaps
import Beckn.Prelude
import Beckn.Types.Id
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

type RouteRequest = GoogleMaps.GetRoutesReq

type RouteResponse = GoogleMaps.GetRoutesResp

getRoute :: Id Person.Person -> RouteRequest -> FlowHandler RouteResponse
getRoute personId = withFlowHandlerAPI . withPersonIdLogTag personId . GoogleMaps.getRoutes
