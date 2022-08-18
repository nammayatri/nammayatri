module API.UI.Route
  ( API,
    handler,
    RouteReq,
    RouteRes,
  )
where

import Beckn.Prelude
import qualified Beckn.Product.MapSearch.GoogleMaps as GoogleMaps
import Beckn.Types.Id
import qualified Beckn.Types.MapSearch as MapSearch
import Beckn.Utils.Logging
import qualified Domain.Types.Person as Person
import Environment
import Servant
import Utils.Auth
import Utils.Common hiding (id)

type API =
  "route"
    :> TokenAuth
    :> ReqBody '[JSON] RouteReq
    :> Post '[JSON] RouteRes

type RouteReq = MapSearch.Request

type RouteRes = MapSearch.Response

handler :: FlowServer API
handler = getRoute

getRoute :: Id Person.Person -> RouteReq -> FlowHandler RouteRes
getRoute personId = withFlowHandlerAPI . withPersonIdLogTag personId . GoogleMaps.getRoutes
