module API.UI.Route
  ( API,
    handler,
    RouteReq,
    RouteRes,
  )
where

import qualified Beckn.External.Maps.Google as Google
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
    :> ReqBody '[JSON] RouteReq
    :> Post '[JSON] RouteRes

type RouteReq = Google.GetRoutesReq

type RouteRes = Google.GetRoutesResp

handler :: FlowServer API
handler = getRoute

getRoute :: Id Person.Person -> RouteReq -> FlowHandler RouteRes
getRoute personId = withFlowHandlerAPI . withPersonIdLogTag personId . Google.getRoutes
