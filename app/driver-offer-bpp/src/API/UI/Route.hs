module API.UI.Route
  ( API,
    handler,
    DRoute.GetRoutesReq,
    DRoute.GetRoutesResp,
  )
where

import qualified Beckn.External.Maps as Maps
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Action.UI.Route as DRoute
import qualified Domain.Types.Person as Person
import Environment
import Servant
import Tools.Auth

type API =
  "route"
    :> TokenAuth
    :> ReqBody '[JSON] Maps.GetRoutesReq
    :> Post '[JSON] Maps.GetRoutesResp

handler :: FlowServer API
handler = getRoute

getRoute :: Id Person.Person -> Maps.GetRoutesReq -> FlowHandler Maps.GetRoutesResp
getRoute personId = withFlowHandlerAPI . withPersonIdLogTag personId . DRoute.getRoutes personId
