module API.UI.Route
  ( API,
    handler,
    DRoute.GetRoutesReq,
    DRoute.GetRoutesResp,
  )
where

import qualified Domain.Action.UI.Route as DRoute
import qualified Domain.Types.Person as Person
import Environment
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
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
