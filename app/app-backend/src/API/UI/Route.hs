module API.UI.Route
  ( API,
    handler,
    GoogleMaps.GetRoutesReq,
    GoogleMaps.GetRoutesResp,
  )
where

import qualified Beckn.External.Maps.Google as GoogleMaps
import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Person as Person
import Environment
import Servant
import Tools.Auth

type API =
  "route"
    :> TokenAuth
    :> ReqBody '[JSON] GoogleMaps.GetRoutesReq
    :> Post '[JSON] GoogleMaps.GetRoutesResp

handler :: FlowServer API
handler = getRoute

getRoute :: Id Person.Person -> GoogleMaps.GetRoutesReq -> FlowHandler GoogleMaps.GetRoutesResp
getRoute personId = withFlowHandlerAPI . withPersonIdLogTag personId . GoogleMaps.getRoutes
