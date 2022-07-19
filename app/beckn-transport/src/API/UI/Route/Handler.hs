module API.UI.Route.Handler where

import API.UI.Route.Types
import App.Types
import Beckn.Prelude
import qualified Beckn.Product.MapSearch.GoogleMaps as GoogleMaps
import Beckn.Types.Id
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

getRoute :: Id Person.Person -> RouteRequest -> FlowHandler RouteResponse
getRoute personId = withFlowHandlerAPI . withPersonIdLogTag personId . GoogleMaps.getRoutes
