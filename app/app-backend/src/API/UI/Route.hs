module API.UI.Route
  ( API,
    handler,
    MapSearch.Request,
    MapSearch.Response,
  )
where

import Beckn.Prelude
import qualified Beckn.Product.MapSearch.GoogleMaps as GoogleMaps
import Beckn.Types.Id
import qualified Beckn.Types.MapSearch as MapSearch
import Beckn.Utils.Common
import qualified Domain.Types.Person as Person
import Environment
import Servant
import Tools.Auth

type API =
  "route"
    :> TokenAuth
    :> ReqBody '[JSON] MapSearch.Request
    :> Post '[JSON] MapSearch.Response

handler :: FlowServer API
handler = getRoute

getRoute :: Id Person.Person -> MapSearch.Request -> FlowHandler MapSearch.Response
getRoute personId = withFlowHandlerAPI . withPersonIdLogTag personId . GoogleMaps.getRoutes
