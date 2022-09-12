module API.Dashboard.Driver where

import Beckn.Prelude
import Beckn.Utils.Common
import Environment
import Servant hiding (throwError)
import Tools.Auth (Dashboard, DashboardTokenAuth)

type API =
  DriverListAPI

type DriverListAPI =
  "dashboard"
    :> "driver"
    :> "list"
    :> DashboardTokenAuth
    :> Get '[JSON] Text

handler :: FlowServer API
handler =
  listDriver

listDriver :: Dashboard -> FlowHandler Text
listDriver _ = withFlowHandlerAPI $ do
  pure "To be done"