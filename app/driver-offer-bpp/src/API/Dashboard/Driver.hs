module API.Dashboard.Driver where

import Beckn.Prelude
import Beckn.Utils.Common
import Environment
import Servant hiding (throwError)
import Tools.Auth (Dashboard, DashboardTokenAuth)

type API =
  "dashboard"
    :> DashboardTokenAuth
    :> DriverListAPI

type DriverListAPI =
  "driver"
    :> "list"
    :> QueryParam "limit" Integer
    :> QueryParam "offset" Integer
    :> Get '[JSON] Text

handler :: FlowServer API
handler =
  listDriver

listDriver ::
  Dashboard ->
  Maybe Integer ->
  Maybe Integer ->
  FlowHandler Text
listDriver _ _ _ = withFlowHandlerAPI $ do
  pure "To be done"
