module API.Dashboard where

import App.Types
import Beckn.Prelude
import Beckn.Utils.Common
import Servant hiding (throwError)
import Utils.Auth (Dashboard, DashboardTokenAuth)

type API =
  CustomerListAPI

-- TODO use different tokens (or different routes) for different merchants

type CustomerListAPI =
  "dashboard"
    :> "customer"
    :> "list"
    :> DashboardTokenAuth
    :> Get '[JSON] Text

handler :: FlowServer API
handler =
  listCustomer

listCustomer :: Dashboard -> FlowHandler Text
listCustomer _ = withFlowHandlerAPI $ do
  pure "To be done"
