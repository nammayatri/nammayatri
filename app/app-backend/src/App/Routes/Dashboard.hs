module App.Routes.Dashboard where

import App.Types
import Beckn.Prelude
import Beckn.Utils.Common
import Servant hiding (throwError)
import Utils.Auth (Dashboard, DashboardTokenAuth)

type API =
  CustomerListAPI

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
