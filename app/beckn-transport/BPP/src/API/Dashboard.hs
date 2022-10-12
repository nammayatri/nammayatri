module API.Dashboard where

import qualified API.Dashboard.Driver as Driver
import Environment
import Servant
import Tools.Auth

type API =
  "dashboard"
    :> DashboardTokenAuth
    :> Driver.API

handler :: FlowServer API
handler _ =
  Driver.handler
