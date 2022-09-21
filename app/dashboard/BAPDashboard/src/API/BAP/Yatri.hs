module API.BAP.Yatri where

import qualified API.BAP.Yatri.Customer as Customer
import "lib-dashboard" Environment
import Servant
import "lib-dashboard" Tools.Auth

type API =
  "yatri"
    :> ServerAuth 'APP_BACKEND_YATRI
    :> Customer.API

handler :: FlowServer API
handler _serverName =
  Customer.handler
