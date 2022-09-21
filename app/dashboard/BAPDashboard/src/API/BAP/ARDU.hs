module API.BAP.ARDU where

import qualified API.BAP.ARDU.Customer as Customer
import "lib-dashboard" Environment
import Servant
import "lib-dashboard" Tools.Auth

type API =
  "ardu"
    :> ServerAuth 'APP_BACKEND_ARDU
    :> Customer.API

handler :: FlowServer API
handler _serverName =
  Customer.handler
