module API.BAP where

import qualified API.BAP.Customer as Customer
import "lib-dashboard" Environment
import Servant

type API =
  "bap"
    :> Customer.API

handler :: FlowServer API
handler =
  Customer.handler
