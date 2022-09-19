module API.Dashboard where

import qualified API.Dashboard.Driver as Driver
import Environment
import Servant

type API =
  "dashboard"
    :> Driver.API

handler :: FlowServer API
handler =
  Driver.handler
