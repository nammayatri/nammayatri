module API.Dashboard where

import qualified API.Dashboard.Driver as Driver
import Environment

type API =
  Driver.API

handler :: FlowServer API
handler =
  Driver.handler