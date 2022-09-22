module API.BPP.DriverOffer
  ( API,
    handler,
  )
where

import qualified API.BPP.DriverOffer.Driver as Driver
import "lib-dashboard" Environment
import Servant

type API =
  "driver-offer"
    :> Driver.API

handler :: FlowServer API
handler =
  Driver.handler
