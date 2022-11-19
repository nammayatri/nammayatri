module API.BPP.BecknTransport
  ( API,
    handler,
  )
where

import qualified API.BPP.BecknTransport.Driver as Driver
import Beckn.Types.Id
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import "lib-dashboard" Environment
import Servant

type API =
  "beckn-transport"
    :> Capture "merchantId" (ShortId DM.Merchant)
    :> Driver.API

handler :: FlowServer API
handler =
  Driver.handler
