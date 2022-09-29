module API.BPP
  ( API,
    handler,
  )
where

import qualified API.BPP.BecknTransport as BecknTransport
import qualified API.BPP.DriverOffer as DriverOffer
import "lib-dashboard" Environment
import Servant

type API =
  "bpp"
    :> ( BecknTransport.API
           :<|> DriverOffer.API
       )

handler :: FlowServer API
handler =
  BecknTransport.handler
    :<|> DriverOffer.handler
