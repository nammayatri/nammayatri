module API.BPP.DriverOffer
  ( API,
    handler,
  )
where

import qualified API.BPP.DriverOffer.Driver as Driver
import "lib-dashboard" Environment
import Servant
import "lib-dashboard" Tools.Auth

type API =
  "driver-offer"
    :> ServerAuth (ServerAccess 'DRIVER_OFFER_BPP)
    :> Driver.API

handler :: FlowServer API
handler _serverName =
  Driver.handler
