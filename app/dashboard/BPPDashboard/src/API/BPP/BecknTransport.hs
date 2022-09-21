module API.BPP.BecknTransport
  ( API,
    handler,
  )
where

import qualified API.BPP.BecknTransport.Driver as Driver
import "lib-dashboard" Environment
import Servant
import "lib-dashboard" Tools.Auth

type API =
  "beckn-transport"
    :> ServerAuth (ServerAccess 'BECKN_TRANSPORT)
    :> Driver.API

handler :: FlowServer API
handler _serverName =
  Driver.handler
