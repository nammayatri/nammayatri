module API
  ( API,
    handler,
  )
where

import qualified API.BPP as BPP
import qualified "lib-dashboard" API.Dashboard as Dashboard
import "lib-dashboard" Environment
import Servant

type API =
  Dashboard.API
    :<|> BPP.API

handler :: FlowServer API
handler =
  Dashboard.handler
    :<|> BPP.handler
