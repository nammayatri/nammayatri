module API
  ( API,
    handler,
  )
where

import qualified API.BAP as BAP
import qualified "lib-dashboard" API.Dashboard as Dashboard
import "lib-dashboard" Environment
import Servant

type API =
  Dashboard.API
    :<|> BAP.API

handler :: FlowServer API
handler =
  Dashboard.handler
    :<|> BAP.handler
