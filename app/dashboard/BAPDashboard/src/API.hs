module API where

import qualified API.BAP as BAP
import qualified API.Dashboard as Dashboard
import Environment
import Servant

type API =
  Dashboard.API
    :<|> BAP.API

handler :: FlowServer API
handler =
  Dashboard.handler
    :<|> BAP.handler
