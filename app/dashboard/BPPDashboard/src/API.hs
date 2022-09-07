module API where

import qualified API.BPP as BPP
import qualified API.Dashboard as Dashboard
import Environment
import Servant

type API =
  Dashboard.API
    :<|> BPP.API

handler :: FlowServer API
handler =
  Dashboard.handler
    :<|> BPP.handler
