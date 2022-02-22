module API.Handler where

import API.Beckn.Handler as Beckn
import qualified API.Swagger.Handler as Swagger
import qualified API.Types as API
import API.UI.Handler as UI
import Environment
import Servant

handler :: FlowServer API.API
handler =
  ( Beckn.handler
      :<|> UI.handler
  )
    :<|> Swagger.handler
