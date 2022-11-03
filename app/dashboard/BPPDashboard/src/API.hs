module API
  ( API,
    handler,
  )
where

import qualified API.BPP as BPP
import qualified "lib-dashboard" API.Dashboard as Dashboard
import Beckn.Prelude
import Data.OpenApi
import "lib-dashboard" Environment
import Servant
import Servant.OpenApi

type API =
  MainAPI
    :<|> SwaggerAPI

type MainAPI =
  Dashboard.API
    :<|> BPP.API

handler :: FlowServer API
handler =
  mainServer
    :<|> writeSwaggerJSONFlow

mainServer :: FlowServer MainAPI
mainServer =
  Dashboard.handler
    :<|> BPP.handler

type SwaggerAPI = "swagger" :> Get '[JSON] OpenApi

swagger :: OpenApi
swagger = do
  let openApi = toOpenApi (Proxy :: Proxy MainAPI)
  openApi
    { _openApiInfo =
        (_openApiInfo openApi)
          { _infoTitle = "BPP Dashboard",
            _infoVersion = "1.0"
          }
    }

writeSwaggerJSONFlow :: FlowServer SwaggerAPI
writeSwaggerJSONFlow = return swagger
