module API where

import qualified API.Beckn.Handler as Beckn
import qualified API.UI.Handler as UI
import App.Types
import Data.OpenApi
import EulerHS.Prelude
import Servant
import Servant.OpenApi

type TransportAPI =
  MainAPI
    :<|> SwaggerAPI

type MainAPI =
  UI.API
    :<|> Beckn.API

transporterAPI :: Proxy TransportAPI
transporterAPI = Proxy

mainServer :: FlowServer MainAPI
mainServer =
  UI.handler
    :<|> Beckn.handler

transporterServer :: FlowServer TransportAPI
transporterServer =
  mainServer
    :<|> writeSwaggerJSONFlow

type HealthCheckAPI = Get '[JSON] Text

type SwaggerAPI = "swagger" :> Get '[JSON] OpenApi

swagger :: OpenApi
swagger = do
  let openApi = toOpenApi (Proxy :: Proxy MainAPI)
  openApi
    { _openApiInfo =
        (_openApiInfo openApi)
          { _infoTitle = "Yatri Partner",
            _infoVersion = "1.0"
          }
    }

writeSwaggerJSONFlow :: FlowServer SwaggerAPI
writeSwaggerJSONFlow = return swagger
