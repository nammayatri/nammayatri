module API where

import qualified API.Beckn as Beckn
import qualified API.UI as UI
import Data.OpenApi
import Environment
import EulerHS.Prelude
import Servant
import Servant.OpenApi

type DriverOfferAPI =
  MainAPI
    :<|> SwaggerAPI

type MainAPI =
  UI.API
    :<|> Beckn.API

driverOfferAPI :: Proxy DriverOfferAPI
driverOfferAPI = Proxy

mainServer :: FlowServer MainAPI
mainServer =
  UI.handler
    :<|> Beckn.handler

driverOfferServer :: FlowServer DriverOfferAPI
driverOfferServer =
  mainServer
    :<|> writeSwaggerJSONFlow

type SwaggerAPI = "swagger" :> Get '[JSON] OpenApi

swagger :: OpenApi
swagger = do
  let openApi = toOpenApi (Proxy :: Proxy MainAPI)
  openApi
    { _openApiInfo =
        (_openApiInfo openApi)
          { _infoTitle = "Namma Yatri Partner",
            _infoVersion = "1.0"
          }
    }

writeSwaggerJSONFlow :: FlowServer SwaggerAPI
writeSwaggerJSONFlow = return swagger
