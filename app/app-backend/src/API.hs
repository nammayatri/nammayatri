{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API
  ( API,
    appAPI,
    handler,
  )
where

import qualified API.Auth as Auth
import qualified API.Beckn as Beckn
import qualified API.Dashboard as Dashboard
import qualified API.MetroBeckn as MetroBeckn
import qualified API.UI as UI
import App.Types
import Data.OpenApi (Info (..), OpenApi (..))
import EulerHS.Prelude
import Servant hiding (throwError)
import Servant.OpenApi

type API =
  MainAPI
    :<|> SwaggerAPI

type MainAPI =
  UI.API
    :<|> Beckn.API
    :<|> MetroBeckn.API
    :<|> Auth.API
    :<|> Dashboard.API

appAPI :: Proxy API
appAPI = Proxy

handler :: FlowServer API
handler =
  mainServer
    :<|> writeSwaggerJSONFlow

mainServer :: FlowServer MainAPI
mainServer =
  UI.handler
    :<|> Beckn.handler
    :<|> MetroBeckn.handler
    :<|> Auth.handler
    :<|> Dashboard.handler

type SwaggerAPI = "swagger" :> Get '[JSON] OpenApi

swagger :: OpenApi
swagger = do
  let openApi = toOpenApi (Proxy :: Proxy MainAPI)
  openApi
    { _openApiInfo =
        (_openApiInfo openApi)
          { _infoTitle = "Yatri",
            _infoVersion = "1.0"
          }
    }

writeSwaggerJSONFlow :: FlowServer SwaggerAPI
writeSwaggerJSONFlow = return swagger
