{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API
  ( API,
    handler,
  )
where

import qualified API.Auth as Auth
import qualified API.Beckn as Beckn
import qualified API.Dashboard as Dashboard
import qualified API.MetroBeckn as MetroBeckn
import qualified API.UI as UI
import qualified Data.ByteString as BS
import Data.OpenApi
import Environment
import EulerHS.Prelude
import Kernel.Utils.Servant.Client
import Kernel.Utils.Servant.HTML
import Servant hiding (serveDirectoryWebApp, throwError)
import Servant.OpenApi

type API =
  MainAPI
    :<|> SwaggerAPI
    :<|> OpenAPI
    :<|> Raw

type MainAPI =
  UI.API
    :<|> Beckn.API
    :<|> MetroBeckn.API
    :<|> Auth.API
    :<|> Dashboard.API

handler :: FlowServer API
handler =
  mainServer
    :<|> writeSwaggerHTMLFlow
    :<|> writeOpenAPIFlow
    :<|> serveDirectoryWebApp "swagger"

mainServer :: FlowServer MainAPI
mainServer =
  UI.handler
    :<|> Beckn.handler
    :<|> MetroBeckn.handler
    :<|> Auth.handler
    :<|> Dashboard.handler

type SwaggerAPI = "swagger" :> Get '[HTML] BS.ByteString

type OpenAPI = "openapi" :> Get '[JSON] OpenApi

openAPI :: OpenApi
openAPI = do
  let openApi = toOpenApi (Proxy :: Proxy MainAPI)
  openApi
    { _openApiInfo =
        (_openApiInfo openApi)
          { _infoTitle = "Yatri",
            _infoVersion = "1.0"
          }
    }

writeSwaggerHTMLFlow :: FlowServer SwaggerAPI
writeSwaggerHTMLFlow = lift $ BS.readFile "swagger/index.html"

writeOpenAPIFlow :: FlowServer OpenAPI
writeOpenAPIFlow = pure openAPI
