module API where

import qualified API.Beckn as Beckn
import qualified API.Dashboard as Dashboard
import qualified API.UI as UI
import Beckn.Utils.Servant.HTML
import qualified Data.ByteString as BS
import Data.OpenApi
import Environment
import EulerHS.Prelude
import Servant
import Servant.OpenApi

type TransportAPI =
  MainAPI
    :<|> SwaggerAPI
    :<|> OpenAPI
    :<|> Raw

type MainAPI =
  UI.API
    :<|> Beckn.API
    :<|> Dashboard.API

transporterAPI :: Proxy TransportAPI
transporterAPI = Proxy

mainServer :: FlowServer MainAPI
mainServer =
  UI.handler
    :<|> Beckn.handler
    :<|> Dashboard.handler

transporterServer :: FlowServer TransportAPI
transporterServer =
  mainServer
    :<|> writeSwaggerHTMLFlow
    :<|> writeOpenAPIFlow
    :<|> serveDirectoryWebApp "dev/swagger"

type SwaggerAPI = "swagger" :> Get '[HTML] BS.ByteString

type OpenAPI = "openapi" :> Get '[JSON] OpenApi

openAPI :: OpenApi
openAPI = do
  let openApi = toOpenApi (Proxy :: Proxy MainAPI)
  openApi
    { _openApiInfo =
        (_openApiInfo openApi)
          { _infoTitle = "Yatri Partner",
            _infoVersion = "1.0"
          }
    }

writeSwaggerHTMLFlow :: FlowServer SwaggerAPI
writeSwaggerHTMLFlow = lift $ BS.readFile "dev/swagger/index.html"

writeOpenAPIFlow :: FlowServer OpenAPI
writeOpenAPIFlow = pure openAPI
