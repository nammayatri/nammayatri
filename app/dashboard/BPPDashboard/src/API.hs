module API
  ( API,
    handler,
  )
where

import qualified API.BPP as BPP
import qualified "lib-dashboard" API.Dashboard as Dashboard
import Beckn.Prelude
import Beckn.Utils.Servant.HTML
import qualified Data.ByteString as BS
import Data.OpenApi
import "lib-dashboard" Environment
import Servant
import Servant.OpenApi

type API =
  MainAPI
    :<|> SwaggerAPI
    :<|> OpenAPI
    :<|> Raw

type MainAPI =
  Dashboard.API
    :<|> BPP.API

handler :: FlowServer API
handler =
  mainServer
    :<|> writeSwaggerHTMLFlow
    :<|> writeOpenAPIFlow
    :<|> serveDirectoryWebApp "dev/swagger"

mainServer :: FlowServer MainAPI
mainServer =
  Dashboard.handler
    :<|> BPP.handler

type SwaggerAPI = "swagger" :> Get '[HTML] BS.ByteString

type OpenAPI = "openapi" :> Get '[JSON] OpenApi

openAPI :: OpenApi
openAPI = do
  let openApi = toOpenApi (Proxy :: Proxy MainAPI)
  openApi
    { _openApiInfo =
        (_openApiInfo openApi)
          { _infoTitle = "BPP Dashboard",
            _infoVersion = "1.0"
          }
    }

writeSwaggerHTMLFlow :: FlowServer SwaggerAPI
writeSwaggerHTMLFlow = lift $ BS.readFile "dev/swagger/index.html"

writeOpenAPIFlow :: FlowServer OpenAPI
writeOpenAPIFlow = pure openAPI
