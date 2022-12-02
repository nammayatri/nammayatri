module API.Swagger.Handler where

import qualified API.Swagger.Types as Swagger
import API.Types (MainAPI)
import Beckn.Prelude
import qualified Data.ByteString as BS
import Data.OpenApi
import Environment
import Servant
import Servant.OpenApi

openAPI :: OpenApi
openAPI = do
  let openApi = toOpenApi (Proxy :: Proxy MainAPI)
  openApi
    { _openApiInfo =
        (_openApiInfo openApi)
          { _infoTitle = "Public Transport",
            _infoVersion = "1.0"
          }
    }

handler :: FlowServer Swagger.API
handler =
  writeSwaggerHTMLFlow
    :<|> writeOpenAPIFlow

writeSwaggerHTMLFlow :: FlowServer Swagger.SwaggerAPI
writeSwaggerHTMLFlow = lift $ BS.readFile "swagger/index.html"

writeOpenAPIFlow :: FlowServer Swagger.OpenAPI
writeOpenAPIFlow = pure openAPI
