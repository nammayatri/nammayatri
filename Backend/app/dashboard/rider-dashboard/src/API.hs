{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API
  ( API,
    handler,
  )
where

import qualified "lib-dashboard" API.Dashboard as Dashboard
import qualified API.RiderPlatform as BAP
import qualified Data.ByteString as BS
import Data.OpenApi
import "lib-dashboard" Environment
import Kernel.Prelude
import Kernel.Utils.Servant.Client
import Kernel.Utils.Servant.HTML
import Servant hiding (serveDirectoryWebApp)
import Servant.OpenApi

type API =
  MainAPI
    :<|> SwaggerAPI
    :<|> OpenAPI
    :<|> Raw

type MainAPI =
  Dashboard.API
    :<|> BAP.API
    :<|> BAP.APIV2

handler :: FlowServer API
handler =
  mainServer
    :<|> writeSwaggerHTMLFlow
    :<|> writeOpenAPIFlow
    :<|> serveDirectoryWebApp "swagger"

mainServer :: FlowServer MainAPI
mainServer =
  Dashboard.handler
    :<|> BAP.handler
    :<|> BAP.handlerV2

type SwaggerAPI = "swagger" :> Get '[HTML] BS.ByteString

type OpenAPI = "openapi" :> Get '[JSON] OpenApi

openAPI :: OpenApi
openAPI = do
  let openApi = toOpenApi (Proxy :: Proxy MainAPI)
  openApi
    { _openApiInfo =
        (_openApiInfo openApi)
          { _infoTitle = "BAP Dashboard",
            _infoVersion = "1.0"
          }
    }

writeSwaggerHTMLFlow :: FlowServer SwaggerAPI
writeSwaggerHTMLFlow = lift $ BS.readFile "swagger/index.html"

writeOpenAPIFlow :: FlowServer OpenAPI
writeOpenAPIFlow = pure openAPI
