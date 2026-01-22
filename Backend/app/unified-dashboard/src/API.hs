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

import qualified API.Action.Management as Management
import qualified API.Action.Provider as Provider
import qualified Data.ByteString as BS
import Data.OpenApi
import qualified Domain.Types.Merchant
import Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Id
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
  "dashboard"
    :> Capture "merchantId" (ShortId Domain.Types.Merchant.Merchant)
    :> Capture "city" Kernel.Types.Beckn.Context.City
    :> (Management.API :<|> Provider.API)

handler :: FlowServer API
handler =
  mainServer
    :<|> writeSwaggerHTMLFlow
    :<|> writeOpenAPIFlow
    :<|> serveDirectoryWebApp "swagger"

mainServer :: ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> FlowServer (Management.API :<|> Provider.API)
mainServer merchantId city = Management.handler merchantId city :<|> Provider.handler merchantId city

type SwaggerAPI = "swagger" :> Get '[HTML] BS.ByteString

type OpenAPI = "openapi" :> Get '[JSON] OpenApi

openAPI :: OpenApi
openAPI = do
  let openApi = toOpenApi (Proxy :: Proxy MainAPI)
  openApi
    { _openApiInfo =
        (_openApiInfo openApi)
          { _infoTitle = "Unified Dashboard",
            _infoVersion = "1.0"
          }
    }

writeSwaggerHTMLFlow :: FlowServer SwaggerAPI
writeSwaggerHTMLFlow = lift $ BS.readFile "swagger/index.html"

writeOpenAPIFlow :: FlowServer OpenAPI
writeOpenAPIFlow = pure openAPI
