 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Swagger.Handler where

import qualified API.Swagger.Types as Swagger
import API.Types (MainAPI)
import qualified Data.ByteString as BS
import Data.OpenApi
import Environment
import Kernel.Prelude
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
