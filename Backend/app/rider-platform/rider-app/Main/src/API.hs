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

-- import qualified API.Beckn as Beckn
import qualified API.Dashboard as Dashboard
import qualified API.FRFS as FRFS
import qualified API.Internal as Internal
import qualified API.MetroBeckn as MetroBeckn
import qualified API.UI as UI
import qualified Data.ByteString as BS
import Data.OpenApi
import qualified Domain.Action.UI.Payment as Payment
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude
import qualified Kernel.External.Payment.Juspay.Webhook as Juspay
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.BasicAuth ()
import Kernel.Utils.Servant.HTML
import Servant hiding (serveDirectoryWebApp, throwError)
import Servant.OpenApi
import Storage.Beam.SystemConfigs ()

type API =
  MainAPI
    :<|> FRFS.APIM
    :<|> SwaggerAPI
    :<|> OpenAPI
    :<|> Raw

type MainAPI =
  UI.API
    -- :<|> Beckn.API -- TODO :: Needs to be deprecated
    -- :<|> Beckn.APIV2
    :<|> MetroBeckn.API
    :<|> ( Capture "merchantId" (ShortId DM.Merchant)
             :> Juspay.JuspayWebhookAPI
         )
    :<|> Dashboard.API -- TODO :: Needs to be deprecated
    :<|> Dashboard.APIV2
    :<|> Internal.API

handler :: FlowServer API
handler =
  mainServer
    :<|> const FRFS.handler
    :<|> writeSwaggerHTMLFlow
    :<|> writeOpenAPIFlow
    :<|> serveDirectoryWebApp "swagger"

mainServer :: FlowServer MainAPI
mainServer =
  UI.handler
    -- :<|> Beckn.handler
    -- :<|> const Beckn.handler
    :<|> MetroBeckn.handler
    :<|> juspayWebhookHandler
    :<|> Dashboard.handler
    :<|> Dashboard.handlerV2
    :<|> Internal.handler

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

juspayWebhookHandler ::
  ShortId DM.Merchant ->
  BasicAuthData ->
  Value ->
  FlowHandler AckResponse
juspayWebhookHandler merchantShortId secret =
  withFlowHandlerAPI . Payment.juspayWebhookHandler merchantShortId secret
