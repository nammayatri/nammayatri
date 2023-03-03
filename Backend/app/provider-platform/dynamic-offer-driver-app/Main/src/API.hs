{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API where

import qualified API.Beckn as Beckn
import qualified API.Dashboard as Dashboard
import qualified API.Internal as Internal
import qualified API.UI as UI
-- import Domain.Action.UI.DriverOnboarding.DriverLicense as DriverOnboarding
-- import Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DriverOnboarding

import qualified Data.ByteString as BS
import Data.OpenApi
import Domain.Action.UI.DriverOnboarding.IdfyWebhook as DriverOnboarding
import Environment
import EulerHS.Prelude
import qualified Idfy.Flow as Idfy
import Kernel.Utils.Servant.Client
import Kernel.Utils.Servant.HTML
import Servant hiding (serveDirectoryWebApp)
import Servant.OpenApi

type DriverOfferAPI =
  MainAPI
    :<|> SwaggerAPI
    :<|> OpenAPI
    :<|> Raw

type MainAPI =
  UI.API
    :<|> Beckn.API
    :<|> Idfy.IdfyWebhookAPI
    :<|> Dashboard.API
    :<|> Internal.API

driverOfferAPI :: Proxy DriverOfferAPI
driverOfferAPI = Proxy

mainServer :: FlowServer MainAPI
mainServer =
  UI.handler
    :<|> Beckn.handler
    :<|> Idfy.idfyWebhookHandler DriverOnboarding.onVerify
    :<|> Dashboard.handler
    :<|> Internal.handler

driverOfferServer :: FlowServer DriverOfferAPI
driverOfferServer =
  mainServer
    :<|> writeSwaggerHTMLFlow
    :<|> writeOpenAPIFlow
    :<|> serveDirectoryWebApp "swagger"

type SwaggerAPI = "swagger" :> Get '[HTML] BS.ByteString

type OpenAPI = "openapi" :> Get '[JSON] OpenApi

openAPI :: OpenApi
openAPI = do
  let openApi = toOpenApi (Proxy :: Proxy MainAPI)
  openApi
    { _openApiInfo =
        (_openApiInfo openApi)
          { _infoTitle = "Namma Yatri Partner",
            _infoVersion = "1.0"
          }
    }

writeSwaggerHTMLFlow :: FlowServer SwaggerAPI
writeSwaggerHTMLFlow = lift $ BS.readFile "swagger/index.html"

writeOpenAPIFlow :: FlowServer OpenAPI
writeOpenAPIFlow = pure openAPI
