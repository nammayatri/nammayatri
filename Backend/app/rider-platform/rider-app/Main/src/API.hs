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

import qualified API.Beckn as Beckn
import qualified API.Conductor as Conductor
import qualified API.Dashboard as Dashboard
import qualified API.FRFS as FRFS
import qualified API.IGM as IGM
import qualified API.Internal as Internal
import qualified API.UI as UI
import qualified Data.ByteString as BS
import Data.OpenApi hiding (Header)
import qualified Domain.Action.Internal.Payout as Payout
import qualified Domain.Action.UI.Payment as Payment
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude
import qualified Kernel.External.Payment.Juspay.Webhook as Juspay
import qualified Kernel.External.Payment.Stripe.Webhook as Stripe
import qualified Kernel.External.Payout.Juspay.Webhook as JuspayPayout
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.BasicAuth ()
import Kernel.Utils.Servant.HTML
import Servant hiding (serveDirectoryWebApp, throwError)
import Servant.OpenApi
import Storage.Beam.SystemConfigs ()
import qualified Tools.Payment as TPayment

type API =
  MainAPI
    :<|> IGM.IGMAPI
    :<|> FRFS.APIM
    :<|> Beckn.API -- TODO : Revert after 2.x release
    :<|> Beckn.APIV2 -- TODO : Revert after 2.x release
    :<|> SwaggerAPI
    :<|> OpenAPI
    :<|> Raw

type MainAPI =
  UI.API
    -- :<|> Beckn.API -- TODO :: Needs to be deprecated  -- TODO : Revert after 2.x release
    -- :<|> Beckn.APIV2 -- TODO : Revert after 2.x release
    :<|> ( Capture "merchantId" (ShortId DM.Merchant)
             :> QueryParam "city" Context.City
             :> QueryParam "serviceType" TPayment.PaymentServiceType
             :> QueryParam "placeId" Text
             :> Juspay.JuspayWebhookAPI
         )
    :<|> ( Capture "merchantId" (ShortId DM.Merchant)
             :> QueryParam "city" Context.City
             :> QueryParam "serviceType" TPayment.PaymentServiceType
             :> QueryParam "placeId" Text
             :> Stripe.StripeWebhookAPI
         )
    :<|> Dashboard.APIV2
    :<|> Internal.API
    :<|> ( Capture "merchantId" (ShortId DM.Merchant)
             :> QueryParam "city" Context.City
             :> "v2"
             :> JuspayPayout.JuspayPayoutWebhookAPI
         )
    :<|> Conductor.API

handler :: FlowServer API
handler =
  mainServer
    :<|> IGM.handler
    :<|> const FRFS.handler
    :<|> Beckn.handler -- TODO : Revert after 2.x release
    :<|> const Beckn.handler -- TODO : Revert after 2.x release
    :<|> writeSwaggerHTMLFlow
    :<|> writeOpenAPIFlow
    :<|> serveDirectoryWebApp "swagger"

mainServer :: FlowServer MainAPI
mainServer =
  UI.handler
    -- :<|> Beckn.handler  -- TODO : Revert after 2.x release
    -- :<|> const Beckn.handler  -- TODO : Revert after 2.x release
    :<|> juspayWebhookHandler
    :<|> stripeWebhookHandler
    :<|> Dashboard.handlerV2
    :<|> Internal.handler
    :<|> juspayPayoutWebhookHandlerV2
    :<|> Conductor.handler

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
  Maybe Context.City ->
  Maybe TPayment.PaymentServiceType ->
  Maybe Text ->
  BasicAuthData ->
  Value ->
  FlowHandler AckResponse
juspayWebhookHandler merchantShortId mbCity mbServiceType mbPlaceId secret =
  withFlowHandlerAPI . Payment.juspayWebhookHandler merchantShortId mbCity mbServiceType mbPlaceId secret

stripeWebhookHandler ::
  ShortId DM.Merchant ->
  Maybe Context.City ->
  Maybe TPayment.PaymentServiceType ->
  Maybe Text ->
  Maybe Text ->
  Stripe.RawByteString ->
  FlowHandler AckResponse
stripeWebhookHandler merchantShortId mbCity mbServiceType mbPlaceId mbSigHeader =
  withFlowHandlerAPI . Payment.stripeWebhookHandler merchantShortId mbCity mbServiceType mbPlaceId mbSigHeader

juspayPayoutWebhookHandlerV2 ::
  ShortId DM.Merchant ->
  Maybe Context.City ->
  BasicAuthData ->
  Value ->
  FlowHandler AckResponse
juspayPayoutWebhookHandlerV2 merchantShortId mbOpCity secret value' =
  withFlowHandlerAPI $ Payout.juspayPayoutWebhookHandler merchantShortId mbOpCity secret value'
