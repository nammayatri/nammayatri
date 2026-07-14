{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Public root-level Meta WhatsApp Cloud API webhook (Juspay/Stripe style),
-- mounted in "API". Two routes under @whatsapp/webhook@: the GET verification
-- handshake (echo @hub.challenge@) and the POST inbound message delivery
-- (raw body kept for HMAC verification). Wiring shape from
-- @API.Internal.XyneWebhook@.
module API.MetaWhatsappWebhook
  ( API,
    handler,
  )
where

import qualified Domain.Action.UI.MetaWhatsappWebhook as Domain
import Environment
import EulerHS.Prelude
import Kernel.External.Meta.Webhook (RawByteString, RawJson)
import qualified Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Utils.Common
import Servant

type API =
  "whatsapp"
    :> "webhook"
    :> ( QueryParam "hub.mode" Kernel.Prelude.Text
           :> QueryParam "hub.verify_token" Kernel.Prelude.Text
           :> QueryParam "hub.challenge" Kernel.Prelude.Text
           :> Get '[PlainText] Kernel.Prelude.Text
           :<|> Header "X-Hub-Signature-256" Kernel.Prelude.Text
             :> ReqBody '[RawJson, OctetStream] RawByteString
             :> Post '[JSON] APISuccess
       )

handler :: FlowServer API
handler = getWebhookChallenge :<|> postWebhook
  where
    getWebhookChallenge mMode mToken mChallenge =
      withFlowHandlerAPI $ Domain.getWebhookChallenge mMode mToken mChallenge
    postWebhook mbSig rawBody =
      withFlowHandlerAPI $ Domain.postWebhook mbSig rawBody
