module API.Internal.NotificationWebhook
  ( API,
    handler,
  )
where

import qualified Domain.Action.Internal.NotificationWebhook as Domain
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.CommunicationEngine.Webhook as Webhook
import Servant

type API =
  "notification"
    :> ( SendAPI
           :<|> TemplatesAPI
       )

type SendAPI =
  "send"
    :> Header "token" Text
    :> ReqBody '[JSON] Webhook.SendNotificationReq
    :> Post '[JSON] APISuccess

type TemplatesAPI =
  "templates"
    :> Header "token" Text
    :> MandatoryQueryParam "channel" Webhook.NotifyChannel
    :> MandatoryQueryParam "userId" (Id DP.Person)
    :> Get '[JSON] [Webhook.MerchantMessageTemplate]

handler :: FlowServer API
handler =
  sendHandler
    :<|> templatesHandler
  where
    sendHandler mbToken req = withFlowHandlerAPI $ Domain.notificationWebhookSend mbToken req
    templatesHandler mbToken channel userId = withFlowHandlerAPI $ Domain.notificationWebhookTemplates mbToken channel userId
