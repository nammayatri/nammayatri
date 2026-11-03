module API.Internal.EmailEvents
  ( API,
    handler,
  )
where

import qualified Data.Aeson as A
import qualified Domain.Action.Internal.EmailEvents as Domain
import Environment
import EulerHS.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Utils.Common
import Servant

-- | Email delivery events: SES through an SNS HTTPS subscription (authenticated by the SNS message signature and
-- the city's configured topic), and the SendGrid event webhook (authenticated by the city's webhook token).
type API =
  "email"
    :> ( SesEventsAPI
           :<|> SendGridEventsAPI
       )

type SesEventsAPI =
  "sesEvents"
    :> ReqBody '[PlainText] Text
    :> Post '[JSON] APISuccess

type SendGridEventsAPI =
  "sendgridEvents"
    :> MandatoryQueryParam "token" Text
    :> ReqBody '[JSON] [A.Value]
    :> Post '[JSON] APISuccess

handler :: FlowServer API
handler =
  sesEventsHandler
    :<|> sendGridEventsHandler
  where
    sesEventsHandler body = withFlowHandlerAPI $ Domain.sesEvents body
    sendGridEventsHandler token events = withFlowHandlerAPI $ Domain.sendGridEvents token events
