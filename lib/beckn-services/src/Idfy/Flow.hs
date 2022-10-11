module Idfy.Flow
  ( IdfyWebhookAPI,
    idfyWebhookHandler,
    validateImage,
    extractRCImage,
    extractDLImage,
    verifyDL,
    verifyRC,
    getTask,
  )
where

import Beckn.Tools.Metrics.CoreMetrics
import Beckn.Types.Common
import Beckn.Types.Flow
import Beckn.Utils.Common hiding (Error)
import Beckn.Utils.IOLogging
import EulerHS.Prelude
import Idfy.Types.IdfyConfig
import Idfy.Types.Response
import Idfy.WebhookHandler
import Servant hiding (throwError)

type IdfyWebhookAPI =
  "service" :> "idfy" :> "verification"
    :> Header "Authorization" Text
    :> ReqBody '[JSON] Value
    :> Post '[JSON] AckResponse

idfyWebhookHandler ::
  ( HasField "isShuttingDown" a (TMVar ()),
    HasField "coreMetrics" a CoreMetricsContainer,
    HasField "loggerEnv" a LoggerEnv,
    HasField "idfyCfg" a IdfyConfig
  ) =>
  (VerificationResponse -> Text -> FlowR a AckResponse) ->
  FlowServerR a IdfyWebhookAPI
idfyWebhookHandler = webhookHandler
