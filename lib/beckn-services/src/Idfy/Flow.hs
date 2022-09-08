module Idfy.Flow
  ( IdfyWebhookAPI,
    idfyWebhookHandler,
    validateImage,
    extractRCImage,
    extractDLImage,
    verifyDL,
    verifyRC,
  )
where

import Beckn.Tools.Metrics.CoreMetrics
import Beckn.Types.Common
import Beckn.Types.Flow
import Beckn.Utils.Common hiding (Error)
import Beckn.Utils.IOLogging
import EulerHS.Prelude
import Idfy.External.Flow
import Idfy.Types.IdfyConfig
import Idfy.Types.VerificationResult
import Idfy.WebhookHandler
import Servant hiding (throwError)

type IdfyWebhookAPI =
  "service" :> "idfy"
    :> ( "drivingLicense"
           :> Header "Authorization" Text
           :> ReqBody '[JSON] DLVerificationResponse
           :> Post '[JSON] AckResponse
           :<|> "registrationCert"
             :> Header "Authorization" Text
             :> ReqBody '[JSON] RCVerificationResponse
             :> Post '[JSON] AckResponse
       )

idfyWebhookHandler ::
  ( HasField "isShuttingDown" a (TMVar ()),
    HasField "coreMetrics" a CoreMetricsContainer,
    HasField "loggerEnv" a LoggerEnv,
    HasField "idfyCfg" a IdfyConfig
  ) =>
  (DLVerificationResponse -> FlowR a AckResponse) ->
  (RCVerificationResponse -> FlowR a AckResponse) ->
  FlowServerR a IdfyWebhookAPI
idfyWebhookHandler dlHandler rcHandler =
  dlWebhookHandler dlHandler
    :<|> rcWebhookHandler rcHandler
