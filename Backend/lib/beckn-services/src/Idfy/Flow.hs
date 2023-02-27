{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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

import EulerHS.Prelude
import Idfy.Types.IdfyConfig
import Idfy.Types.Response
import Idfy.WebhookHandler
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Common
import Kernel.Types.Flow
import Kernel.Utils.Common hiding (Error)
import Kernel.Utils.IOLogging
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
