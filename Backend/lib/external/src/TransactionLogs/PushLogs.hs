{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module TransactionLogs.PushLogs
  ( pushLogs,
    pushPPFLog,
  )
where

import BecknV2.Utils
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import EulerHS.Prelude hiding (state)
import KafkaLogs.TransactionLogs
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Utils.Common
import TransactionLogs.Interface
import TransactionLogs.Interface.Types
import TransactionLogs.ONDC.PPFLog (PPFNetworkObservabilityLog)

pushLogs ::
  ( CoreMetrics m,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    HasRequestId r,
    MonadReader r m
  ) =>
  Text ->
  A.Value ->
  Text ->
  Text ->
  m ()
pushLogs requestType requestData merchantId domain = do
  ondcTokenHashMap <- asks (.ondcTokenHashMap)
  let mLogsTokenConfig = HM.lookup (KeyConfig merchantId domain) ondcTokenHashMap
  let kafkaLog = TransactionLog requestType requestData
  pushBecknLogToKafka kafkaLog
  let transactionLog = TransactionLogReq requestType (maskSensitiveData requestData)
  whenJust mLogsTokenConfig $ \logsTokenConfig ->
    pushTxnLogs (ONDCCfg $ ONDCConfig {apiToken = logsTokenConfig.token, url = logsTokenConfig.ondcUrl}) transactionLog

-- | Push PPF Network Observability log to ONDC and Kafka.
-- Used to report payment status transitions for settlement verification.
pushPPFLog ::
  ( CoreMetrics m,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    HasRequestId r,
    MonadReader r m
  ) =>
  PPFNetworkObservabilityLog ->
  Text ->
  Text ->
  m ()
pushPPFLog ppfLog merchantId domain = do
  let logData = A.toJSON ppfLog
  pushLogs "ppf_network_observability" logData merchantId domain
