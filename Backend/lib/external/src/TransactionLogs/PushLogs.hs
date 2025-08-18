{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module TransactionLogs.PushLogs where

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

pushLogs ::
  ( CoreMetrics m,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig]
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
  logDebug $ "Pushing transaction logs to ONDC preprod for merchantId: " <> merchantId <> ", domain: " <> domain <> ", TokenConfig" <> show mLogsTokenConfig
  whenJust mLogsTokenConfig $ \logsTokenConfig -> do
    logDebug $ "Pushing transaction logs to ONDC preprod:" <> show logsTokenConfig
    pushTxnLogs (ONDCCfg $ ONDCConfig {apiToken = logsTokenConfig.token, url = logsTokenConfig.ondcUrl}) transactionLog
