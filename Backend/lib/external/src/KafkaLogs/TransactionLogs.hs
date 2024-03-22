{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module KafkaLogs.TransactionLogs where

import qualified Data.Aeson as A
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer (produceMessage)
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Utils.Common
import Kernel.Utils.JSON

data TransactionLog = TransactionLog
  { _type :: Text,
    _data :: A.Value
  }
  deriving (Generic, Eq, Show)

instance FromJSON TransactionLog where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON TransactionLog where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

pushBecknLogToKafka ::
  (HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]) =>
  TransactionLog ->
  m ()
pushBecknLogToKafka transactionLog = do
  produceMessage ("beckn-transaction-log", Nothing) transactionLog
