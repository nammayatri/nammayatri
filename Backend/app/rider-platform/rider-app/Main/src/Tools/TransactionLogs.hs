{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.TransactionLogs where

import qualified BecknV2.OnDemand.Types as Spec
-- shrey00 : need to use this

import qualified Data.Aeson as A
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer (produceMessage)
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Utils.Common
import Kernel.Utils.JSON

data TransactionLog = TransactionLog
  { _type :: Text,
    _data :: Req
  }
  deriving (Generic, Eq, Show)

instance FromJSON TransactionLog where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON TransactionLog where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data Req = Req
  { context :: Spec.Context, -- Context type defined in NY
    message :: A.Value -- message body of type of req (defined in NY)
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

pushBecknLogToKafka ::
  (HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]) =>
  TransactionLog ->
  m ()
pushBecknLogToKafka transactionLog = do
  produceMessage ("beckn-transaction-log", Nothing) transactionLog
