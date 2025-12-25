{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Streaming.Kafka.Environment
  ( module Tools.Streaming.Kafka.Environment,
    module Reexport,
  )
where

import EulerHS.Prelude
import Kernel.Streaming.Kafka.Producer.Types as Reexport
import Kernel.Streaming.Kafka.Topic.BusinessEvent.Environment
import Kernel.Streaming.Kafka.Topic.BusinessEvent.Types as Reexport

newtype SAFETYKafkaEnvs = SAFETYKafkaEnvs
  { businessEventEnv :: KafkaBEEnv
  }
  deriving (Generic)

buildSAFETYKafkaEnvs :: IO SAFETYKafkaEnvs
buildSAFETYKafkaEnvs = do
  businessEventEnv <- buildKafkaBEEnv "SAFETY"

  return $ SAFETYKafkaEnvs {..}
