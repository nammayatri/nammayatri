{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=ambiguous-fields #-}

module Lib.SessionizerMetrics.Types.Event where

import Kernel.Prelude hiding (at, traceId)
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.App
import Kernel.Utils.Common
import Kernel.Utils.Dhall (FromDhall)
import Lib.SessionizerMetrics.Kafka.Config
import Prometheus

data Event p = Event
  { id :: Text, -- id of the event
    traceId :: Maybe Text, -- id for tracing the transaction from customer to driver
    sessionId :: Maybe Text, -- id for current session of customer/driver
    personId :: Maybe Text, -- id for customer or driver (whoever triggered the event)
    merchantId :: Text, -- id for merchant
    deploymentVersion :: Text, -- version of the current deployment
    at :: Double, -- time of the event
    eventType :: EventType, -- type of the event. Type should be defined at the application layer
    subType :: Maybe Text, -- sub type of the event based on type, for easy processig of event in the pipeline. Defined at the application layer
    primaryId :: Maybe Text, -- id of the primary entity involved in the event
    service :: Service, -- name of the service which generated this event
    triggeredBy :: EventTriggeredBy, -- who triggered the event
    payload :: Maybe p, -- payload of the event. Type of this payload should be defined at the application layer
    merchantOperatingCityId :: Maybe Text -- id for merchant operating city
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, ToSchema)

instance FromJSON p => FromJSON (Event p)

data EventTriggeredBy = User | System
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data EventStreamConfig = KafkaStream KafkaConfig | PrometheusStream Text | LogStream Text
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema, FromDhall)

data EventStreamName = KAFKA_STREAM | REDIS_STREAM | PROMETHEUS_STREAM | LOG_STREAM
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema, FromDhall)

data Service = DYNAMIC_OFFER_DRIVER_APP | RIDER_APP
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data EventStreamMap = EventStreamMap
  { streamName :: EventStreamName,
    streamConfig :: EventStreamConfig,
    eventTypes :: [EventType]
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema, FromDhall)

data EventType = RideCreated | RideStarted | RideEnded | RideCancelled | BookingCreated | BookingCancelled | BookingCompleted | SearchRequest | Quotes | Estimate | ExophoneData
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema, FromDhall)

type EventStreamFlow m r = (HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools], HasField "version" r DeploymentVersion, HasField "eventStreamMap" r [EventStreamMap], HasField "eventRequestCounter" r (Vector (Text, Text, Text) Counter))
