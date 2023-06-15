{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Types.Event where

import Kernel.Prelude hiding (at, traceId)
import Lib.Kafka.Config
import qualified Lib.Types.Ride as Ride

data Event = Event
  { id :: Text, -- id of the event
    traceId :: Maybe Text, -- id for tracing the transaction from customer to driver
    sessionId :: Maybe Text, -- id for current session of customer/driver
    personId :: Text, -- id for customer or driver (whoever triggered the event)
    merchantId :: Text, -- id for merchant
    deploymentVersion :: Text, -- version of the current deployment
    at :: UTCTime, -- time of the event
    eventType :: Text, -- type of the event. Type should be defined at the application layer
    subType :: Maybe Text, -- sub type of the event based on type, for easy processig of event in the pipeline. Defined at the application layer
    primaryId :: Maybe Text, -- id of the primary entity involved in the event
    service :: Text, -- name of the service which generated this event
    triggeredBy :: EventTriggeredBy, -- who triggered the event
    payload :: Maybe Payload -- payload of the event. Type of this payload should be defined at the application layer
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data EventTriggeredBy = User | System
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data Payload = RideStarted Ride.RideStart | RideEnd Ride.RideEnd
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data EventStreamConfig = KafkaStream KafkaConfig | LogStream Text
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data EventStream = EventStream
  { name :: Text,
    config :: EventStreamConfig
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

data EventStreamMap = EventStreamMap
  { streamName :: Text,
    eventTypes :: [Text]
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)
