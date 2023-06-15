{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.EventStream where

import Kernel.Prelude hiding (at, traceId)
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Utils.Common
import Lib.Kafka.Config
import Lib.Kafka.Internal
import Lib.Types.Event

sampleConfig :: KafkaConfig
sampleConfig =
  KafkaConfig
    { topicName = "rider-updates"
    }

eventStreams :: [EventStream]
eventStreams = [EventStream {name = "kafkaEventStream", config = KafkaStream sampleConfig}, EventStream {name = "logEventStream", config = LogStream "LSD"}]

eventStreamMapping :: [EventStreamMap]
eventStreamMapping =
  [ EventStreamMap
      { streamName = "kafkaEventStream",
        eventTypes = ["RideStarted", "RideEnded"]
      },
    EventStreamMap
      { streamName = "redisEventStream",
        eventTypes = ["RideStarted", "RideEnded", "Booked"]
      },
    EventStreamMap
      { streamName = "databaseEventStream",
        eventTypes = ["RideStarted"]
      },
    EventStreamMap
      { streamName = "logEventStream",
        eventTypes = ["RideEnded"]
      }
  ]

dummyEvent :: Event
dummyEvent =
  Event
    { id = "12345",
      traceId = Just "abcde",
      sessionId = Just "session123",
      personId = "person123",
      merchantId = "merchant123",
      deploymentVersion = "1.0.0",
      at = read "2023-06-13 12:00:00 UTC",
      eventType = "RideStarted",
      subType = Just "SubType",
      primaryId = Just "primary123",
      service = "ServiceName",
      triggeredBy = User,
      payload = Nothing
    }

triggerEvent :: (Monad m, Log m, MonadFlow m, MonadReader r0 m, HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]) => [EventStream] -> [EventStreamMap] -> Event -> m ()
triggerEvent est allEventStream event = do
  let streamNames = filter (elem (eventType event) . eventTypes) allEventStream
  logDebug $ "my filtered stream map" <> show streamNames
  forM_ streamNames $ \stream -> do
    case streamName stream of
      "kafkaEventStream" -> do
        let matchedConfig = find (\es -> name es == streamName stream) est
        case matchedConfig of
          Just config -> do
            let KafkaStream cfg = config.config
            fork "updating in kafka" $ streamUpdates event cfg
          _ -> logDebug "No matching Kafka config found"
      _ -> logDebug "Default stream"

createEvent :: (MonadGuid m, MonadTime m) => Text -> Text -> Text -> Text -> Text -> m Event
createEvent personId merchantId eventType deployVersion service = do
  uid <- generateGUID
  now <- getCurrentTime
  let ev =
        Event
          { id = uid,
            traceId = Just "abcde",
            sessionId = Just "session123",
            personId = personId,
            merchantId = merchantId,
            deploymentVersion = deployVersion,
            at = now,
            eventType = eventType,
            subType = Nothing,
            primaryId = Just "primary123",
            service = service,
            triggeredBy = User,
            payload = Nothing
          }
  return ev
