{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-uni-patterns #-}

module Lib.SessionizerMetrics.EventStream where

import Kernel.Prelude hiding (at, traceId)
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Kafka.Internal
import Lib.SessionizerMetrics.Prometheus.Internal
import Lib.SessionizerMetrics.Types.Event

triggerEvent ::
  ( EventStreamFlow m r,
    ToJSON p
  ) =>
  Event p ->
  m ()
triggerEvent event = do
  allEventStream <- asks (.eventStreamMap)
  let streamNames = filter (elem (eventType event) . eventTypes) allEventStream
  logDebug $ "my filtered stream map" <> show streamNames
  forM_ streamNames $ \stream -> do
    case streamName stream of
      KAFKA_STREAM -> do
        let KafkaStream matchedConfig = stream.streamConfig
        fork "updating in kafka" $ streamUpdates event matchedConfig
      PROMETHEUS_STREAM -> do
        let merchantId = event.merchantId
        let eventType = show event.eventType
        let deploymentVersion = event.deploymentVersion
        fork "updating in prometheus" $ incrementCounter merchantId eventType deploymentVersion
      _ -> logDebug "Default stream"

createEvent :: (MonadReader r1 m, MonadGuid m, MonadTime m, HasField "getDeploymentVersion" r2 Text, HasField "version" r1 r2) => Maybe Text -> Text -> EventType -> Service -> EventTriggeredBy -> Maybe p -> Maybe Text -> Maybe Text -> m (Event p)
createEvent personId merchantId eventType service triggredBy payload primaryId merchantOperatingCityId = do
  version <- asks (.version)
  uid <- generateGUID
  now <- getCurrentTime
  let ev =
        Event
          { id = uid,
            traceId = Nothing,
            sessionId = Nothing,
            personId = personId,
            merchantId = merchantId,
            deploymentVersion = version.getDeploymentVersion,
            at = utcToMilliseconds now,
            eventType = eventType,
            subType = Nothing,
            primaryId = primaryId,
            service = service,
            triggeredBy = triggredBy,
            payload = payload,
            ..
          }
  return ev
