{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.FleetAnalytics.Realtime
  ( FleetRealtimeEvent (..),
    OperatorRedisDelta (..),
    fleetAnalyticsTopic,
    redisCountersViaConsumer,
    publishFleetRealtimeEvent,
    applyFleetRealtimeEvent,
    reconcileOperatorRedis,
  )
where

import Data.Aeson
import qualified Data.Text as T
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as TC
import Kernel.Prelude
import qualified Kernel.Storage.Clickhouse.Config as CH
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer (produceMessage)
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Utils.Common
import SharedLogic.AnalyticsExtra
import qualified Storage.Queries.FleetOperatorStats as QFleetOps
import System.Environment (lookupEnv)

fleetAnalyticsTopic :: Text
fleetAnalyticsTopic = "fleet-analytics-realtime"

-- Longer than a Kafka retry window. A claimed event is not applied again.
eventClaimTtlSeconds :: Int
eventClaimTtlSeconds = 7 * 86400

data OperatorRedisDelta = OperatorRedisDelta
  { operatorId :: Text,
    metric :: Text,
    amount :: Integer
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data FleetRealtimeEvent = FleetRealtimeEvent
  { eventId :: Text,
    merchantOperatingCityId :: Text,
    deltas :: [OperatorRedisDelta]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

-- | Handler Redis increments for the operator counters stay on until this is "true".
-- The consumer writes those same keys. Both at once counts the event twice.
redisCountersViaConsumer :: MonadIO m => m Bool
redisCountersViaConsumer = do
  mb <- liftIO $ lookupEnv "FLEET_ANALYTICS_REDIS_VIA_CONSUMER"
  pure $ mb == Just "true"

publishFleetRealtimeEvent ::
  ( MonadFlow m,
    MonadReader r m,
    HasKafkaProducer r
  ) =>
  TC.TransporterConfig ->
  Text ->
  [OperatorRedisDelta] ->
  m ()
publishFleetRealtimeEvent transporterConfig eventId deltas =
  unless (null deltas) $ do
    let event =
          FleetRealtimeEvent
            { eventId = eventId,
              merchantOperatingCityId = transporterConfig.merchantOperatingCityId.getId,
              deltas = deltas
            }
    result <- try @_ @SomeException $ produceMessage (fleetAnalyticsTopic, Just (encodeUtf8 eventId)) event
    case result of
      Left err -> logError $ "fleet analytics publish failed for " <> eventId <> ": " <> T.pack (show err)
      Right _ -> pure ()

applyFleetRealtimeEvent ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  FleetRealtimeEvent ->
  m ()
applyFleetRealtimeEvent transporterConfig event = do
  claimed <- Redis.setNxExpire ("event:" <> event.eventId) eventClaimTtlSeconds ("1" :: Text)
  if not claimed
    then logDebug $ "fleet analytics event already applied " <> event.eventId
    else forM_ event.deltas $ applyDelta transporterConfig

applyDelta ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  OperatorRedisDelta ->
  m ()
applyDelta transporterConfig delta =
  case parseOperatorMetric delta.metric of
    Nothing -> logError $ "fleet analytics ignored metric " <> delta.metric <> " for operator " <> delta.operatorId
    Just metric -> do
      let key = makeOperatorAnalyticsKey delta.operatorId metric
      ensureRedisKeysExistForAllTimeCommon transporterConfig DP.OPERATOR delta.operatorId key Redis.incrby delta.amount

-- Association counters and driver_status stay on the request path. This SET
-- overwrites only the six operator counters the consumer increments.
reconcileOperatorRedis ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r
  ) =>
  Text ->
  m ()
reconcileOperatorRedis operatorId = do
  mbStats <- QFleetOps.findByPrimaryKey operatorId
  case mbStats of
    Nothing -> logInfo $ "fleet analytics recon: no fleet_operator_stats row for " <> operatorId
    Just stats -> do
      setMetric TOTAL_RIDE_COUNT stats.totalCompletedRides
      setMetric TOTAL_REQUEST_COUNT stats.totalRequestCount
      setMetric ACCEPTATION_COUNT stats.acceptationRequestCount
      setMetric CANCEL_COUNT stats.driverCancellationCount
      setMetric RATING_SUM stats.totalRatingScore
      setMetric RATING_COUNT stats.totalRatingCount
  where
    setMetric metric mbVal =
      whenJust mbVal $ \val ->
        Redis.set (makeOperatorAnalyticsKey operatorId metric) val

parseOperatorMetric :: Text -> Maybe AllTimeMetric
parseOperatorMetric = \case
  "TOTAL_RIDE_COUNT" -> Just TOTAL_RIDE_COUNT
  "TOTAL_REQUEST_COUNT" -> Just TOTAL_REQUEST_COUNT
  "ACCEPTATION_COUNT" -> Just ACCEPTATION_COUNT
  "CANCEL_COUNT" -> Just CANCEL_COUNT
  "RATING_SUM" -> Just RATING_SUM
  "RATING_COUNT" -> Just RATING_COUNT
  _ -> Nothing
