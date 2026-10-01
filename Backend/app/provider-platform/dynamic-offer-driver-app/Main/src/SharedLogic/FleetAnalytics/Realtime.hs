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
    syncOperatorRedis,
    applyFleetRealtimeEvent,
    reconcileOperatorRedis,
  )
where

import Data.Aeson
import qualified Data.Map.Strict as Map
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

-- | One place decides whether the handler or the consumer writes operator Redis.
-- `messages` is one Kafka event per item: the event id and the deltas it carries.
syncOperatorRedis ::
  ( MonadFlow m,
    MonadReader r m,
    HasKafkaProducer r,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  [(Text, [OperatorRedisDelta])] ->
  m ()
syncOperatorRedis transporterConfig messages = do
  viaConsumer <- redisCountersViaConsumer
  case viaConsumer of
    True -> forM_ messages $ \(eventId, deltas) -> publishFleetRealtimeEvent transporterConfig eventId deltas
    False ->
      forM_ (Map.toList grouped) $ \((operatorId, metric), amount) ->
        applyDelta transporterConfig (OperatorRedisDelta operatorId metric amount)
  where
    grouped =
      Map.fromListWith (+) [((delta.operatorId, delta.metric), delta.amount) | (_, deltas) <- messages, delta <- deltas]

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
applyFleetRealtimeEvent transporterConfig event =
  forM_ event.deltas $ \delta -> do
    let claimKey = "event:" <> event.eventId <> ":" <> delta.operatorId <> ":" <> delta.metric
    claimed <- Redis.setNxExpire claimKey eventClaimTtlSeconds ("1" :: Text)
    if not claimed
      then logDebug $ "fleet analytics delta already applied " <> claimKey
      else applyDelta transporterConfig delta

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
-- Returns how many of those six keys were missing or different from Postgres.
reconcileOperatorRedis ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r
  ) =>
  Text ->
  m Int
reconcileOperatorRedis operatorId = do
  mbStats <- QFleetOps.findByPrimaryKey operatorId
  case mbStats of
    Nothing -> do
      logInfo $ "fleet analytics recon: no fleet_operator_stats row for " <> operatorId
      pure 0
    Just stats ->
      sum
        <$> sequence
          [ compareMetric TOTAL_RIDE_COUNT stats.totalCompletedRides,
            compareMetric TOTAL_REQUEST_COUNT stats.totalRequestCount,
            compareMetric ACCEPTATION_COUNT stats.acceptationRequestCount,
            compareMetric CANCEL_COUNT stats.driverCancellationCount,
            compareMetric RATING_SUM stats.totalRatingScore,
            compareMetric RATING_COUNT stats.totalRatingCount
          ]
  where
    compareMetric metric Nothing = do
      mbRedis <- Redis.get @Int (makeOperatorAnalyticsKey operatorId metric)
      logInfo $
        "fleet analytics recon postgres null, redis left unchanged operator="
          <> operatorId
          <> " metric="
          <> show metric
          <> " redis="
          <> show mbRedis
      pure 0
    compareMetric metric (Just pg) = do
      let key = makeOperatorAnalyticsKey operatorId metric
      mbRedis <- Redis.get @Int key
      case mbRedis of
        Nothing -> do
          logError $
            "fleet analytics recon missing redis key, set from postgres operator="
              <> operatorId
              <> " metric="
              <> show metric
              <> " postgres="
              <> show pg
          Redis.set key pg
          pure 1
        Just redisVal
          | redisVal == pg -> pure 0
          | otherwise -> do
            let reason =
                  if redisVal > pg
                    then "redis ahead of postgres" :: Text
                    else "redis behind postgres"
            logError $
              "fleet analytics recon drift operator="
                <> operatorId
                <> " metric="
                <> show metric
                <> " redis="
                <> show redisVal
                <> " postgres="
                <> show pg
                <> " delta="
                <> show (redisVal - pg)
                <> " reason="
                <> reason
            Redis.set key pg
            pure 1

parseOperatorMetric :: Text -> Maybe AllTimeMetric
parseOperatorMetric = \case
  "TOTAL_RIDE_COUNT" -> Just TOTAL_RIDE_COUNT
  "TOTAL_REQUEST_COUNT" -> Just TOTAL_REQUEST_COUNT
  "ACCEPTATION_COUNT" -> Just ACCEPTATION_COUNT
  "CANCEL_COUNT" -> Just CANCEL_COUNT
  "RATING_SUM" -> Just RATING_SUM
  "RATING_COUNT" -> Just RATING_COUNT
  _ -> Nothing
