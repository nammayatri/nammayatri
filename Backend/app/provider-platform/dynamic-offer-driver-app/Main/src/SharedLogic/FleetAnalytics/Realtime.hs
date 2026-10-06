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
    fleetAnalyticsTopic,
    publishFleetRealtimeEvent,
    withFleetEventClaim,
    incrOperatorCounter,
    reconcileOperatorsRedis,
    reconcileFleetLiveRedis,
    reconcileOperatorLiveRedis,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Domain.Types.DriverFlowStatus as DDF
import qualified Domain.Types.FleetOperatorStats as DFS
import qualified Domain.Types.Person as DP
import qualified Domain.Types.SubscriptionPurchase as DSP
import qualified Domain.Types.TransporterConfig as TC
import Kernel.Prelude
import qualified Kernel.Storage.Clickhouse.Config as CH
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer (produceMessage)
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer)
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common
import SharedLogic.AnalyticsExtra
import qualified SharedLogic.FleetOperatorStats as SFleetOperatorStats
import qualified Storage.Clickhouse.DriverInformation as CDI
import qualified Storage.Clickhouse.DriverOperatorAssociation as CDOA
import qualified Storage.Clickhouse.FleetDriverAssociation as CFDA
import qualified Storage.Clickhouse.SubscriptionPurchase as CSubscriptionPurchase
import qualified Storage.Clickhouse.Vehicle as CVehicle
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverOperatorAssociation as QDOA
import qualified Storage.Queries.FleetOperatorStats as QFleetOps
import qualified Storage.Queries.SubscriptionPurchaseExtra as QSubscriptionPurchaseExtra

fleetAnalyticsTopic :: Text
fleetAnalyticsTopic = "fleet-analytics-realtime"

-- Short lease while the consumer applies one entity's share of the event. A crash releases it so Kafka can retry.
leaseTtlSeconds :: Int
leaseTtlSeconds = 30

-- Kept after a successful apply so a redelivery does not count the event again.
doneTtlSeconds :: Int
doneTtlSeconds = 7 * 86400

-- | The fact type lives in SharedLogic.Analytics, which imports this module.
data FleetRealtimeEvent a = FleetRealtimeEvent
  { eventId :: Text,
    merchantOperatingCityId :: Text,
    payload :: a
  }
  deriving (Generic, Show, ToJSON, FromJSON)

publishFleetRealtimeEvent ::
  ( MonadFlow m,
    MonadReader r m,
    HasKafkaProducer r,
    ToJSON a
  ) =>
  TC.TransporterConfig ->
  Text ->
  a ->
  m Bool
publishFleetRealtimeEvent transporterConfig eventId analyticsFact = do
  let event =
        FleetRealtimeEvent
          { eventId = eventId,
            merchantOperatingCityId = transporterConfig.merchantOperatingCityId.getId,
            payload = analyticsFact
          }
  result <- try @_ @SomeException $ produceMessage (fleetAnalyticsTopic, Just (encodeUtf8 eventId)) event
  case result of
    Left err -> do
      logError $ "fleet analytics publish failed for " <> eventId <> ": " <> T.pack (show err)
      pure False
    Right _ -> pure True

-- | Postgres writes, then Redis counters, for one entity. "db" means Postgres is committed, so a
-- retry increments Redis only. A Postgres failure deletes the claim. A Redis failure leaves "db".
-- Taken inside the entity lock, so the lease covers the Postgres writes and not the lock wait.
withFleetEventClaim ::
  ( MonadFlow m,
    Redis.HedisFlow m r
  ) =>
  Text ->
  Text ->
  m () ->
  m () ->
  m ()
withFleetEventClaim eventId entityId postgresWrite redisWrite = do
  let claimKey = "event:" <> eventId <> ":" <> entityId
  mb <- Redis.get @Text claimKey
  case mb of
    Just "done" -> logDebug $ "fleet analytics event already applied " <> claimKey
    Just "db" -> applyRedis claimKey
    _ -> do
      leased <- Redis.setNxExpire claimKey leaseTtlSeconds ("in_progress" :: Text)
      if not leased
        then throwError $ InternalError $ "fleet analytics event in progress " <> claimKey
        else do
          pgResult <- try @_ @SomeException postgresWrite
          case pgResult of
            Left err -> do
              void $ Redis.del claimKey
              throwM err
            Right () -> do
              void $ Redis.setExp claimKey ("db" :: Text) doneTtlSeconds
              applyRedis claimKey
  where
    applyRedis claimKey = do
      redisResult <- try @_ @SomeException redisWrite
      case redisResult of
        Right () -> void $ Redis.setExp claimKey ("done" :: Text) doneTtlSeconds
        Left err -> throwM err

-- | The six operator counters events write, with the fleet_operator_stats column behind each.
operatorKvMetrics :: [(AllTimeMetric, DFS.FleetOperatorStats -> Maybe Int)]
operatorKvMetrics =
  [ (TOTAL_RIDE_COUNT, (.totalCompletedRides)),
    (TOTAL_REQUEST_COUNT, (.totalRequestCount)),
    (ACCEPTATION_COUNT, (.acceptationRequestCount)),
    (CANCEL_COUNT, (.driverCancellationCount)),
    (RATING_SUM, (.totalRatingScore)),
    (RATING_COUNT, (.totalRatingCount))
  ]

-- | The caller must hold the operator lock it took for the KV write, so recon never
-- sees the KV row and this counter on different sides of one event. A missing key is
-- rebuilt without the increment: the rebuild may already include this event, and a
-- rebuild that is short is raised by recon, which never lowers Redis.
incrOperatorCounter ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  Text ->
  AllTimeMetric ->
  Integer ->
  m ()
incrOperatorCounter transporterConfig operatorId metric amount =
  unless (amount == 0) $ do
    let key = makeOperatorAnalyticsKey operatorId metric
    mbRedis <- Redis.get @Int key
    if isJust mbRedis
      then void $ Redis.incrby key amount
      else ensureRedisKeysExistForAllTimeCommon transporterConfig DP.OPERATOR operatorId key (\_ _ -> pure 0) amount

-- Association counters stay on the request path. This SET repairs only the six
-- operator counters, and only upwards from the KV row.
-- One MGET for the page; each SET still runs under that operator's lock.
-- Returns how many of those keys were set.
reconcileOperatorsRedis ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r
  ) =>
  [DFS.FleetOperatorStats] ->
  m Int
reconcileOperatorsRedis statsList = do
  let keys =
        [ makeOperatorAnalyticsKey stats.fleetOperatorId metric
          | stats <- statsList,
            (metric, _) <- operatorKvMetrics
        ]
  found <-
    if null keys
      then pure Map.empty
      else Map.fromList <$> Redis.mGetClusterWithKeys @Int keys
  sum <$> traverse (reconcileOperatorRedis found) statsList

reconcileOperatorRedis ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r
  ) =>
  Map.Map Text Int ->
  DFS.FleetOperatorStats ->
  m Int
reconcileOperatorRedis found stats = do
  let operatorId = stats.fleetOperatorId
      metrics = operatorKvMetrics
  let decide (metric, readMetric) =
        case (readMetric stats, Map.lookup (makeOperatorAnalyticsKey operatorId metric) found) of
          (Nothing, mbRedis) -> do
            logInfo $
              "fleet analytics recon kv null, redis left unchanged operator="
                <> operatorId
                <> " metric="
                <> show metric
                <> " redis="
                <> show mbRedis
            pure Nothing
          (Just kv, Just redisVal)
            | redisVal == kv -> pure Nothing
            | redisVal > kv -> do
              logInfo $
                "fleet analytics recon redis ahead of kv, left unchanged operator="
                  <> operatorId
                  <> " metric="
                  <> show metric
                  <> " redis="
                  <> show redisVal
                  <> " kv="
                  <> show kv
              pure Nothing
          (Just _, _) -> pure (Just (metric, readMetric))
  pending <- catMaybes <$> mapM decide metrics
  if null pending
    then pure 0
    else Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey operatorId) 10 5000 $ do
      mbFresh <- QFleetOps.findByPrimaryKey operatorId
      fmap sum $
        forM pending $ \(metric, readMetric) -> do
          let key = makeOperatorAnalyticsKey operatorId metric
          mbRedis <- Redis.get @Int key
          case (mbFresh >>= readMetric, mbRedis) of
            (Just kv, Just redisVal) | redisVal >= kv -> pure 0
            (Just kv, _) -> do
              logError $
                "fleet analytics recon set from kv operator="
                  <> operatorId
                  <> " metric="
                  <> show metric
                  <> " redis="
                  <> show mbRedis
                  <> " kv="
                  <> show kv
              Redis.set key kv
              pure 1
            (Nothing, _) -> pure 0

-- Active drivers, active vehicles, and online drivers are current row counts, not
-- running totals. Recon sets Redis to the master count in either direction. A failed
-- count leaves that key unchanged. Returns how many keys were set.
reconcileFleetLiveRedis ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  [Text] ->
  m Int
reconcileFleetLiveRedis fleetOwnerIds =
  sum <$> traverse reconcileFleetOwnerLive fleetOwnerIds

reconcileFleetOwnerLive ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Text ->
  m Int
reconcileFleetOwnerLive fleetOwnerId =
  reconcileLiveCounts "fleetOwner" fleetOwnerId (fleetLiveTargets fleetOwnerId)

-- Operator all-time gauges. Same rule as the fleet live keys: set Redis to the
-- master count in either direction. A failed count leaves that key unchanged.
-- Each pair is an operator id and that city's useDbForEarningAndMetrics flag, the same
-- switch the all-time miss path uses.
reconcileOperatorLiveRedis ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  [(Text, Bool)] ->
  m Int
reconcileOperatorLiveRedis operators =
  sum <$> traverse reconcileOperatorLive operators

reconcileOperatorLive ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  (Text, Bool) ->
  m Int
reconcileOperatorLive (operatorId, useDbForAnalytics) =
  reconcileLiveCounts "operator" operatorId (operatorLiveTargets useDbForAnalytics operatorId)

reconcileLiveCounts ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r
  ) =>
  Text ->
  Text ->
  m [(Text, Text, Maybe Int)] ->
  m Int
reconcileLiveCounts role entityId loadTargets = do
  firstPass <- loadTargets
  current <- readLiveKeys firstPass
  let pending = [(key, label) | (key, label, Just n) <- firstPass, Map.lookup key current /= Just n]
  if null pending
    then pure 0
    else Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey entityId) 10 5000 $ do
      fresh <- loadTargets
      fmap sum $
        forM fresh $ \(key, label, mbCount) ->
          case mbCount of
            Nothing -> pure 0
            Just n -> do
              mbRedis <- Redis.get @Int key
              if mbRedis == Just n
                then pure 0
                else do
                  logInfo $
                    "fleet analytics recon set live count "
                      <> role
                      <> "="
                      <> entityId
                      <> " metric="
                      <> label
                      <> " redis="
                      <> show mbRedis
                      <> " count="
                      <> show n
                  Redis.set key n
                  pure 1

-- Same counts as fallbackToClickHouseAndUpdateRedisForAllTimeFleet. Active drivers are
-- the ClickHouse fleet links with isActive. Active vehicles are vehicle rows for those
-- drivers. Online is the ONLINE bucket from the same driver-status rebuild.
fleetLiveTargets ::
  ( MonadFlow m,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Text ->
  m [(Text, Text, Maybe Int)]
fleetLiveTargets fleetOwnerId = do
  mbDriverIds <- CFDA.getDriverIdsByFleetOwnerId fleetOwnerId
  let mbDrivers = length <$> mbDriverIds
  mbVehicles <- maybe (pure Nothing) CVehicle.countByDriverIds mbDriverIds
  mbOnline <- case mbDriverIds of
    Nothing -> pure Nothing
    Just driverIds -> do
      modeCounts <- CDI.getModeCountsByDriverIds driverIds
      pure $ Just $ fromMaybe 0 $ lookup (Just DDF.ONLINE) modeCounts
  pure
    [ (makeFleetAnalyticsKey fleetOwnerId ACTIVE_DRIVER_COUNT, "ACTIVE_DRIVER_COUNT", mbDrivers),
      (makeFleetAnalyticsKey fleetOwnerId ACTIVE_VEHICLE_COUNT, "ACTIVE_VEHICLE_COUNT", mbVehicles),
      (DDF.getStatusKey fleetOwnerId DDF.ONLINE, "ONLINE", mbOnline)
    ]

-- Same counts as fallbackToClickHouseAndUpdateRedisForAllTime. useDbForAnalytics picks
-- Postgres or ClickHouse, matching that miss path.
operatorLiveTargets ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Bool ->
  Text ->
  m [(Text, Text, Maybe Int)]
operatorLiveTargets useDbForAnalytics operatorId = do
  operatorAssociatedDriverIds <-
    if useDbForAnalytics
      then QDOA.getActiveDriverIdsByOperatorId operatorId
      else CDOA.getDriverIdsByOperatorId operatorId
  let associated = Just $ Set.size $ Set.fromList operatorAssociatedDriverIds
      driverOwnerIds = (.getId) <$> operatorAssociatedDriverIds
  activeDrivers <-
    if useDbForAnalytics
      then QSubscriptionPurchaseExtra.findActiveDistinctOwnersByOwnerIds driverOwnerIds DSP.DRIVER
      else CSubscriptionPurchase.findActiveDistinctOwnersByOwnerIds driverOwnerIds DSP.DRIVER
  mbEnabled <-
    if useDbForAnalytics
      then QDI.mbCountEnabledByDriverIds driverOwnerIds
      else Just <$> CDI.countEnabledByDriverIds operatorAssociatedDriverIds
  pure
    [ (makeOperatorAnalyticsKey operatorId TOTAL_ASSOCIATED_DRIVER, "TOTAL_ASSOCIATED_DRIVER", associated),
      (makeOperatorAnalyticsKey operatorId TOTAL_ACTIVE_DRIVERS, "TOTAL_ACTIVE_DRIVERS", Just activeDrivers),
      (makeOperatorAnalyticsKey operatorId TOTAL_ENABLED_DRIVERS, "TOTAL_ENABLED_DRIVERS", mbEnabled)
    ]

readLiveKeys ::
  ( MonadFlow m,
    Redis.HedisFlow m r
  ) =>
  [(Text, Text, Maybe Int)] ->
  m (Map.Map Text Int)
readLiveKeys targets = do
  let keys = [key | (key, _, Just _) <- targets]
  if null keys
    then pure Map.empty
    else Map.fromList <$> Redis.mGetClusterWithKeys @Int keys
