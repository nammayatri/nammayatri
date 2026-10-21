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
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common
import SharedLogic.AnalyticsExtra
import qualified SharedLogic.FleetOperatorStats as SFleetOperatorStats
import qualified Storage.Clickhouse.DriverInformation as CDI
import qualified Storage.Clickhouse.DriverOperatorAssociation as CDOA
import qualified Storage.Clickhouse.SubscriptionPurchase as CSubscriptionPurchase
import qualified Storage.Queries.DriverInformationExtra as QDI
import qualified Storage.Queries.DriverOperatorAssociationExtra as QDOA
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.FleetOperatorStatsExtra as QFleetOpsExtra
import qualified Storage.Queries.SubscriptionPurchaseExtra as QSubscriptionPurchase
import qualified Storage.Queries.VehicleExtra as QVehicle

fleetAnalyticsTopic :: Text
fleetAnalyticsTopic = "fleet-analytics-realtime"

-- Shorter than the consumer restart backoff (20s), so a restarted process does not
-- still see in_progress and fail the claim.
leaseTtlSeconds :: Int
leaseTtlSeconds = 10

-- Kept after a successful apply so a redelivery does not count the event again.
doneTtlSeconds :: Int
doneTtlSeconds = 86400

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
-- rebuild that is short is raised by recon. Recon also sets Redis down to the row
-- when Redis is ahead.
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
    ensureRedisKeysExistForAllTimeCommon True transporterConfig DP.OPERATOR operatorId key Redis.incrby amount

-- Association counters stay on the request path. This SET repairs the six
-- operator counters to the KV row in either direction. A null row leaves Redis.
-- One MGET and the page of stats rows. The SET under the lock uses those reads.
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
        let mbRedis = Map.lookup (makeOperatorAnalyticsKey operatorId metric) found
         in case (readMetric stats, mbRedis) of
              (Nothing, _) -> do
                logInfo $
                  "fleet analytics recon kv null, redis left unchanged operator="
                    <> operatorId
                    <> " metric="
                    <> show metric
                    <> " redis="
                    <> show mbRedis
                pure Nothing
              (Just kv, Just redisVal) | redisVal == kv -> pure Nothing
              (Just kv, _) -> pure (Just (metric, kv, mbRedis))
  pending <- catMaybes <$> mapM decide metrics
  if null pending
    then pure 0
    else Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey operatorId) 10 5000 $
      fmap sum $
        forM pending $ \(metric, kv, mbRedis) -> do
          let key = makeOperatorAnalyticsKey operatorId metric
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

-- Active drivers, active vehicles, and online drivers are current row counts, not
-- running totals. One link, vehicle, and driver-info read for the page, then one MGET.
-- Recon sets Redis to that Postgres count in either direction.
reconcileFleetLiveRedis ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r
  ) =>
  [Text] ->
  m Int
reconcileFleetLiveRedis fleetOwnerIds =
  if null fleetOwnerIds
    then pure 0
    else do
      counts <- pageFleetLiveCounts fleetOwnerIds
      let targets =
            [ (key, label, Just n)
              | fleetOwnerId <- fleetOwnerIds,
                (drivers, vehicles, online) <- maybeToList (Map.lookup fleetOwnerId counts),
                (key, label, n) <- fleetLiveKeyValues fleetOwnerId drivers vehicles online
            ]
      current <- readLiveKeys targets
      fmap sum $
        forM fleetOwnerIds $ \fleetOwnerId ->
          case Map.lookup fleetOwnerId counts of
            Nothing -> pure 0
            Just (drivers, vehicles, online) ->
              writeFleetLiveIfChanged fleetOwnerId current drivers vehicles online

pageFleetLiveCounts ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  [Text] ->
  m (Map.Map Text (Int, Int, Int))
pageFleetLiveCounts fleetOwnerIds = do
  links <- QFDA.findActiveDriverLinksByFleetOwnerIds fleetOwnerIds
  let grouped =
        Map.fromListWith
          Set.union
          [(fleetOwnerId, Set.singleton driverId.getId) | (fleetOwnerId, driverId) <- links]
      driverIds = Set.toList $ Set.unions (Map.elems grouped)
  vehicles <-
    if null driverIds
      then pure []
      else QVehicle.findAllByDriverIds (map Id.Id driverIds)
  infos <-
    if null driverIds
      then pure []
      else QDI.findAllByDriverIds driverIds
  let vehiclesByDriver = Map.fromListWith (+) [(vehicle.driverId.getId, 1 :: Int) | vehicle <- vehicles]
      onlineDrivers =
        Set.fromList
          [info.driverId.getId | info <- infos, info.driverFlowStatus == Just DDF.ONLINE]
  pure $
    Map.fromList
      [ let drivers = Map.findWithDefault Set.empty fleetOwnerId grouped
         in ( fleetOwnerId,
              ( Set.size drivers,
                sum [Map.findWithDefault 0 driverId vehiclesByDriver | driverId <- Set.toList drivers],
                Set.size (Set.intersection drivers onlineDrivers)
              )
            )
        | fleetOwnerId <- fleetOwnerIds
      ]

fleetLiveKeyValues :: Text -> Int -> Int -> Int -> [(Text, Text, Int)]
fleetLiveKeyValues fleetOwnerId drivers vehicles online =
  [ (makeFleetAnalyticsKey fleetOwnerId ACTIVE_DRIVER_COUNT, "ACTIVE_DRIVER_COUNT", drivers),
    (makeFleetAnalyticsKey fleetOwnerId ACTIVE_VEHICLE_COUNT, "ACTIVE_VEHICLE_COUNT", vehicles),
    (DDF.getStatusKey fleetOwnerId DDF.ONLINE, "ONLINE", online)
  ]

writeFleetLiveIfChanged ::
  ( MonadFlow m,
    Redis.HedisFlow m r
  ) =>
  Text ->
  Map.Map Text Int ->
  Int ->
  Int ->
  Int ->
  m Int
writeFleetLiveIfChanged fleetOwnerId current drivers vehicles online = do
  let pending =
        [ (key, label, n)
          | (key, label, n) <- fleetLiveKeyValues fleetOwnerId drivers vehicles online,
            Map.lookup key current /= Just n
        ]
  if null pending
    then pure 0
    else Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey fleetOwnerId) 10 5000 $
      fmap sum $
        forM pending $ \(key, label, n) -> do
          logInfo $
            "fleet analytics recon set live count fleetOwner="
              <> fleetOwnerId
              <> " metric="
              <> label
              <> " redis="
              <> show (Map.lookup key current)
              <> " count="
              <> show n
          Redis.set key n
          pure 1

-- Operator associated, active, and enabled counts live on fleet_operator_stats.
-- One association, subscription, and enabled read per source for the page.
-- A failed enabled read leaves every enabled column on that source unchanged.
reconcileOperatorLiveRedis ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  [DFS.FleetOperatorStats] ->
  [(Text, Bool)] ->
  m Int
reconcileOperatorLiveRedis rows operators = do
  let pgIds = [operatorId | (operatorId, True) <- operators]
      chIds = [operatorId | (operatorId, False) <- operators]
  pgCounts <- pageLiveCounts True pgIds
  chCounts <- pageLiveCounts False chIds
  let counts = Map.union pgCounts chCounts
      stats = Map.fromList [(row.fleetOperatorId, row) | row <- rows]
  fmap sum $
    forM operators $ \(operatorId, _) ->
      case (Map.lookup operatorId counts, Map.lookup operatorId stats) of
        (Just gauge, Just row) -> writeOperatorLiveIfChanged operatorId row gauge
        _ -> pure 0

pageLiveCounts ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Bool ->
  [Text] ->
  m (Map.Map Text (Int, Int, Maybe Int))
pageLiveCounts _ [] = pure Map.empty
pageLiveCounts useDb operatorIds = do
  links <-
    if useDb
      then QDOA.findActiveDriverLinksByOperatorIds operatorIds
      else CDOA.getActiveDriverLinksByOperatorIds operatorIds
  let grouped =
        Map.fromListWith
          Set.union
          [(operatorId, Set.singleton driverId.getId) | (operatorId, driverId) <- links]
      driverIds = Set.toList $ Set.unions (Map.elems grouped)
  activeOwners <-
    Set.fromList
      <$> if useDb
        then QSubscriptionPurchase.findActiveOwnerIdsByOwnerIds driverIds DSP.DRIVER
        else CSubscriptionPurchase.findActiveOwnerIdsByOwnerIds driverIds DSP.DRIVER
  mbEnabled <-
    if useDb
      then fmap Set.fromList <$> QDI.mbEnabledDriverIds driverIds
      else Just . Set.fromList . map (.getId) <$> CDI.getEnabledDriverIds (map Id.Id driverIds)
  pure $
    Map.fromList
      [ let drivers = Map.findWithDefault Set.empty operatorId grouped
         in ( operatorId,
              ( Set.size drivers,
                Set.size (Set.intersection drivers activeOwners),
                fmap (\enabled -> Set.size (Set.intersection drivers enabled)) mbEnabled
              )
            )
        | operatorId <- operatorIds
      ]

writeOperatorLiveIfChanged ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r
  ) =>
  Text ->
  DFS.FleetOperatorStats ->
  (Int, Int, Maybe Int) ->
  m Int
writeOperatorLiveIfChanged operatorId row (associated, active, mbEnabled) = do
  let changed current n = if current == Just n then Nothing else Just n
      mbAssociated = changed row.totalAssociatedDriver associated
      mbActive = changed row.totalActiveDrivers active
      mbEnabledWrite = mbEnabled >>= \n -> changed row.totalEnabledDrivers n
  if isNothing mbAssociated && isNothing mbActive && isNothing mbEnabledWrite
    then pure 0
    else
      Redis.withWaitAndLockRedis (SFleetOperatorStats.makeFleetOperatorMetricLockKey operatorId) 10 5000 $
        QFleetOpsExtra.setOperatorLiveCounts operatorId mbAssociated mbActive mbEnabledWrite

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
