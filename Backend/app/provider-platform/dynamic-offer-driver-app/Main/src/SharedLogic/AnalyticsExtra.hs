module SharedLogic.AnalyticsExtra where

import Domain.Types.Person as DP
import Kernel.Prelude
import qualified Kernel.Storage.Clickhouse.Config as CH
import qualified Kernel.Storage.ClickhouseV2 as CHV2
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.CacheFlow
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Domain.Types.TransporterConfig as TC
import qualified Data.Map as Map
import qualified Storage.Clickhouse.FleetOperatorStats as CFO
import qualified Storage.Queries.DriverOperatorAssociation as QDOA
import qualified Storage.Clickhouse.DriverOperatorAssociation as CDOA
import qualified Storage.Clickhouse.FleetDriverAssociation as CFDA
import qualified Storage.Clickhouse.Vehicle as CVehicle
import qualified Data.Set as Set
import qualified Storage.Queries.SubscriptionPurchaseExtra as QSubscriptionPurchaseExtra
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Clickhouse.SubscriptionPurchase as CSubscriptionPurchase
import qualified Storage.Clickhouse.DriverInformation as CDI
import qualified Domain.Types.SubscriptionPurchase as DSP
import qualified SharedLogic.DriverFlowStatus as SDFStatus
import qualified Domain.Types.DriverFlowStatus as DDF
import Tools.Error (GenericError(InternalError), GenericError(InvalidRequest))

data FleetAllTimeMetric = ACTIVE_DRIVER_COUNT | ACTIVE_VEHICLE_COUNT
  deriving (Show, Eq, Ord)

data AllTimeMetric = TOTAL_RIDE_COUNT | RATING_SUM | RATING_COUNT | CANCEL_COUNT | ACCEPTATION_COUNT | TOTAL_REQUEST_COUNT | TOTAL_ASSOCIATED_DRIVER | TOTAL_ACTIVE_DRIVERS | TOTAL_ENABLED_DRIVERS
  deriving (Show, Eq, Ord)

-- | Common datatype for all analytics fallback results
data CommonAllTimeFallbackRes
  = OperatorAllTimeFallback AllTimeFallbackRes
  | FleetAllTimeFallback FleetAllTimeFallbackRes
  deriving (Show, Eq)

fleetAllTimeMetrics :: [FleetAllTimeMetric]
fleetAllTimeMetrics = [ACTIVE_DRIVER_COUNT, ACTIVE_VEHICLE_COUNT]

data AllTimeFallbackRes = AllTimeFallbackRes
  { totalRideCount :: Maybe Int,
    ratingSum :: Maybe Int,
    ratingCount :: Maybe Int,
    cancelCount :: Maybe Int,
    acceptationCount :: Maybe Int,
    totalRequestCount :: Maybe Int,
    totalAssociatedDriver :: Maybe Int,
    totalActiveDrivers :: Maybe Int,
    totalEnabledDrivers :: Maybe Int
  }
  deriving (Show, Eq)

data FleetAllTimeFallbackRes = FleetAllTimeFallbackRes
  { activeDriverCount :: Maybe Int,
    activeVehicleCount :: Maybe Int,
    currentOnlineDriverCount :: Maybe Int
  }
  deriving (Show, Eq)

allTimeMetrics :: [AllTimeMetric]
allTimeMetrics = [TOTAL_RIDE_COUNT, RATING_SUM, RATING_COUNT, CANCEL_COUNT, ACCEPTATION_COUNT, TOTAL_REQUEST_COUNT, TOTAL_ASSOCIATED_DRIVER, TOTAL_ACTIVE_DRIVERS, TOTAL_ENABLED_DRIVERS]

allTimeKeys :: Text -> [Text]
allTimeKeys operatorId = map (makeOperatorAnalyticsKey operatorId) allTimeMetrics

-- | Zip keys with optional values, dropping entries where the value is Nothing.
zipJusts :: [a] -> [Maybe b] -> [(a, b)]
zipJusts keys mbVals = [(k, v) | (k, Just v) <- zip keys mbVals]

-- | Redis key functions for operator analytics
makeOperatorKeyPrefix :: Text -> Text
makeOperatorKeyPrefix operatorId = "operator:" <> operatorId <> ":"

makeFleetKeyPrefix :: Text -> Text
makeFleetKeyPrefix fleetOperatorId = "fleetOwner:" <> fleetOperatorId <> ":"

makeOperatorAnalyticsKey :: Text -> AllTimeMetric -> Text
makeOperatorAnalyticsKey operatorId metric = makeOperatorKeyPrefix operatorId <> show metric

makeFleetAnalyticsKey :: Text -> FleetAllTimeMetric -> Text
makeFleetAnalyticsKey fleetOperatorId metric = makeFleetKeyPrefix fleetOperatorId <> show metric

fleetAllTimeKeys :: Text -> [Text]
fleetAllTimeKeys fleetOperatorId = map (makeFleetAnalyticsKey fleetOperatorId) fleetAllTimeMetrics

convertToAllTimeFallbackRes :: [(AllTimeMetric, Int)] -> CommonAllTimeFallbackRes
convertToAllTimeFallbackRes metricsList =
  let metricsMap = Map.fromList metricsList
      getMetricValue metric = Map.lookup metric metricsMap
   in OperatorAllTimeFallback $
        AllTimeFallbackRes
          { totalRideCount = getMetricValue TOTAL_RIDE_COUNT,
            ratingSum = getMetricValue RATING_SUM,
            ratingCount = getMetricValue RATING_COUNT,
            cancelCount = getMetricValue CANCEL_COUNT,
            acceptationCount = getMetricValue ACCEPTATION_COUNT,
            totalRequestCount = getMetricValue TOTAL_REQUEST_COUNT,
            totalAssociatedDriver = getMetricValue TOTAL_ASSOCIATED_DRIVER,
            totalActiveDrivers = getMetricValue TOTAL_ACTIVE_DRIVERS,
            totalEnabledDrivers = getMetricValue TOTAL_ENABLED_DRIVERS
          }

-- | Common function to ensure Redis keys exist by falling back to ClickHouse if needed
-- Uses Person role to determine which fallback function to call
ensureRedisKeysExistForAllTimeCommon ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  DP.Role ->
  Text ->
  Text ->
  (Text -> Integer -> m Integer) ->
  Integer ->
  m ()
ensureRedisKeysExistForAllTimeCommon transporterConfig role entityId targetKey redisOps value = do
  mbValue <- Redis.get @Int targetKey
  (msg, logTag, allTimeKeysData, handleCacheMiss) <- case role of
    DP.OPERATOR ->
      pure
        ( "Key does not exist for operator alltime analytics key: " <> show targetKey <> ", operatorId: " <> show entityId,
          "AllTimeAnalytics",
          allTimeKeys entityId,
          handleCacheMissForAnalyticsAllTimeCommon transporterConfig DP.OPERATOR entityId
        )
    DP.FLEET_OWNER ->
      pure
        ( "Key does not exist for fleet alltime analytics key: " <> show targetKey <> ", fleetOwnerId: " <> show entityId,
          "AllTimeAnalyticsFleet",
          fleetAllTimeKeys entityId,
          handleCacheMissForAnalyticsAllTimeCommon transporterConfig DP.FLEET_OWNER entityId
        )
    _ -> throwError $ InvalidRequest $ "Unsupported role for analytics: " <> show role

  if isNothing mbValue
    then fork msg $ do
      logTagInfo logTag $ "Key does not exist. Handling cache miss for " <> show role <> "Id: " <> show entityId
      void $ handleCacheMiss allTimeKeysData
      void $ redisOps targetKey value
      logTagInfo logTag $ "Updated key after cache miss: key=" <> show targetKey <> ", " <> show role <> "Id=" <> show entityId
    else do
      logTagInfo logTag $ "Key already exists for " <> show role <> " analytics key: " <> show targetKey <> ", " <> show role <> "Id=" <> show entityId
      void $ redisOps targetKey value

-- | ClickHouse fallback function for fleet analytics
fallbackToClickHouseAndUpdateRedisForAllTime ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    CHV2.HasClickhouseEnv CHV2.APP_SERVICE_CLICKHOUSE m,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  Text ->
  [Text] ->
  m CommonAllTimeFallbackRes
fallbackToClickHouseAndUpdateRedisForAllTime transporterConfig operatorId allTimeKeysData = do
  logTagInfo "FallbackClickhouseAllTime" $ "Initiating ClickHouse query to retrieve analytics data for operator ID: " <> operatorId
  let useDBForAnalytics = transporterConfig.analyticsConfig.useDbForEarningAndMetrics
  operatorStats <- CFO.sumStatsByFleetOperatorId operatorId
  let tcr = operatorStats.totalCompletedRidesSum
      trn = operatorStats.totalRatingCountSum
      rs = operatorStats.totalRatingScoreSum
      dcc = operatorStats.driverCancellationCountSum
      ac = operatorStats.acceptationRequestCountSum
      trc = operatorStats.totalRequestCountSum
  operatorAssociatedDriverIds <- if useDBForAnalytics then QDOA.getActiveDriverIdsByOperatorId operatorId else CDOA.getDriverIdsByOperatorId operatorId
  let tad = Just $ Set.size (Set.fromList operatorAssociatedDriverIds)
  let driverOwnerIds = (.getId) <$> operatorAssociatedDriverIds
  activeDrivers <- if useDBForAnalytics then QSubscriptionPurchaseExtra.findActiveDistinctOwnersByOwnerIds driverOwnerIds DSP.DRIVER else CSubscriptionPurchase.findActiveDistinctOwnersByOwnerIds driverOwnerIds DSP.DRIVER
  let tadv = Just activeDrivers

  ted <-
    if useDBForAnalytics
      then Just <$> QDI.countEnabledByDriverIds driverOwnerIds
      else Just <$> CDI.countEnabledByDriverIds operatorAssociatedDriverIds

  mapM_ (uncurry Redis.set) (zipJusts allTimeKeysData [tcr, rs, trn, dcc, ac, trc, tad, tadv, ted])
  pure $ convertToAllTimeFallbackRes (zipJusts allTimeMetrics [tcr, rs, trn, dcc, ac, trc, tad, tadv, ted])

convertToFleetAllTimeFallbackRes :: [(FleetAllTimeMetric, Int)] -> Maybe Int -> CommonAllTimeFallbackRes
convertToFleetAllTimeFallbackRes metricsList mbCurrentOnlineDriverCount =
  let metricsMap = Map.fromList metricsList
      getMetricValue metric = Map.lookup metric metricsMap
   in FleetAllTimeFallback $
        FleetAllTimeFallbackRes
          { activeDriverCount = getMetricValue ACTIVE_DRIVER_COUNT,
            activeVehicleCount = getMetricValue ACTIVE_VEHICLE_COUNT,
            currentOnlineDriverCount = mbCurrentOnlineDriverCount
          }

fallbackToClickHouseAndUpdateRedisForAllTimeFleet ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Text ->
  [Text] ->
  m CommonAllTimeFallbackRes
fallbackToClickHouseAndUpdateRedisForAllTimeFleet fleetOwnerId fleetAllTimeKeysData = do
  logTagInfo "FallbackClickhouseAllTimeFleet" $ "Initiating ClickHouse query to retrieve analytics data for fleetOwnerId: " <> fleetOwnerId
  mbDriverIds <- CFDA.getDriverIdsByFleetOwnerId fleetOwnerId
  let mbActiveDriverCount = length <$> mbDriverIds
  mbActiveVehicleCount <- maybe (pure Nothing) (CVehicle.countByDriverIds) mbDriverIds
  mbCurrentOnlineDriverCount <- getOnlineDriverCount
  logTagInfo "fallbackClickhouseAllTimeFleet" ("mbActiveDriverCount: " <> show mbActiveDriverCount <> ", mbActiveVehicleCount: " <> show mbActiveVehicleCount <> ", mbCurrentOnlineDriverCount: " <> show mbCurrentOnlineDriverCount)

  mapM_ (uncurry Redis.set) (zipJusts fleetAllTimeKeysData [mbActiveDriverCount, mbActiveVehicleCount])

  pure $ convertToFleetAllTimeFallbackRes (zipJusts fleetAllTimeMetrics [mbActiveDriverCount, mbActiveVehicleCount]) mbCurrentOnlineDriverCount
  where
    getOnlineDriverCount = do
      res <- DDF.getOnlineKeyValue fleetOwnerId
      if isNothing res
        then do
          void $ SDFStatus.handleCacheMissForDriverFlowStatus DP.FLEET_OWNER fleetOwnerId (DDF.allKeys fleetOwnerId)
          onlineRes <- DDF.getOnlineKeyValue fleetOwnerId
          pure onlineRes
        else pure res

-- | Common function to handle cache miss for analytics using Person role
handleCacheMissForAnalyticsAllTimeCommon ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  DP.Role ->
  Text ->
  [Text] ->
  m CommonAllTimeFallbackRes
handleCacheMissForAnalyticsAllTimeCommon transporterConfig role entityId allTimeKeysData = do
  (inProgressKey, logTag, fallbackFunc) <- case role of
    DP.OPERATOR ->
      pure
        ( makeOperatorKeyPrefix entityId <> "inProgressAllTime",
          "OperatorAnalyticsAllTime",
          fallbackToClickHouseAndUpdateRedisForAllTime transporterConfig entityId
        )
    DP.FLEET_OWNER ->
      pure
        ( makeFleetKeyPrefix entityId <> "inProgressAllTime",
          "FleetAnalyticsAllTime",
          fallbackToClickHouseAndUpdateRedisForAllTimeFleet entityId
        )
    _ -> throwError $ InvalidRequest $ "Unsupported role for analytics: " <> show role

  inProgress <- Redis.get @Bool inProgressKey
  case inProgress of
    Just True -> do
      logTagInfo logTag $ "inProgress key present for " <> show role <> "Id: " <> entityId <> ". Waiting for it to clear."
      SDFStatus.waitUntilKeyGone inProgressKey
      allTimeKeysRes <- mapM (\key -> Redis.get @Int key) allTimeKeysData
      logTagInfo logTag $ "allTimeKeysRes: " <> show allTimeKeysRes
      case role of
        DP.OPERATOR -> pure $ convertToAllTimeFallbackRes (zipJusts allTimeMetrics allTimeKeysRes)
        DP.FLEET_OWNER -> do
          mbCurrentOnlineDriverCount <- DDF.getOnlineKeyValue entityId
          pure $ convertToFleetAllTimeFallbackRes (zipJusts fleetAllTimeMetrics allTimeKeysRes) mbCurrentOnlineDriverCount
        _ -> throwError $ InvalidRequest $ "Unsupported role for analytics: " <> show role
    _ -> do
      lockAcquired <- Redis.setNxExpire inProgressKey 60 True -- 60 seconds expiry
      if lockAcquired
        then do
          logTagInfo logTag $ "Acquired inProgress lock for " <> show role <> "Id: " <> entityId <> ". Running ClickHouse query."
          res <- withTryCatch "fallbackFunc:handleCacheMissForAnalyticsAllTimeCommon" $ fallbackFunc allTimeKeysData
          Redis.del inProgressKey
          case res of
            Left err -> do
              logTagError logTag $ "Error during ClickHouse/Redis operation for " <> show role <> "Id: " <> entityId <> ". Error: " <> show err
              throwError (InternalError $ "Failed to perform operation for " <> show role <> "Id: " <> entityId)
            Right allTimeRes -> pure allTimeRes
        else do
          logTagInfo logTag $ "inProgress lock already held for " <> show role <> "Id: " <> entityId <> " (race detected in else). Waiting for it to clear."
          handleCacheMissForAnalyticsAllTimeCommon transporterConfig role entityId allTimeKeysData

adjustOperatorAllTimeAnalyticsMetric ::
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
adjustOperatorAllTimeAnalyticsMetric transporterConfig operatorIdText metric delta =
  when transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics $
    when (delta /= 0) $ do
      let key = makeOperatorAnalyticsKey operatorIdText metric
      if delta > 0
        then ensureRedisKeysExistForAllTimeCommon transporterConfig DP.OPERATOR operatorIdText key Redis.incrby delta
        else ensureRedisKeysExistForAllTimeCommon transporterConfig DP.OPERATOR operatorIdText key Redis.decrby (abs delta)

adjustOperatorDriverAssociationAnalytics ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  Text ->
  Integer ->
  Int ->
  Bool ->
  m ()
adjustOperatorDriverAssociationAnalytics transporterConfig operatorId delta activeSubscriptionCount driverEnabled = do
  adjustOperatorAllTimeAnalyticsMetric transporterConfig operatorId TOTAL_ASSOCIATED_DRIVER delta
  when (activeSubscriptionCount > 0) $
    adjustOperatorAllTimeAnalyticsMetric transporterConfig operatorId TOTAL_ACTIVE_DRIVERS delta
  when driverEnabled $
    adjustOperatorAllTimeAnalyticsMetric transporterConfig operatorId TOTAL_ENABLED_DRIVERS delta

findOperatorIdForDriver ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DP.Person ->
  m [Text]
findOperatorIdForDriver driverId = do
  -- First try to find direct driver-operator association
  mbDriverOperatorAssoc <- QDOA.findByDriverId driverId True
  case mbDriverOperatorAssoc of
    Just assoc -> pure [assoc.operatorId]
    Nothing -> pure []


decrementOperatorTotalActiveDriversIfDriverHasNoActiveSubscription ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  TC.TransporterConfig ->
  Text ->
  m ()
decrementOperatorTotalActiveDriversIfDriverHasNoActiveSubscription transporterConfig driverOwnerIdText =
  when transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics $ do
    operatorIds <- findOperatorIdForDriver (Id driverOwnerIdText)
    unless (null operatorIds) $ do
      activeRemaining <- QSubscriptionPurchaseExtra.countActiveSubscriptionsForOwner driverOwnerIdText DSP.DRIVER
      when (activeRemaining == 0) $
        forM_ operatorIds $ \oid ->
          adjustOperatorAllTimeAnalyticsMetric transporterConfig oid TOTAL_ACTIVE_DRIVERS (-1)