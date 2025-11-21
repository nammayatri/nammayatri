module SharedLogic.DriverFlowStatus where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Driver as Common
import qualified Data.Map.Strict as Map
import qualified Domain.Types.DriverFlowStatus as DDF
import qualified Domain.Types.Person as DP
import Kernel.Prelude hiding (toList)
import qualified Kernel.Storage.Clickhouse.Config as CH
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Clickhouse.DriverInformation as CDI
import qualified Storage.Clickhouse.DriverOperatorAssociation as CDOA
import qualified Storage.Clickhouse.FleetDriverAssociation as CFDA
import qualified Storage.Clickhouse.FleetOperatorAssociation as CFOA
import Tools.Error

handleCacheMissForDriverFlowStatus ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  DP.Role ->
  Text ->
  [Text] ->
  m Common.DriverStatusRes
handleCacheMissForDriverFlowStatus entityRole entityId allKeys = do
  let inProgressKey = getProgressKey entityId
  inProgress <- Redis.withMasterRedis $ Redis.get @Bool inProgressKey
  case inProgress of
    Just True -> do
      logTagInfo "DriverStatus" $ "inProgress key present for entityId: " <> entityId <> ". Waiting for it to clear."
      waitUntilKeyGone inProgressKey
      -- After waiting, re-fetch status keys and return
      redisCounts' <-
        mapM
          ( \key ->
              Redis.withMasterRedis (Redis.get @Int key) >>= \v -> do
                logTagInfo "DriverStatus" $ "Redis.get (after wait) " <> key <> " => " <> show v
                pure v
          )
          allKeys
      pure $ toDriverStatusRes (zip (map Just DDF.statusList) (map (fromMaybe 0) redisCounts'))
    _ -> do
      lockAcquired <- Redis.setNxExpire inProgressKey 30 True -- 30 seconds expiry
      if lockAcquired
        then do
          logTagInfo "DriverStatus" $ "Acquired inProgress lock for entityId: " <> entityId <> ". Running ClickHouse query."
          res <- withTryCatch "handleCacheMissForDriverFlowStatus" $ do
            driverIds <- case entityRole of
              DP.FLEET_OWNER -> do
                mbDriverIds <- CFDA.getDriverIdsByFleetOwnerId entityId
                case mbDriverIds of
                  Just driverIds -> pure driverIds
                  Nothing -> pure []
              DP.OPERATOR -> getFleetDriverIdsAndDriverIdsByOperatorId entityId
              _ -> throwError (InvalidRequest "Invalid Data")
            logTagInfo "DriverStatus" $ "Fetched driverIds from association: " <> show driverIds
            driverModeInfo <- CDI.getModeCountsByDriverIds driverIds
            logTagInfo "DriverStatus" $ "ClickHouse returned: " <> show driverModeInfo <> ". Updating Redis."
            forM_ driverModeInfo $ \(mbStatus, count) -> do
              let k = DDF.getStatusKey entityId (fromMaybe DDF.INACTIVE mbStatus)
              logTagInfo "DriverStatus" $ "Redis.set " <> k <> " = " <> show count
              Redis.set @Int k count
            pure $ toDriverStatusRes driverModeInfo
          Redis.del inProgressKey
          case res of
            Left err -> do
              logTagError "DriverStatus" $ "Error during ClickHouse/Redis to get driver count under each status for entityId: " <> entityId <> ". Error: " <> show err
              throwError (InternalError $ "Failed to get driver count under each status for entityId: " <> entityId)
            Right statusRes -> pure statusRes
        else do
          logTagInfo "DriverStatus" $ "inProgress lock already held for entityId: " <> entityId <> " (race detected in else). Waiting for it to clear."
          handleCacheMissForDriverFlowStatus entityRole entityId allKeys

getFleetDriverIdsAndDriverIdsByOperatorId ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Text ->
  m [Id DP.Person]
getFleetDriverIdsAndDriverIdsByOperatorId operatorId = do
  fleetOwnerIds <- CFOA.getFleetOwnerIdsByOperatorId operatorId
  fleetDriverIds <-
    if null fleetOwnerIds
      then pure []
      else CFDA.getDriverIdsByFleetOwnerIds fleetOwnerIds
  driverIds <- CDOA.getDriverIdsByOperatorId operatorId
  pure (driverIds <> fleetDriverIds)

getTotalFleetDriverAndDriverCountByOperatorIdInDateRange ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Text ->
  UTCTime ->
  UTCTime ->
  m Int
getTotalFleetDriverAndDriverCountByOperatorIdInDateRange operatorId from to = do
  fleetOwnerIds <- CFOA.getFleetOwnerIdsByOperatorId operatorId
  fleetDriverCount <-
    if null fleetOwnerIds
      then pure 0
      else CFDA.getTotalDriverCountByFleetOwnerIdsInDateRange fleetOwnerIds from to
  driverCount <- CDOA.getTotalDriverCountByOperatorIdInDateRange operatorId from to
  pure (fleetDriverCount + driverCount)

toDriverStatusRes :: [(Maybe DDF.DriverFlowStatus, Int)] -> Common.DriverStatusRes
toDriverStatusRes xs =
  let m = Map.fromList xs
      get s = Map.findWithDefault 0 (Just s) m
      getInactive = get DDF.INACTIVE + Map.findWithDefault 0 Nothing m
   in Common.DriverStatusRes
        { online = get DDF.ONLINE,
          offline = get DDF.OFFLINE,
          silent = get DDF.SILENT,
          toPickup = get DDF.ON_PICKUP,
          onRide = get DDF.ON_RIDE,
          active = get DDF.ACTIVE,
          inactive = getInactive
        }

-- Helper to wait until a Redis key is gone (polling every 100ms)
waitUntilKeyGone :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r) => Text -> m ()
waitUntilKeyGone key = do
  exists <- Redis.withMasterRedis $ Redis.get @Bool key
  when (exists == Just True) $ do
    liftIO $ threadDelay 100000 -- 100ms
    waitUntilKeyGone key

getProgressKey :: Text -> Text
getProgressKey entityId = "DriverStatus:InProgress:" <> entityId
