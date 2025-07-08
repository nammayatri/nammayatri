module SharedLogic.DriverFlowStatus where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Fleet.Driver as Common
import qualified Data.Map.Strict as Map
import qualified Domain.Types.DriverFlowStatus as DDF
import qualified Domain.Types.Person as DP
import Kernel.Prelude hiding (toList)
import qualified Kernel.Storage.Clickhouse.Config as CH
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import qualified Storage.Clickhouse.DriverInformation as CDI
import qualified Storage.Clickhouse.DriverOperatorAssociation as CDOA
import qualified Storage.Clickhouse.FleetDriverAssociation as CFDV
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
  inProgress <- Redis.get @Bool inProgressKey
  case inProgress of
    Just True -> do
      logTagInfo "DriverStatus" $ "inProgress key present for entityId: " <> entityId <> ". Waiting for it to clear."
      waitUntilKeyGone inProgressKey
      -- After waiting, re-fetch status keys and return
      redisCounts' <-
        mapM
          ( \key ->
              Redis.get @Int key >>= \v -> do
                logTagInfo "DriverStatus" $ "Redis.get (after wait) " <> key <> " => " <> show v
                pure v
          )
          allKeys
      pure $ toDriverStatusRes (zip (map Just DDF.statusList) (map (fromMaybe 0) redisCounts'))
    _ -> do
      logTagInfo "DriverStatus" $ "inProgress key not present for entityId: " <> entityId <> ". Setting and running ClickHouse query."
      Redis.setExp inProgressKey True 30 -- 30 seconds expiry
      driverIds <- case entityRole of
        DP.FLEET_OWNER -> CFDV.getDriverIdsByFleetOwnerId entityId
        DP.OPERATOR -> CDOA.getDriverIdsByOperatorId entityId
        _ -> throwError (InvalidRequest "Invalid Data")
      logTagInfo "DriverStatus" $ "Fetched driverIds from association: " <> show driverIds
      driverModeInfo <- CDI.getModeCountsByDriverIds driverIds
      logTagInfo "DriverStatus" $ "ClickHouse returned: " <> show driverModeInfo <> ". Updating Redis."
      forM_ driverModeInfo $ \(mbStatus, count) -> do
        let k = DDF.getStatusKey entityId (fromMaybe DDF.INACTIVE mbStatus)
        logTagInfo "DriverStatus" $ "Redis.set " <> k <> " = " <> show count
        Redis.set @Int k count
      Redis.del inProgressKey
      pure $ toDriverStatusRes driverModeInfo

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
  exists <- Redis.get @Bool key
  if exists == Just True
    then do
      liftIO $ threadDelay 100000 -- 100ms
      waitUntilKeyGone key
    else pure ()

getProgressKey :: Text -> Text
getProgressKey entityId = "DriverStatus:InProgress:" <> entityId
