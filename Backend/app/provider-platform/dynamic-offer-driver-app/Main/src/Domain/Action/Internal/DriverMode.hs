{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.DriverMode where

import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Domain.Types.Common as DriverInfo
import qualified Domain.Types.DriverFlowStatus as DDFS
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Person as DP
import Environment
import EulerHS.Prelude
import qualified Kernel.Storage.Clickhouse.Config as CH
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverFlowStatus as SDF
import qualified Storage.Queries.DriverInformation as QDriverInformation
import qualified Storage.Queries.DriverOperatorAssociationExtra as QDriverOperatorAssociationExtra
import qualified Storage.Queries.FleetDriverAssociationExtra as QFleetDriverAssociationExtra
import qualified Storage.Queries.FleetOperatorAssociation as QFleetOperatorAssociation
import Tools.Error

data DriverModeReq = DriverModeReq
  { driverId :: Id DP.Person,
    mode :: DriverInfo.DriverMode,
    isActive :: Bool,
    allowCacheDriverFlowStatus :: Maybe Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

setDriverMode :: (HasField "locationTrackingServiceKey" AppEnv Text) => Maybe Text -> DriverModeReq -> Flow APISuccess
setDriverMode apiKey req = do
  let driverId = req.driverId
      mode = req.mode
      isActive = req.isActive
  locationTrackingServiceKey <- asks (.locationTrackingServiceKey)
  unless (apiKey == Just locationTrackingServiceKey) $ do
    throwError $ InvalidRequest "Invalid API key"
  let newFlowStatus = getDriverFlowStatus (Just mode) isActive
  updateDriverModeAndFlowStatus driverId req.allowCacheDriverFlowStatus isActive (Just mode) newFlowStatus Nothing
  pure Success

getDriverFlowStatus :: Maybe DriverInfo.DriverMode -> Bool -> DDFS.DriverFlowStatus
getDriverFlowStatus mode isActive =
  case mode of
    Just DriverInfo.ONLINE -> DDFS.ONLINE
    Just DriverInfo.SILENT -> DDFS.SILENT
    Just DriverInfo.OFFLINE -> DDFS.OFFLINE
    Nothing -> if isActive then DDFS.ACTIVE else DDFS.INACTIVE

updateFleetOperatorStatusKeyForDriver ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, HasField "serviceClickhouseCfg" r CH.ClickhouseCfg, HasField "serviceClickhouseEnv" r CH.ClickhouseEnv) =>
  Id DP.Person ->
  DDFS.DriverFlowStatus ->
  Maybe DI.DriverInformation ->
  m ()
updateFleetOperatorStatusKeyForDriver driverId newStatus mbDriverInfo = do
  driverInfo <- case mbDriverInfo of
    Just driverInfoData -> pure driverInfoData
    Nothing -> QDriverInformation.findById driverId >>= fromMaybeM (DriverNotFound driverId.getId)
  let oldStatus = fromMaybe DDFS.INACTIVE driverInfo.driverFlowStatus
  -- Try to find active FleetDriverAssociation
  mbFleetDriverAssociation <- QFleetDriverAssociationExtra.findByDriverId driverId True
  mbEntity <- case mbFleetDriverAssociation of
    Just fda -> pure $ Just (fda.fleetOwnerId, DP.FLEET_OWNER)
    Nothing -> do
      -- If not found, try to find active DriverOperatorAssociation
      mbDriverOperatorAssociation <- QDriverOperatorAssociationExtra.findByDriverId driverId True
      pure $ fmap (\doa -> (doa.operatorId, DP.OPERATOR)) mbDriverOperatorAssociation
  whenJust mbEntity $ \(entityId, entityType) -> do
    decrementFleetOperatorStatusKeyForDriver entityType entityId (Just oldStatus)
    incrementFleetOperatorStatusKeyForDriver entityType entityId (Just newStatus)
    when (entityType == DP.FLEET_OWNER) $ do
      mbFleetOperatorAssociation <- QFleetOperatorAssociation.findByFleetOwnerId entityId True
      whenJust mbFleetOperatorAssociation $ \foa -> do
        decrementFleetOperatorStatusKeyForDriver DP.OPERATOR foa.operatorId (Just oldStatus)
        incrementFleetOperatorStatusKeyForDriver DP.OPERATOR foa.operatorId (Just newStatus)

-- | Increment the fleet/operator status key for a given entityId and status
incrementFleetOperatorStatusKeyForDriver ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  DP.Role ->
  Text ->
  Maybe DDFS.DriverFlowStatus ->
  m ()
incrementFleetOperatorStatusKeyForDriver entityType entityId mbStatus = do
  let status = fromMaybe DDFS.INACTIVE mbStatus
  let key = DDFS.getStatusKey entityId status
  keyExists <- Redis.get @T.Text key
  let msg = "Key does not exist for driver status key: " <> show key <> " for status=" <> show status <> ", entityId=" <> show entityId
  if isNothing keyExists
    then fork msg $ do
      void $ SDF.handleCacheMissForDriverFlowStatus entityType entityId (DDFS.allKeys entityId)
      void $ Redis.incr key
    else do
      logInfo $ "Key already exists for driver status key: " <> show key <> " for status=" <> show status <> ", entityId=" <> show entityId
      void $ Redis.incr key

incrementOperatorStatusKeyForFleetOwner ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Text ->
  Text ->
  m ()
incrementOperatorStatusKeyForFleetOwner operatorId fleetOwnerId = do
  let fleetOwnerKeys = DDFS.allKeys fleetOwnerId
      operatorKeys = DDFS.allKeys operatorId

  fleetRedisCount <- mapM (Redis.get @Int) fleetOwnerKeys
  operatorRedisCount <- mapM (Redis.get @Int) operatorKeys

  let incrementKeysWithCounts counts =
        forM_ (zip operatorKeys counts) $ \(operatorKey, fleetCount) ->
          Redis.incrby operatorKey (fromIntegral fleetCount)

      handleFallback role entityId keyData = do
        void $ SDF.handleCacheMissForDriverFlowStatus role entityId keyData
        mapM (Redis.get @Int) keyData

  case (all isJust fleetRedisCount, all isJust operatorRedisCount) of
    (True, True) -> incrementKeysWithCounts (map (fromMaybe 0) fleetRedisCount)
    (True, False) -> fork "Operator key data not found" $ do
      _ <- handleFallback DP.OPERATOR operatorId operatorKeys
      incrementKeysWithCounts (map (fromMaybe 0) fleetRedisCount)
    (False, True) -> fork "Fleet owner key data not found" $ do
      newFleetRedisCount <- handleFallback DP.FLEET_OWNER fleetOwnerId fleetOwnerKeys
      incrementKeysWithCounts (map (fromMaybe 0) newFleetRedisCount)
    (False, False) -> fork "Both fleet owner and operator key data not found" $ do
      newFleetRedisCount <- handleFallback DP.FLEET_OWNER fleetOwnerId fleetOwnerKeys
      _ <- handleFallback DP.OPERATOR operatorId operatorKeys
      incrementKeysWithCounts (map (fromMaybe 0) newFleetRedisCount)

-- | Decrement the fleet/operator status key for a given entityId and status
decrementFleetOperatorStatusKeyForDriver ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  DP.Role ->
  Text ->
  Maybe DDFS.DriverFlowStatus ->
  m ()
decrementFleetOperatorStatusKeyForDriver entityType entityId mbStatus = do
  let status = fromMaybe DDFS.INACTIVE mbStatus
  let key = DDFS.getStatusKey entityId status
  keyExists <- Redis.get @T.Text key
  let msg = "Key does not exist for driver status key: " <> show key <> " for status=" <> show status <> ", entityId=" <> show entityId
  if isNothing keyExists
    then fork msg $ do
      void $ SDF.handleCacheMissForDriverFlowStatus entityType entityId (DDFS.allKeys entityId)
      void $ Redis.decr key
    else do
      logInfo $ "Key already exists for driver status key: " <> show key <> " for status=" <> show status <> ", entityId=" <> show entityId
      void $ Redis.decr key

decrementOperatorStatusKeyForFleetOwner ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Text ->
  Text ->
  m ()
decrementOperatorStatusKeyForFleetOwner operatorId fleetOwnerId = do
  let fleetOwnerKeys = DDFS.allKeys fleetOwnerId
      operatorKeys = DDFS.allKeys operatorId

  fleetRedisCount <- mapM (Redis.get @Int) fleetOwnerKeys
  operatorRedisCount <- mapM (Redis.get @Int) operatorKeys

  let decrementKeysWithCounts counts =
        forM_ (zip operatorKeys counts) $ \(operatorKey, fleetCount) ->
          Redis.decrby operatorKey (fromIntegral fleetCount)

      handleFallback role entityId keyData = do
        void $ SDF.handleCacheMissForDriverFlowStatus role entityId keyData
        mapM (Redis.get @Int) keyData

  case (all isJust fleetRedisCount, all isJust operatorRedisCount) of
    (True, True) -> decrementKeysWithCounts (map (fromMaybe 0) fleetRedisCount)
    (True, False) -> fork "Operator key data not found" $ do
      _ <- handleFallback DP.OPERATOR operatorId operatorKeys
      decrementKeysWithCounts (map (fromMaybe 0) fleetRedisCount)
    (False, True) -> fork "Fleet owner key data not found" $ do
      newFleetRedisCount <- handleFallback DP.FLEET_OWNER fleetOwnerId fleetOwnerKeys
      decrementKeysWithCounts (map (fromMaybe 0) newFleetRedisCount)
    (False, False) -> fork "Both fleet owner and operator key data not found" $ do
      newFleetRedisCount <- handleFallback DP.FLEET_OWNER fleetOwnerId fleetOwnerKeys
      _ <- handleFallback DP.OPERATOR operatorId operatorKeys
      decrementKeysWithCounts (map (fromMaybe 0) newFleetRedisCount)

-- | Common function to update both the fleet/operator status key and the driver activity in the DB
updateDriverModeAndFlowStatus ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, HasField "serviceClickhouseCfg" r CH.ClickhouseCfg, HasField "serviceClickhouseEnv" r CH.ClickhouseEnv) =>
  Id DP.Person ->
  Maybe Bool ->
  Bool ->
  Maybe DriverInfo.DriverMode ->
  DDFS.DriverFlowStatus ->
  Maybe DI.DriverInformation ->
  m ()
updateDriverModeAndFlowStatus driverId mbAllowCacheDriverFlowStatus isActive mbMode newFlowStatus mbDriverInfo = do
  when (mbAllowCacheDriverFlowStatus == Just True) $
    updateFleetOperatorStatusKeyForDriver driverId newFlowStatus mbDriverInfo
  QDriverInformation.updateActivity isActive mbMode (Just newFlowStatus) driverId
