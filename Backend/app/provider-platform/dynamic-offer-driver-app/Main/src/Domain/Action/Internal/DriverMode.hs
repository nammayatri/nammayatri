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
import Domain.Action.Internal.ProcessingChangeOnline (processingChangeOnline)
import qualified Domain.Types.Common as DriverInfo
import qualified Domain.Types.DriverFlowStatus as DDFS
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTC
import Environment
import EulerHS.Prelude
import qualified Kernel.Storage.Clickhouse.Config as CH
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverFlowStatus as SDF
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.Queries.DriverInformation as QDriverInformation
import qualified Storage.Queries.DriverOperatorAssociationExtra as QDriverOperatorAssociationExtra
import qualified Storage.Queries.FleetDriverAssociationExtra as QFleetDriverAssociationExtra
import qualified Storage.Queries.FleetOperatorAssociation as QFleetOperatorAssociation
import qualified Storage.Queries.Person as QPerson
import Tools.Error

data DriverModeReq = DriverModeReq
  { driverId :: Id DP.Person,
    mode :: DriverInfo.DriverMode,
    isActive :: Bool
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

  isLocked <- withLockDriverIdForSetActivity driverId
  unless isLocked $ do
    throwError $ DriverActivityUpdateInProgress driverId.getId
  finally
    ( do
        let newFlowStatus = getDriverFlowStatus (Just mode) isActive
        driver <- QPerson.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
        transporterConfig <-
          SCTC.findByMerchantOpCityId driver.merchantOperatingCityId Nothing
            >>= fromMaybeM (TransporterConfigNotFound driver.merchantOperatingCityId.getId)
        oldDriverInfo <- QDriverInformation.findById driverId >>= fromMaybeM (DriverNotFound driverId.getId)
        updateDriverModeAndFlowStatus driverId transporterConfig isActive (Just mode) newFlowStatus oldDriverInfo Nothing Nothing
        pure Success
    )
    ( Redis.unlockRedis (buildSetActivityLockKey driverId)
    )
  where
    withLockDriverIdForSetActivity driverId' = do
      isLockSuccessful <- Redis.tryLockRedis (buildSetActivityLockKey driverId') 5
      return isLockSuccessful

    buildSetActivityLockKey :: Id DP.Person -> Text
    buildSetActivityLockKey driverId' = "Driver:SetActivity:" <> show driverId'

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
  DI.DriverInformation ->
  m ()
updateFleetOperatorStatusKeyForDriver driverId newStatus driverInfo = do
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
      fleetOperatorAssociations <- QFleetOperatorAssociation.findAllByFleetOwnerId (Id entityId) True
      forM_ fleetOperatorAssociations $ \foa -> do
        decrementFleetOperatorStatusKeyForDriver DP.OPERATOR foa.operatorId (Just oldStatus)
        incrementFleetOperatorStatusKeyForDriver DP.OPERATOR foa.operatorId (Just newStatus)

updateAtomicallyFleetOperatorStatusKeyForDriver ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, HasField "serviceClickhouseCfg" r CH.ClickhouseCfg, HasField "serviceClickhouseEnv" r CH.ClickhouseEnv) =>
  (Text -> m Integer) ->
  DP.Role ->
  Text ->
  Maybe DDFS.DriverFlowStatus ->
  m ()
updateAtomicallyFleetOperatorStatusKeyForDriver redisOp entityType entityId mbStatus = do
  let tag = "FleetOperatorStatusUpdate"
      status = fromMaybe DDFS.INACTIVE mbStatus
      key = DDFS.getStatusKey entityId status
      msg = "Key does not exist for driver status key: " <> show key <> " for status=" <> show status <> ", entityId=" <> show entityId

  logTagInfo tag $ "Checking existence of key=" <> key <> ", status=" <> show status <> ", entityId=" <> entityId <> ", entityType=" <> show entityType
  keyExists <- Redis.get @Int key
  logTagInfo tag $ "Checked key existence: key=" <> key <> ", exists=" <> show keyExists
  if isNothing keyExists
    then fork msg $ do
      logTagError tag $ "Key does not exist. Handling cache miss for entityType=" <> show entityType <> ", entityId=" <> entityId <> ", key=" <> key
      void $ SDF.handleCacheMissForDriverFlowStatus entityType entityId (DDFS.allKeys entityId)
      logTagInfo tag $ "Retrying redisOp after cache miss for key=" <> key
      void $ redisOp key
      logTagInfo tag $ "Updated key after cache miss: key=" <> key <> ", entityId=" <> entityId
    else do
      logTagInfo tag $ "Key already exists for driver status key: " <> show key <> " for status=" <> show status <> ", entityId=" <> show entityId
      void $ redisOp key
      logTagInfo tag $ "Updated key: key=" <> key <> ", entityId=" <> entityId

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
incrementFleetOperatorStatusKeyForDriver = updateAtomicallyFleetOperatorStatusKeyForDriver Redis.incr

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
decrementFleetOperatorStatusKeyForDriver = updateAtomicallyFleetOperatorStatusKeyForDriver Redis.decr

updateAtomicallyOperatorStatusKeyForFleetOwner ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  (Text -> Integer -> m Integer) ->
  Text ->
  Text ->
  m ()
updateAtomicallyOperatorStatusKeyForFleetOwner redisOp operatorId fleetOwnerId = do
  let tag = "OperatorStatusUpdate"
      fleetOwnerKeys = DDFS.allKeys fleetOwnerId
      operatorKeys = DDFS.allKeys operatorId

  logTagInfo tag $ "Fetching Redis counts for fleetOwnerId=" <> fleetOwnerId <> ", operatorId=" <> operatorId
  fleetRedisCount <- mapM (Redis.get @Int) fleetOwnerKeys
  operatorRedisCount <- mapM (Redis.get @Int) operatorKeys
  logTagInfo tag $ "Fetched Redis counts: fleetOwnerKeys=" <> show fleetOwnerKeys <> ", fleetRedisCount=" <> show fleetRedisCount <> ", operatorKeys=" <> show operatorKeys <> ", operatorRedisCount=" <> show operatorRedisCount

  let updateKeysWithCounts counts = do
        logTagInfo tag $ "Updating operator keys with counts: " <> show (zip operatorKeys counts)
        forM_ (zip operatorKeys counts) $ \(operatorKey, fleetCount) -> do
          res <- redisOp operatorKey (fromIntegral fleetCount)
          logTagInfo tag $ "Updated operatorKey=" <> operatorKey <> " with count=" <> show fleetCount <> ", result=" <> show res

      handleFallback role entityId keyData = do
        logTagInfo tag $ "Handling cache miss for role=" <> show role <> ", entityId=" <> entityId <> ", keys=" <> show keyData
        void $ SDF.handleCacheMissForDriverFlowStatus role entityId keyData
        counts <- mapM (Redis.get @Int) keyData
        logTagInfo tag $ "Fetched counts after cache miss: " <> show counts
        pure counts

  case (all isJust fleetRedisCount, all isJust operatorRedisCount) of
    (True, True) -> do
      logTagInfo tag $ "All Redis counts present. Proceeding to update keys."
      updateKeysWithCounts (map (fromMaybe 0) fleetRedisCount)
      logTagInfo tag $ "Successfully updated operator keys for operatorId=" <> operatorId <> ", fleetOwnerId=" <> fleetOwnerId
    (True, False) -> fork "Operator key data not found" $ do
      logTagError tag $ "Operator key data not found for operatorId=" <> operatorId <> ". Attempting fallback."
      _ <- handleFallback DP.OPERATOR operatorId operatorKeys
      updateKeysWithCounts (map (fromMaybe 0) fleetRedisCount)
      logTagInfo tag $ "Fallback: Updated operator keys for operatorId=" <> operatorId <> ", fleetOwnerId=" <> fleetOwnerId
    (False, True) -> fork "Fleet owner key data not found" $ do
      logTagError tag $ "Fleet owner key data not found for fleetOwnerId=" <> fleetOwnerId <> ". Attempting fallback."
      newFleetRedisCount <- handleFallback DP.FLEET_OWNER fleetOwnerId fleetOwnerKeys
      updateKeysWithCounts (map (fromMaybe 0) newFleetRedisCount)
      logTagInfo tag $ "Fallback: Updated operator keys for operatorId=" <> operatorId <> ", fleetOwnerId=" <> fleetOwnerId
    (False, False) -> fork "Both fleet owner and operator key data not found" $ do
      logTagError tag $ "Both fleet owner and operator key data not found for operatorId=" <> operatorId <> ", fleetOwnerId=" <> fleetOwnerId <> ". Attempting fallback."
      newFleetRedisCount <- handleFallback DP.FLEET_OWNER fleetOwnerId fleetOwnerKeys
      _ <- handleFallback DP.OPERATOR operatorId operatorKeys
      updateKeysWithCounts (map (fromMaybe 0) newFleetRedisCount)
      logTagInfo tag $ "Fallback: Updated operator keys for operatorId=" <> operatorId <> ", fleetOwnerId=" <> fleetOwnerId

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
incrementOperatorStatusKeyForFleetOwner = updateAtomicallyOperatorStatusKeyForFleetOwner Redis.incrby

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
decrementOperatorStatusKeyForFleetOwner = updateAtomicallyOperatorStatusKeyForFleetOwner Redis.decrby

-- | Common function to update both the fleet/operator status key and the driver activity in the DB
updateDriverModeAndFlowStatus ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, HasField "serviceClickhouseCfg" r CH.ClickhouseCfg, HasField "serviceClickhouseEnv" r CH.ClickhouseEnv) =>
  Id DP.Person ->
  DTC.TransporterConfig ->
  Bool ->
  Maybe DriverInfo.DriverMode ->
  DDFS.DriverFlowStatus ->
  DI.DriverInformation ->
  Maybe Bool ->
  Maybe UTCTime ->
  m ()
updateDriverModeAndFlowStatus driverId transporterConfig isActive mbNewMode newFlowStatus oldDriverInfo mbHasRideStarted lastOfflineTime = do
  let allowCacheDriverFlowStatus = transporterConfig.analyticsConfig.allowCacheDriverFlowStatus
  if allowCacheDriverFlowStatus
    then do
      QDriverInformation.updateActivityWithDriverFlowStatus isActive mbNewMode (Just newFlowStatus) mbHasRideStarted lastOfflineTime driverId
      updateFleetOperatorStatusKeyForDriver driverId newFlowStatus oldDriverInfo
    else QDriverInformation.updateActivityWithDriverFlowStatus isActive mbNewMode Nothing mbHasRideStarted lastOfflineTime driverId
  fork "update driver online duration" $
    processingChangeOnline driverId transporterConfig mbNewMode oldDriverInfo.mode

updateDriverModeAndFlowStatusWithCancellationDeduction ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, HasField "serviceClickhouseCfg" r CH.ClickhouseCfg, HasField "serviceClickhouseEnv" r CH.ClickhouseEnv) =>
  Id DP.Person ->
  DTC.TransporterConfig ->
  Bool ->
  Maybe DriverInfo.DriverMode ->
  DDFS.DriverFlowStatus ->
  DI.DriverInformation ->
  Maybe Bool ->
  Maybe UTCTime ->
  Maybe (Maybe HighPrecMoney) ->
  m ()
updateDriverModeAndFlowStatusWithCancellationDeduction driverId transporterConfig isActive mbNewMode newFlowStatus oldDriverInfo mbHasRideStarted lastOfflineTime mbCancellationDeduction = do
  let allowCacheDriverFlowStatus = transporterConfig.analyticsConfig.allowCacheDriverFlowStatus
  if allowCacheDriverFlowStatus
    then do
      QDriverInformation.updateActivityWithDriverFlowStatusAndCancellationDeduction isActive mbNewMode (Just newFlowStatus) mbHasRideStarted lastOfflineTime mbCancellationDeduction driverId
      updateFleetOperatorStatusKeyForDriver driverId newFlowStatus oldDriverInfo
    else QDriverInformation.updateActivityWithDriverFlowStatusAndCancellationDeduction isActive mbNewMode Nothing mbHasRideStarted lastOfflineTime mbCancellationDeduction driverId
  fork "update driver online duration" $
    processingChangeOnline driverId transporterConfig mbNewMode oldDriverInfo.mode
