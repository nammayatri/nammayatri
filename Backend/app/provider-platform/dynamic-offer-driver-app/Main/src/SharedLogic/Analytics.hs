{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Analytics where

import qualified Domain.Types.Person as DP
import EulerHS.Prelude
import Kernel.Prelude hiding (any, elem, map)
import qualified Kernel.Storage.Clickhouse.Config as CH
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverFlowStatus as SDFStatus
import qualified Storage.Clickhouse.DriverStats as CDS
import qualified Storage.Queries.DriverOperatorAssociation as QDOA
import qualified Storage.Queries.FleetDriverAssociation as QFDA
import qualified Storage.Queries.FleetOperatorAssociation as QFOA
import Tools.Error

-- | Find the operator ID for a driver by checking direct association or fleet association
findOperatorIdForDriver ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DP.Person ->
  m Text
findOperatorIdForDriver driverId = do
  -- First try to find direct driver-operator association
  mbDriverOperatorAssoc <- QDOA.findByDriverId driverId True
  case mbDriverOperatorAssoc of
    Just assoc -> pure assoc.operatorId
    Nothing -> do
      -- If no direct association, try to find through fleet
      mbFleetDriverAssoc <- QFDA.findByDriverId driverId True
      case mbFleetDriverAssoc of
        Just fleetAssoc -> do
          -- Found fleet association, now find the operator for this fleet
          mbFleetOperatorAssoc <- QFOA.findByFleetOwnerIdAndIsActive (Id fleetAssoc.fleetOwnerId) True
          case mbFleetOperatorAssoc of
            Just fleetOpAssoc -> pure fleetOpAssoc.operatorId
            Nothing -> throwError $ InvalidRequest "Driver is associated with fleet but fleet has no operator"
        Nothing -> throwError $ InvalidRequest "Driver is not associated with any entity"

-- | Common function to ensure Redis keys exist by falling back to ClickHouse if needed
ensureRedisKeysExist ::
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
ensureRedisKeysExist operatorId targetKey = do
  mbValue <- Redis.get @Int targetKey

  when (isNothing mbValue) $ do
    -- Need to add progress key here to ensure that we don't run this function again and again
    let totalRideKey = makeOperatorAnalyticsKey operatorId "totalRideCount"
        ratingSumKey = makeOperatorAnalyticsKey operatorId "ratingSum"
        cancelCountKey = makeOperatorAnalyticsKey operatorId "cancelCount"
    (_, _, _) <- fallbackToClickHouseAndUpdateRedisForAllTime operatorId totalRideKey ratingSumKey cancelCountKey
    return ()

-- | Update the operator analytics cancel count for a driver
updateOperatorAnalyticsCancelCount ::
  ( MonadFlow m,
    EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Id DP.Person ->
  m ()
updateOperatorAnalyticsCancelCount driverId = do
  -- Find the operator ID for this driver
  operatorId <- findOperatorIdForDriver driverId
  let cancelCountKey = makeOperatorAnalyticsKey operatorId "cancelCount"
  -- Ensure Redis keys exist
  ensureRedisKeysExist operatorId cancelCountKey

  -- Increment the cancel count
  void $ Redis.incr cancelCountKey

-- | Update the operator analytics rating score for a driver
updateOperatorAnalyticsRatingScoreKey ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Id DP.Person ->
  Int ->
  m ()
updateOperatorAnalyticsRatingScoreKey driverId ratingValue = do
  operatorId <- findOperatorIdForDriver driverId
  let ratingSumKey = makeOperatorAnalyticsKey operatorId "ratingSum"
  -- Ensure Redis keys exist
  ensureRedisKeysExist operatorId ratingSumKey

  -- Increment the rating sum
  void $ Redis.incrby ratingSumKey (fromIntegral ratingValue)

updateOperatorAnalyticsTotalRideCount ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Id DP.Person ->
  m ()
updateOperatorAnalyticsTotalRideCount driverId = do
  operatorId <- findOperatorIdForDriver driverId
  let totalRideCountKey = makeOperatorAnalyticsKey operatorId "totalRideCount"
  -- Ensure Redis keys exist
  ensureRedisKeysExist operatorId totalRideCountKey

  -- Increment the total ride count
  void $ Redis.incr totalRideCountKey

-- | Redis key functions for fleet analytics
makeOperatorKeyPrefix :: Text -> Text
makeOperatorKeyPrefix operatorId = "operator:" <> operatorId <> ":"

makeOperatorAnalyticsKey :: Text -> Text -> Text
makeOperatorAnalyticsKey operatorId metric = makeOperatorKeyPrefix operatorId <> metric

makeOperatorPeriodicKey :: Text -> Text -> Text -> Text
makeOperatorPeriodicKey operatorId metric period = makeOperatorKeyPrefix operatorId <> metric <> ":" <> period

-- | ClickHouse fallback function for fleet analytics
fallbackToClickHouseAndUpdateRedisForAllTime ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Text ->
  Text ->
  Text ->
  Text ->
  m (Int, Int, Int)
fallbackToClickHouseAndUpdateRedisForAllTime operatorId totalRideKey ratingSumKey cancelCountKey = do
  logTagInfo "Fallback to ClickHouse" "true"
  driverIds <- SDFStatus.getFleetDriverIdsAndDriverIdsByOperatorId operatorId
  driverStats <- CDS.sumRatingAndTotalRidesByDriverIds driverIds
  let tr = driverStats.totalRidesSum
      rs = fromMaybe 0 driverStats.totalRatingScoreSum
      cc = driverStats.cancelledCount
  -- update redis (best-effort)
  void $ Redis.set totalRideKey tr
  void $ Redis.set ratingSumKey rs
  void $ Redis.set cancelCountKey cc
  pure (tr, rs, cc)
