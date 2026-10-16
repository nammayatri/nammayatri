module Storage.Queries.FleetOperatorStatsExtra where

import qualified Domain.Types.FleetOperatorStats as DFS
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.FleetOperatorStats as Beam
import Storage.Queries.FleetOperatorStats ()

-- Nothing leaves that column as it is. A replica failure on the enabled count is Nothing.
setOperatorLiveCounts ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Text ->
  Maybe Int ->
  Maybe Int ->
  Maybe Int ->
  m Int
setOperatorLiveCounts fleetOperatorId mbAssociated mbActive mbEnabled = do
  now <- getCurrentTime
  let sets =
        catMaybes
          [ Se.Set Beam.totalAssociatedDriver . Just <$> mbAssociated,
            Se.Set Beam.totalActiveDrivers . Just <$> mbActive,
            Se.Set Beam.totalEnabledDrivers . Just <$> mbEnabled
          ]
  if null sets
    then pure 0
    else do
      updateOneWithKV (sets <> [Se.Set Beam.updatedAt now]) [Se.Is Beam.fleetOperatorId $ Se.Eq fleetOperatorId]
      pure $ length sets

findAllByFleetOperatorIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  [Text] ->
  m [DFS.FleetOperatorStats]
findAllByFleetOperatorIds fleetOperatorIds =
  findAllWithKV [Se.Is Beam.fleetOperatorId $ Se.In fleetOperatorIds]

-- Postgres keyset. The id list must not come from KV: fleet_operator_id is the
-- only key, and a role filter cannot be answered from Redis.
findStatsPageAfter ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe Text ->
  Int ->
  m [DFS.FleetOperatorStats]
findStatsPageAfter mbCursor limit =
  findAllWithOptionsDb
    [Se.Is Beam.fleetOperatorId $ Se.GreaterThan (fromMaybe "" mbCursor)]
    (Se.Asc Beam.fleetOperatorId)
    (Just limit)
    Nothing
