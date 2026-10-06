module Storage.Queries.FleetOperatorStatsExtra where

import qualified Domain.Types.FleetOperatorStats as DFS
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.FleetOperatorStats as Beam
import Storage.Queries.FleetOperatorStats ()

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
