module Storage.Queries.FleetOperatorStatsExtra where

import qualified Domain.Types.FleetOperatorStats as DFS
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.FleetOperatorStats as Beam
import Storage.Queries.FleetOperatorStats ()

findAllFleetOperatorStats ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  m [DFS.FleetOperatorStats]
findAllFleetOperatorStats =
  findAllWithKV [Se.Is Beam.fleetOperatorId $ Se.Not $ Se.Eq ""]
